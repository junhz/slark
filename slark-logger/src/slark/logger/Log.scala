package slark
package logger

import scala.language.experimental.macros

/**
 * export log ability
 */
trait Log {
  protected[this] def newLogger = Log.factory.lift.apply(this).getOrElse(Logger.silent)

  /**
   * create Logger.Record instance with message built by standard string interpolation
   */
  implicit class Context(val context: StringContext) {
    def info(args: Any*): Logger.Record = macro LogMacros.log
    def warn(args: Any*): Logger.Record = macro LogMacros.log
    def error(args: Any*): Logger.Record = macro LogMacros.log
  }
}

object Log {
  type Factory = PartialFunction[Log, Logger]
  lazy val factory: Factory = {
    try {
      import scala.reflect.runtime.universe
      val runtime = universe.runtimeMirror(getClass.getClassLoader)
      val configs = runtime.staticModule("configs.package")
      val log = configs.typeSignature.declaration(universe.newTermName("log")).asTerm
      runtime.reflect(runtime.reflectModule(configs).instance).reflectField(log).get.asInstanceOf[Factory]
    } catch {
      case e: Throwable => {
        e.printStackTrace()
        ({ case _: Log => Logger.silent })
      }
    }
  }
}

object LogMacros {
  import scala.reflect.macros.Context
  def log(c: Context)(args: c.Expr[Any]*): c.Expr[Logger.Record] = {
    import c.universe._

    def collect(path: List[Tree]): String = {
      @tailrec
      def rec(path: List[Tree], names: List[String]): List[String] = {
        if (path.isEmpty) names
        else path.head match {
          case PackageDef(ref, _) => rec(path.tail, ref.name.decoded :: names)
          case ModuleDef(_, moduleName, _) => rec(path.tail, moduleName.decoded :: names)
          case ClassDef(_, className, _, _) => rec(path.tail, className.decoded :: names)
          case DefDef(_, methodName, _, _, _, _) => (methodName.decoded :: names)
          case ValDef(_, valName, _, _) => (valName.decoded :: names)
          case _ => rec(path.tail, names)
        }
      }
      rec(path, Nil).reverse.mkString(".")
    }

    val source = collect(Macros.searchForDef(c)(c.enclosingUnit.body, c.enclosingMethod match {
      case null => c.enclosingClass // bug?
      case EmptyTree => c.enclosingClass
      case t => t
    }))

    val file = c.enclosingUnit.source.file.name

    val line = c.enclosingPosition.line

    val level = c.macroApplication match {
      case Apply(Select(_, func), _) => func.decoded
    }

    /**
     * new A(...)
     */
    def construct[T: TypeTag](args: Tree*): Tree = {
      Apply(Select(New(TypeTree(typeOf[T])), nme.CONSTRUCTOR), args.toList)
    }

    /**
     * trait A
     * object A {
     * 	  def apply(...): A = ???
     * }
     */
    def build[T: WeakTypeTag](args: Tree*): Tree = {
      Apply(Select(Ident(weakTypeOf[T].typeSymbol.companionSymbol), newTermName("apply")), args.toList)
    }

    c.Expr(construct[Logger.Record](
      Literal(Constant(s"$source($file:$line)")),
      Select(Ident(typeOf[Level].typeSymbol.companionSymbol), newTermName(level)),
      Select(c.prefix.tree, newTermName("context")),
      build[List[Any]](args.map(_.tree): _*)))
  }
}