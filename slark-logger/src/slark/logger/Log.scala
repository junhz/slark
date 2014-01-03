package slark
package logger

import scala.language.experimental.macros

/**
 * export log ability
 */
trait Log {
  protected[this] def newLogger = Log.factory.lift.apply(this).getOrElse(Logger.silent)

  //TODO: improve performance of record.message
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
      //TODO: wrap this logic and provide a trait of config location for testing
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

    def find(root: c.universe.Tree, target: c.universe.Tree): List[c.universe.Tree] = {
      import c.universe._
      @tailrec
      def rec(path: List[Tree], bodies: List[List[Tree]]): List[Tree] = {
        if (bodies.isEmpty) List()
        else if (bodies.head.isEmpty) rec(path.tail, bodies.tail)
        else if (bodies.head.head == target) (target :: path).reverse
        else {
          bodies.head.head match {
            case EmptyTree => rec(path, bodies.head.tail :: bodies.tail)
            case _: Import => rec(path, bodies.head.tail :: bodies.tail)
            case _: Super => rec(path, bodies.head.tail :: bodies.tail)
            case _: Literal => rec(path, bodies.head.tail :: bodies.tail)
            case _: Ident => rec(path, bodies.head.tail :: bodies.tail)
            case _: TypeBoundsTree => rec(path, bodies.head.tail :: bodies.tail)
            case _: TypeDef => rec(path, bodies.head.tail :: bodies.tail)
            case _: This => rec(path, bodies.head.tail :: bodies.tail)
            case t @ ModuleDef(_, _, impl) => rec(t :: path, impl.body :: bodies.head.tail :: bodies.tail)
            case t @ ClassDef(_, _, _, impl) => rec(t :: path, impl.body :: bodies.head.tail :: bodies.tail)
            case t @ PackageDef(_, stats) => rec(t :: path, stats :: bodies.head.tail :: bodies.tail)
            case t @ DefDef(_, _, _, _, _, rhs) => rec(t :: path, (rhs :: Nil) :: bodies.head.tail :: bodies.tail)
            case t @ ValDef(_, _, _, rhs) => rec(t :: path, (rhs :: Nil) :: bodies.head.tail :: bodies.tail)
            case t @ New(tpt) => rec(t :: path, (tpt :: Nil) :: bodies.head.tail :: bodies.tail)
            case t @ Apply(fun, args) => rec(t :: path, (fun :: args) :: bodies.head.tail :: bodies.tail)
            case t @ Block(stats, expr) => rec(t :: path, (expr :: stats) :: bodies.head.tail :: bodies.tail)
            case t @ Select(qualifier, _) => rec(t :: path, (qualifier :: Nil) :: bodies.head.tail :: bodies.tail)
            case t @ Template(_, _, body) => rec(t :: path, body :: bodies.head.tail :: bodies.tail)
            case tree => {
              c.warning(tree.pos, s"${tree.getClass().getName()} not matched: ${tree.toString}")
              rec(tree :: path, tree.children :: bodies.head.tail :: bodies.tail)
            }
          }
        }
      }

      rec(Nil, (root :: Nil) :: Nil)
    }

    val source = collect(find(c.enclosingUnit.body, c.enclosingMethod match {
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