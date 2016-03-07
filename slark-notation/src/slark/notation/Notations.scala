package slark
package notation

import combinator.parser._
import scala.language.experimental.macros

/**
 * @author a554114
 */

trait Notations extends ResultsApi {
  type Input = List[scala.reflect.macros.blackbox.Context#Tree]
  
  case class Fail(val msg: List[FailReason]) extends FailApi
  object Fail extends FailExtractor
  
  case class Succ[+S](val result: S, val next: Input) extends SuccApi[S]
  object Succ extends SuccExtractor
  
  trait Notation[S] {
    def apply(input: Input): Result[S]
  }
  
  def grammer(c: scala.reflect.macros.blackbox.Context): Notation[c.Tree]
}

object Notations {
  
  implicit class NotationContext(val sc: StringContext)(implicit val p: ParsersApi, n: Notations) {
    def compile[T](args: p.Parser[Any]*): p.Parser[T] = macro Notations.compile[T]
  }
  
  def compile[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context)(args: c.Tree*): c.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(_, Apply(_, partTrees) :: Nil), pTree :: nTree :: Nil) => {
        val parts = partTrees.map {
          case t@Literal(Constant(s: String)) => t.pos -> s
          case t => c.abort(t.pos, "not literal")
        }
        
        val notationsName = nTree.symbol.typeSignature.typeSymbol.fullName
        loadModule(notationsName) match {
          case Some(notations: Notations) => {
            parts.foreach { part => c.warning(part._1, part._2) }
            c.warning(pTree.pos, pTree.toString())
            c.warning(nTree.pos, notations.toString())
            notations.grammer(c).apply(partTrees) match {
              case notations.Succ(r, n) => r
              case _ => c.abort(c.macroApplication.pos, s"failed to apply notation")
            }
          }
          case _ => c.abort(nTree.pos, s"$notationsName is not an object or missing at compile time")
        }
      }
      case _ => c.abort(c.enclosingPosition, c.prefix.toString())
    }
  }
  
  def loadModule[T](fullName: String): Option[Any] = {
    try {
      import scala.reflect.runtime.universe
      val mirror = universe.runtimeMirror(Notations.getClass.getClassLoader)
      val module = mirror.staticModule(fullName)
      Some(mirror.reflectModule(module).instance)
    } catch {
      case _: Throwable => None
    }
  }
  
  def loadTypedModule[T: scala.reflect.runtime.universe.WeakTypeTag](fullName: String): Option[T] = {
    try {
      import scala.reflect.runtime.universe
      val mirror = universe.runtimeMirror(Notations.getClass.getClassLoader)
      val module = mirror.staticModule(fullName)
      if (module.typeSignature <:< universe.weakTypeOf[T]) Some(mirror.reflectModule(module).instance.asInstanceOf[T])
      else None
    } catch {
      case _: Throwable => None
    }
  }
}