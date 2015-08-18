package slark

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Source[+T](val srcTree: String, private[this] val ret: => T) {
  def output: Either[T, List[FailReason]] = try Left(ret) catch { case e: Throwable => Right(FailReason.causedBy(new Exception(e)).tail) }
}

object Source {

  def apply[T](source: T): Source[T] = macro withSource[T]

  def withSource[T: c.WeakTypeTag](c: Context)(source: c.Expr[T]): c.Expr[Source[T]] = {
    import c.universe._
    //c.universe.reify(new Source(c.literal(c.universe.show(source.tree)).splice, source.splice))
    val srcTree = q"${c.universe.show(source.tree)}"
    val r = q"new Source($srcTree, ${source.tree})"
    c.Expr[Source[T]](r)
  }

}