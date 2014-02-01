package slark

import scala.language.experimental.macros
import scala.reflect.macros.Context

class Source[+T](val srcTree: String, private[this] val ret: => T) {
  def output: Either[T, List[Thrown[_]]] = try Left(ret) catch { case e: Throwable => Right(Thrown.wrap(new Exception(e)).tail) }
}

object Source {

  def apply[T](source: T): Source[T] = macro withSource[T]

  def withSource[T: c.WeakTypeTag](c: Context)(source: c.Expr[T]): c.Expr[Source[T]] = {
    c.universe.reify(new Source(c.literal(c.universe.show(source.tree)).splice, source.splice))
  }

}