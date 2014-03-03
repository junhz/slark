package slark.slim

import scala.language.experimental.macros

object Slim {

  def apply[T](t: T): T = macro SlimMacros.apply[T]

}

object SlimMacros {
  import scala.reflect.macros.Context

  def apply[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    val tpe = weakTypeOf[T]

    def error(msg: String): c.Expr[T] = {
      c.error(c.enclosingPosition, msg)
      reify {
        ???
      }
    }

    t.tree match {
      case Function(args, impl) => {
        c.warning(c.enclosingPosition, showRaw(impl))
        c.Expr(c.parse(
"""final class Slim(i: String) extends scala.runtime.AbstractFunction0[String] {
  override def apply() = i
}
new Slim(i)"""))
      }
      case _ => error("only Function literal is allowed now. " + showRaw(t))
    }
  }

}