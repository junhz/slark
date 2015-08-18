package slark

import scala.language.experimental.macros

object Outer {

  def apply[T]: T = macro OuterMacros.apply[T]

}

object OuterMacros {
  import scala.reflect.macros.blackbox.Context

  def apply[T: c.WeakTypeTag](c: Context): c.Expr[T] = c.Expr(c.universe.This(c.weakTypeOf[T].typeSymbol))
}