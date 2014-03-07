package slark

import scala.language.experimental.macros

object Cache {

  def apply[T](cached: Any*)(t: T): T = macro CacheMacros.apply[T]

}

object CacheMacros {
  import scala.reflect.macros.Context

  def apply[T: c.WeakTypeTag](c: Context)(cached: c.Expr[Any]*)(t: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    val trees = cached.map(_.tree).toList
    val syms = trees.map(_.symbol)
    
    def createVal(count: Int, caches: List[Tree], vals: List[ValDef]): List[ValDef] = {
      if (caches.isEmpty) vals.reverse
      else {
        c.warning(caches.head.pos, caches.head.symbol.fullName)
        createVal(count + 1, caches.tail, ValDef(Modifiers(), newTermName(s"slim$$$count"), TypeTree(), caches.head) :: vals)
      }
    }

    def substituter(vals: List[ValDef], syms: List[Symbol]): List[Symbol] = {
      if (vals.isEmpty) syms.reverse
      else substituter(vals.tail, NoSymbol.newTermSymbol(vals.head.name.toTermName) :: syms)
    }
    val vals = createVal(0, trees, Nil)
    c Expr Block(vals, t.tree.substituteSymbols(syms, substituter(vals, Nil)))
  }

}