package slark

import scala.language.experimental.macros

object Cache {

  def apply[T](cached: Any*)(t: T): T = macro CacheMacros.apply[T]

}

object CacheMacros {
  import scala.reflect.macros.Context

  def apply[T: c.WeakTypeTag](c: Context)(cached: c.Expr[Any]*)(t: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    def createVals(count: Int, cached: Seq[c.Expr[Any]], vals: List[ValDef], fromAndTo: List[(Symbol, TermName)]): (List[ValDef], List[(Symbol, TermName)]) = {
      if (cached.isEmpty) (vals.reverse, fromAndTo)
      else {
        val v = ValDef(Modifiers(), newTermName(c.fresh("cache$")), SingletonTypeTree(cached.head.tree), cached.head.tree)
        val s = cached.head.tree.symbol
        val tn = v.name
        createVals(count + 1, cached.tail, v :: vals, (s, tn) :: fromAndTo)
      }
    }

    val (vals, fromAndTo) = createVals(0, cached, Nil, Nil)

    class CacheTransformer extends Transformer {
      override def transform(tree: Tree) = {
        val found = fromAndTo.find(_._1 == tree.symbol)
        found match {
          case Some(x) => Ident(x._2)
          case _ => super.transform(tree)
        }
      }
    }

    val ret = new CacheTransformer().transform(c.resetLocalAttrs(t.tree))
    c Expr c.resetLocalAttrs(Block(vals, ret)) // somewhere not cleaned
  }

}