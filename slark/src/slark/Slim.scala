package slark

import scala.language.experimental.macros

object Slim {

  def apply[T](t: T): T = macro SlimMacros.apply[T]

}

object SlimMacros {
  import scala.reflect.macros.Context

  def apply[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    def findDeps(root: Tree): List[Symbol] = {
      import c.universe._
      @tailrec
      def rec(rest: List[Tree], results: List[Symbol]): List[Symbol] = {
        if (rest.isEmpty) results
        else {
          rest.head match {
            case _: Ident => rec(rest.tail, results)
            case _: Literal => rec(rest.tail, results)
            case Apply(fun, args) => rec(fun :: args ::: rest.tail, results)
            case TypeApply(fun, _) => rec(fun :: rest.tail, results)
            case Select(qualifier, name) => qualifier match {
              case q @ This(qual) => {
                c.echo(q.pos, s"dep on ${q.symbol}")
                rec(rest.tail, q.symbol :: results)
              }
              case _ => rec(qualifier :: rest.tail, results)
            }
            case tree => {
              c.error(tree.pos, s"${tree.getClass().getName()} not allowed")
              Nil
            }
          }
        }
      }

      rec(root :: Nil, Nil)
    }

    t.tree match {
      case Function(args, impl) => {
        val deps = findDeps(impl).distinct

        def createVal(count: Int, deps: List[Symbol], vals: List[ValDef]): List[ValDef] = {
          if (deps.isEmpty) vals.reverse
          else createVal(count + 1, deps.tail, ValDef(Modifiers(), newTermName(s"slim$$$count"), TypeTree(), This(deps.head)) :: vals)
        }

        val vals = createVal(0, deps, Nil)
        def substitute(tree: Tree, thiss: List[Symbol], vals: List[ValDef]): Tree = {
          if (thiss.isEmpty) tree
          else substitute(tree.substituteThis(thiss.head, TypeApply(Select(Ident(vals.head.name), newTermName("asInstanceOf")), SingletonTypeTree(This(thiss.head)) :: Nil)), thiss.tail, vals.tail)
        }

        c Expr Block(vals, Function(args, substitute(c.resetLocalAttrs(impl), deps, vals)))
      }
      case _ => {
        c.error(c.enclosingPosition, "only Function literal is allowed now.")
        reify { ??? }
      }
    }
  }

}