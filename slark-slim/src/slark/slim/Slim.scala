package slark
package slim

import scala.language.experimental.macros

object Slim {

  def apply[T](t: T): T = macro SlimMacros.apply[T]

  def outer[T] = macro SlimMacros.outer[T]

}

object SlimMacros {
  import scala.reflect.macros.Context

  def outer[T: c.WeakTypeTag](c: Context): c.Expr[T] = c.Expr(c.universe.This(c.weakTypeOf[T].typeSymbol))

  def apply[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    def error(msg: String): c.Expr[T] = {
      c.error(c.enclosingPosition, msg)
      reify {
        ???
      }
    }

    def findDeps(root: Tree): List[Type] = {
      import c.universe._
      @tailrec
      def rec(rest: List[Tree], results: List[Type]): List[Type] = {
        if (rest.isEmpty) results
        else {
          rest.head match {
            case EmptyTree => rec(rest.tail, results)
            case _: Import => rec(rest.tail, results)
            case _: Literal => rec(rest.tail, results)
            case _: Ident => rec(rest.tail, results)
            case _: This => rec(rest.tail, results)
            case ModuleDef(_, _, impl) => rec(rest.tail, results)
            case ClassDef(_, _, _, impl) => rec(rest.tail, results)
            case DefDef(_, _, _, _, _, rhs) => rec(rest.tail, results)
            case ValDef(_, _, _, rhs) => rec(rest.tail, results)
            case New(tpt) => rec(rest.tail, results)
            case Block(stats, expr) => rec(stats ::: expr :: rest.tail, results)
            case Apply(fun, args) => rec(fun :: args ::: rest.tail, results)
            case Select(qualifier, name) => qualifier match {
              case q @ This(qual) => rec(rest.tail, q.tpe :: results)
              case _ => rec(qualifier :: rest.tail, results)
            }
            case tree => {
              c.warning(tree.pos, s"${tree.getClass().getName()} not matched: ${tree.toString}")
              rec(rest.tail, results)
            }
          }
        }
      }

      rec(root :: Nil, Nil)
    }

    t.tree match {
      case Function(args, impl) => {
        val deps = findDeps(impl).distinct

        def createVal(count: Int, deps: List[Type], vals: List[ValDef]): List[ValDef] = {
          if (deps.isEmpty) vals.reverse
          else createVal(count + 1, deps.tail, ValDef(Modifiers(), newTermName(s"slim$$$count"), TypeTree(), This(deps.head.typeSymbol)) :: vals)
        }

        val vals = createVal(0, deps, Nil)
c.warning(c.enclosingPosition, deps.map(t => This(t.typeSymbol)).toString)
c.warning(c.enclosingPosition, vals.map(v => Ident(v.name)).toString)
        def substitute(tree: Tree, thiss: List[Type], vals: List[ValDef]): Tree = {
          if (thiss.isEmpty) tree
          else substitute(tree.substituteThis(thiss.head.typeSymbol, Ident(vals.head.name)), thiss.tail, vals.tail)
        }
substitute(impl, deps, vals)
        c Expr Block(vals, Function(args, impl))
        // 
      }
      case _ => error("only Function literal is allowed now. "+showRaw(t))
    }
  }

}