package slark
package slim

import scala.language.experimental.macros

object Slim {

  def apply[T](t: T): T = macro SlimMacros.apply[T]

}

object SlimMacros {
  import scala.reflect.macros.Context

  def apply[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[T] = {
    import c.universe._

    trait OuterDep
    /** value */
    case class StableDep(qual: TypeName, name: Name) extends OuterDep {
      val e = c.universe.TypeRef
    }
    /** method */
    case class UnstableDep(name: TypeName) extends OuterDep

    val tpe = weakTypeOf[T]

    def error(msg: String): c.Expr[T] = {
      c.error(c.enclosingPosition, msg)
      reify {
        ???
      }
    }

    def findDeps(root: Tree): List[OuterDep] = {
      import c.universe._
      @tailrec
      def rec(rest: List[Tree], results: List[OuterDep]): List[OuterDep] = {
        if (rest.isEmpty) results
        else {
          rest.head match {
            case EmptyTree => rec(rest.tail, results)
            case _: Import => rec(rest.tail, results)
            case _: Literal => rec(rest.tail, results)
            case _: This => rec(rest.tail, results)
            case t @ ModuleDef(_, _, impl) => rec(rest.tail, results)
            case t @ ClassDef(_, _, _, impl) => rec(rest.tail, results)
            case t @ DefDef(_, _, _, _, _, rhs) => rec(rest.tail, results)
            case t @ ValDef(_, _, _, rhs) => rec(rest.tail, results)
            case t @ New(tpt) => rec(rest.tail, results)
            case t @ Block(stats, expr) => rec(stats ::: expr :: rest.tail, results)
            case t @ Apply(fun, args) => rec(fun :: args ::: rest.tail, results) /*{
              c.warning(t.pos, showRaw(t))
              fun match {
                case Select(This(qual), name) => rec(args ::: rest.tail, UnstableDep(qual) :: results)
                case _ => rec(fun :: args ::: rest.tail, results)
              }
            }*/
            case t @ Select(qualifier, name) => {
              qualifier match {
                case q @ This(qual) => {
                  if (q.tpe.member(name).asTerm.isStable) rec(rest.tail, StableDep(qual, name) :: results)
                  else rec(rest.tail, UnstableDep(qual) :: results)
                }
                case _ => rec(qualifier :: rest.tail, results)
              }
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

    def find(root: Tree, target: Tree): List[Tree] = {
      import c.universe._
      @tailrec
      def rec(path: List[Tree], bodies: List[List[Tree]]): List[Tree] = {
        if (bodies.isEmpty) List()
        else if (bodies.head.isEmpty) rec(path.tail, bodies.tail)
        else if (bodies.head.head == target) target :: path
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
            case t @ Function(_, impl) => rec(t :: path, (impl :: Nil) :: bodies.head.tail :: bodies.tail)
            case tree => {
              c.warning(tree.pos, s"${tree.getClass().getName()} not matched: ${tree.toString}")
              rec(tree :: path, tree.children :: bodies.head.tail :: bodies.tail)
            }
          }
        }
      }

      rec(Nil, (root :: Nil) :: Nil)
    }

    def outers(path: List[Tree]): List[TypeName] = {
      @tailrec
      def rec(path: List[Tree], names: List[TypeName]): List[TypeName] = {
        if (path.isEmpty) names
        else path.head match {
          case ClassDef(_, className, _, _) => rec(path.tail, className :: names)
          case _ => rec(path.tail, names)
        }
      }
      rec(path, Nil)
    }

    t.tree match {
      case Function(args, impl) => {
        val typeArgs = tpe match {
          case TypeRef(_, _, args) => args
        }
        c.warning(c.enclosingPosition, findDeps(impl).mkString(", "))
        c.warning(c.enclosingPosition, outers(find(c.enclosingUnit.body, c.enclosingClass)).mkString(", "))
        c.Expr(c.parse(
          s"""final class Slim(i: String) extends scala.runtime.AbstractFunction${args.length}[${typeArgs.mkString(",")}] {
  override def apply(${args.map(_ match { case ValDef(_, name, tpe, _) => s"$name: $tpe" }).mkString(",")}) = i
}
new Slim(i)"""))
      }
      case _ => error("only Function literal is allowed now. "+showRaw(t))
    }
  }

}