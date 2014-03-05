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
    case class StableDep(qual: TypeName, name: Name) extends OuterDep {
      val e = c.universe.TypeRef
    }
    case class UnstableDep(name: TypeName) extends OuterDep

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
            case _: Ident => rec(rest.tail, results)
            case _: This => rec(rest.tail, results)
            case ModuleDef(_, _, impl) => rec(rest.tail, results)
            case ClassDef(_, _, _, impl) => rec(rest.tail, results)
            case DefDef(_, _, _, _, _, rhs) => rec(rest.tail, results)
            case ValDef(_, _, _, rhs) => rec(rest.tail, results)
            case New(tpt) => rec(rest.tail, results)
            case Block(stats, expr) => rec(stats ::: expr :: rest.tail, results)
            case Apply(fun, args) => rec(fun :: args ::: rest.tail, results)
            case Select(qualifier, name) => {
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

    def outers(path: List[Tree]): List[Name] = {
      @tailrec
      def rec(path: List[Tree], names: List[Name], stable: Boolean): List[Name] = {
        if (path.isEmpty) names
        else path.head match {
          case ClassDef(_, className, _, _) => rec(path.tail, className :: names, false)
          case ModuleDef(_, moduleName, _) => if(stable) rec(path.tail, Nil, true) else rec(path.tail, moduleName :: names, false)
          case _: PackageDef => rec(path.tail, Nil, true)
          case _ => rec(path.tail, names, false)
        }
      }
      rec(path, Nil, true)
    }

    t.tree match {
      case Function(args, impl) => {
        val ts = args map { case ValDef(_, _, tpe, _) => tpe }
        val r = TypeTree(impl.tpe)
        c.warning(c.enclosingPosition, findDeps(impl).mkString(", "))
        c.warning(c.enclosingPosition, outers(Macros.searchForDef(c)(c.enclosingUnit.body, c.enclosingClass)).mkString(", "))
        
        val functionType = AppliedTypeTree(Select(Select(Ident(newTermName("scala")), newTermName("runtime")), newTypeName(s"AbstractFunction${ts.length}")), ts ::: r :: Nil)
        val classDef = ClassDef(
          Modifiers(Flag.FINAL),
          newTypeName("Slim"),
          Nil,
          Template(
            functionType :: Nil,
            emptyValDef,
            DefDef(Modifiers(Flag.OVERRIDE), newTermName("apply"), List(), args :: Nil, TypeTree(), c.resetAllAttrs(impl)) ::
              DefDef(Modifiers(), nme.CONSTRUCTOR, Nil, Nil, TypeTree(), Block(List(Apply(Select(Super(This(newTypeName("Slim")), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))) ::
              Nil))
        c Expr Block(
          classDef :: Nil,
          Apply(Select(New(Ident(newTypeName("Slim"))), nme.CONSTRUCTOR), Nil))
      }
      case _ => error("only Function literal is allowed now. "+showRaw(t))
    }
  }

}