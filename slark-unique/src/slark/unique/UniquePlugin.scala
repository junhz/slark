package slark
package unique

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Phase
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.transform.TypingTransformers

//http://lampsvn.epfl.ch/trac/scala/browser/compiler-plugins/uniquerefs/trunk/src/plugin/UniqueComponent.scala

final class UniquePlugin(val global: Global) extends {
  val name = "unique"
  val description = "compiler plugin for unique parameter reference"
  val components = new PatMatOpt(global) /*:: new UniqueTypePluginCompoment(global)*/ :: Nil
} with Plugin

final class PatMatOpt(val global: Global) extends {
  val phaseName = "opt-patmat"
  val runsAfter = "typer" :: Nil
  override val runsBefore = "patmat" :: Nil
} with PluginComponent with TypingTransformers { component =>
  import global._

  final class PatMatTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      val newTree = tree match {
        case Match(selector, CaseDef(Apply(fun1: TypeTree, args1), EmptyTree, Apply(fun2, args2)) :: Nil) => {
          def sameSignature(fun1: Tree, fun2: Tree): Boolean = fun1.tpe =:= fun2.tpe
          def isMethodType(tpe: Type): Boolean = tpe match {
            case MethodType(_, _) => true
            case _ => false
          }
          def isCaseClass(tpe: Type): Boolean = tpe.dealias.typeSymbol.isCaseClass
          def canReturn(mt: MethodType, tpe: Type) = mt.resultType <:< tpe
          def isCaseApply(fun: Tree): Boolean = {
            fun match {
              case TypeApply(Select(qualifier, name), _) => isCaseApplySelect(qualifier, name)
              case Select(qualifier, name) => isCaseApplySelect(qualifier, name)
              case _ => { unit.warning(fun.pos, fun.toString); false }
            }
          }
          def isCaseApplySelect(qualifier: Tree, name: Name): Boolean = {
            qualifier.symbol.isStable && qualifier.tpe.dealias.typeSymbol.companionClass.isCaseClass && (name.toString == "apply" || name.toString == "applySeq")
          }
          @tailrec
          def isClone(binds: List[Tree], params: List[Tree]): Boolean = {
            if (binds.isEmpty && params.isEmpty) true
            else if (!binds.isEmpty && !params.isEmpty) {
              val bind = binds.head
              val param = params.head
              bind match {
                case Bind(sym, Ident(nme.WILDCARD)) => param match {
                  case Ident(name) if sym == name => isClone(binds.tail, params.tail)
                  case _ => false
                }
                case _ => false
              }
            } else false
          }
          if (isCaseClass(selector.tpe) &&
            sameSignature(fun1, fun2) &&
            isMethodType(fun1.tpe) &&
            canReturn(fun1.tpe.asInstanceOf[MethodType], selector.tpe) &&
            isCaseApply(fun2) &&
            isClone(args1, args2)) {
            unit.warning(selector.pos, "optimized")
            selector
          } else {
            unit.warning(tree.pos, tree.toString)
            super.transform(tree)
          }
        }
        case _ => tree
      }

      super.transform(tree)
    }

    override def transformUnit(unit: CompilationUnit) {
      val tree = try transform(unit.body.duplicate)
      catch {
        case ex: Exception =>
          unit.warning(unit.body.pos, FailReason.causedBy(ex).mkString("\r\nCaused By: "))
          unit.body
      }

      if (tree ne unit.body) {

        def escape(t: Tree): Boolean = {
          t match {
            case _ => false
          }
        }
        
        this.localTyper.context.retyping = true
        unit.warning(unit.body.pos, s"pt = ${typeOf[(String, String)] <:< WildcardType}")
        val f = global.analyzer.getClass().getDeclaredField("scala$tools$nsc$typechecker$AnalyzerPlugins$$analyzerPlugins")
        f.setAccessible(true)
        unit.warning(unit.body.pos, s"ap = ${f.get(global.analyzer)}")
        unit.body = this.localTyper.typed(tree)
      }
    }
  }

  def newPhase(prev: Phase): Phase = {
    new StdPhase(prev) {
      def apply(unit: CompilationUnit): Unit = {
        new PatMatTransformer(unit).transformUnit(unit)
      }
    }
  }

}

final class UniqueTypePluginCompoment(val global: Global) extends {
  val phaseName = "unique-type"
  val runsAfter = "opt-patmat" :: "refchecks" :: Nil
} with PluginComponent { component =>
  import global._

  def newPhase(prev: Phase): Phase = {
    new UniquePhase(prev)
  }

  final class UniquePhase(prev: Phase) extends StdPhase(prev) {

    def apply(unit: CompilationUnit): Unit = {
      def annotatedAsUnique(tpe: Type): Boolean = {
        tpe.dealias.annotations.exists(a => a.atp =:= typeOf[Unique.unique]) || tpe.dealias.typeSymbol.annotations.exists(a => a.atp =:= typeOf[Unique.unique])
      }
      def findUnique(tpeTree: TypeTree): Unit = {
        def mark(tpe: Type): Unit = {
          unit.warning(tpeTree.pos, tpe.toString + " is annotated with unique")
        }
        @tailrec
        def rec(tpes: List[List[Type]], found: List[Type]): List[Type] = {
          if (tpes.isEmpty) found
          else if (tpes.head.isEmpty) rec(tpes.tail, found)
          else {
            val tpe = tpes.head.head
            val params = tpe.typeArgs
            if (annotatedAsUnique(tpe)) {
              rec(params :: tpes.head.tail :: tpes.tail, tpe :: found)
            } else {
              rec(params :: tpes.head.tail :: tpes.tail, found)
            }
          }
        }

        rec((tpeTree.tpe :: Nil) :: Nil, Nil) foreach mark _
      }

      def uniqueParams(vparamss: List[List[ValDef]]): List[List[Symbol]] = {
        @tailrec
        def rec(vparamss: List[List[ValDef]], syms: List[List[Symbol]]): List[List[Symbol]] = {
          if (vparamss.isEmpty) syms
          else if (vparamss.head.isEmpty) rec(vparamss.tail, syms)
          else {
            val vparam = vparamss.head.head
            if (annotatedAsUnique(vparam.tpt.tpe)) {
              rec(vparamss.head.tail :: vparamss.tail, (vparam.symbol :: Nil) :: syms)
            } else {
              rec(vparamss.head.tail :: vparamss.tail, syms)
            }
          }
        }
        rec(vparamss, Nil)
      }

      def compatible(lhs: Type, rhs: Type): Boolean = {
        lhs <:< rhs
      }

      def multiApply(app: Apply): (Tree, List[List[Tree]]) = {
        @tailrec
        def rec(fun: Tree, sym: Symbol, paramss: List[List[Tree]]): (Tree, List[List[Tree]]) = {
          fun match {
            case Apply(fun, args) if fun.symbol == sym => rec(fun, sym, args :: paramss)
            case _ => (fun, paramss)
          }
        }

        rec(app.fun, app.fun.symbol, app.args :: Nil)
      }

      @tailrec
      def rec(trees: List[List[Tree]], uniqueSyms: List[List[List[Symbol]]]): Unit = {
        if (trees.isEmpty) {}
        else if (trees.head.isEmpty) rec(trees.tail, uniqueSyms.tail)
        else {
          val tree = trees.head.head
          tree match {
            case pd @ PackageDef(pid, stats) => {
              unit.warning(pd.pos, s"enter package $pid")
              rec(stats :: Nil, Nil :: Nil)
            }
            case md @ ModuleDef(_, name, impl) => {
              unit.warning(md.pos, s"enter class $name")
              rec(impl.body :: trees.head.tail :: trees.tail, Nil :: uniqueSyms)
            }
            case cd @ ClassDef(_, name, _, impl) => {
              unit.warning(cd.pos, s"enter class $name")
              rec(impl.body :: trees.head.tail :: trees.tail, Nil :: uniqueSyms)
            }
            case dd @ DefDef(_, name, _, vparamss, tpt: TypeTree, rhs) => {
              unit.warning(dd.pos, s"enter method $name")
              if (!dd.symbol.isConstructor && !compatible(rhs.tpe, tpt.tpe)) {
                unit.warning(dd.pos, "not compatible")
                rec(trees.head.tail :: trees.tail, uniqueSyms)
              } else {
                val uniqueParameters = uniqueParams(vparamss)
                unit.warning(dd.pos, uniqueParameters.toString)
                rec((rhs :: Nil) :: trees.head.tail :: trees.tail, uniqueParameters :: uniqueSyms)
              }
            }
            case b @ Block(stats, expr) => {
              unit.warning(b.pos, "enter block")
              rec((stats ::: expr :: Nil) :: trees.head.tail :: trees.tail, uniqueSyms.head :: uniqueSyms)
            }
            case app @ Apply(_, _) => {
              val (fun, paramss) = multiApply(app)
              @tailrec
              def compareAll(symss: List[List[Symbol]], paramss: List[List[Tree]]): Unit = {
                if (paramss.isEmpty) {}
                else if (paramss.head.isEmpty) {
                  if (symss.head.isEmpty) compareAll(symss.tail, paramss.tail)
                  else unit.warning(app.pos, "wrong parameters")
                } else if (symss.head.isEmpty) unit.warning(app.pos, "wrong parameter")
                else {
                  val sym = symss.head.head
                  val param = paramss.head.head
                  if (!compatible(sym.tpe, param.tpe)) {
                    unit.warning(param.pos, "not compatible")
                  }
                  compareAll(symss.head.tail :: symss.tail, paramss.head.tail :: paramss.tail)
                }
              }
              compareAll(fun.symbol.paramss, paramss)
              rec((fun :: paramss.flatten) :: trees.head.tail :: trees.tail, uniqueSyms.head :: uniqueSyms)
            }
            case Select(_, _) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case Literal(_) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case Ident(_) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case vd @ ValDef(_, name, tpt, rhs) => {
              unit.warning(vd.pos, s"enter val $name")
              if (!compatible(rhs.tpe, tpt.tpe)) {
                unit.warning(vd.pos, "not compatible")
                rec(trees.head.tail :: trees.tail, uniqueSyms)
              } else {
                rec((rhs :: Nil) :: trees.head.tail :: trees.tail, uniqueSyms.head :: uniqueSyms)
              }
            }
            case TypeApply(_, _) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case Typed(expr, tpt) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case Import(_, _) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case td @ TypeDef(_, _, _, _) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case t => { unit.warning(t.pos, t.getClass().toString()) }
          }
        }
      }

      rec((unit.body :: Nil) :: Nil, Nil :: Nil)
      unit.warning(unit.body.pos, unit.body.toString)
      /*unit.body.foreach {
        case dd @ DefDef(_, _, _, vparamss, tpt: TypeTree, rhs) => {
          vparamss.foreach {
            case ps => {
              ps.foreach {
                case vd @ ValDef(_, _, tpt: TypeTree, _) => findUnique(tpt)
                case t => {}
              }
            }
          }
          findUnique(tpt)
        }
        case _ => {}
      }*/
    }
  }
}