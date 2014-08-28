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
  val phaseName = "optimized pattern match"
  val runsAfter = "parser" :: Nil
} with PluginComponent with TypingTransformers with Transform { component =>
  import global._
  
  def newTransformer(unit: CompilationUnit): Transformer = new PatMatTransformer(unit)
  
  final class PatMatTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    /*unit.warning(unit.body.pos, unit.body.toString)
    override def transformValDef(tree: ValDef): ValDef = {
      tree.rhs match {
        case t => {
          unit.warning(tree.pos, tree.toString)
          super.transformValDef(tree)
        }
      }
    }*/
    override def transform(tree: Tree): Tree = {
      tree match {
        case Match(selector, CaseDef(Apply(fun: TypeTree, args1), EmptyTree, Apply(fun2, args2)) :: Nil) => {
          fun.tpe match {
            case MethodType(params, resultType) => {
              for (param <- params) {
                unit.warning(tree.pos, param.toString)
              }
              unit.warning(tree.pos, resultType.dealias.typeSymbol.isCaseClass.toString)
            }
          }
          for (arg <- args1) {
            unit.warning(tree.pos, arg.getClass().toString() + ": " + arg.toString)
          }
          unit.warning(tree.pos, fun2.getClass().toString() + ": " + fun2.toString)
          for (arg <- args2) {
            unit.warning(tree.pos, arg.getClass().toString() + ": " + arg.toString)
          }
        }
        case _ => 
      }
      super.transform(tree)
    }
  }
  
}

final class UniqueTypePluginCompoment(val global: Global) extends {
  val phaseName = "unique-type"
  val runsAfter = "refchecks" :: Nil
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
          unit.warning(tpeTree.pos, tpe.toString+" is annotated with unique")
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
            case pd@PackageDef(pid, stats) => {
              unit.warning(pd.pos, s"enter package $pid")
              rec(stats :: Nil, Nil :: Nil)
            }
            case md@ModuleDef(_, name, impl) => {
              unit.warning(md.pos, s"enter class $name")
              rec(impl.body :: trees.head.tail :: trees.tail, Nil :: uniqueSyms)
            }
            case cd@ClassDef(_, name, _, impl) => {
              unit.warning(cd.pos, s"enter class $name")
              rec(impl.body :: trees.head.tail :: trees.tail, Nil :: uniqueSyms)
            }
            case dd@DefDef(_, name, _, vparamss, tpt: TypeTree, rhs) => {
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
            case b@Block(stats, expr) => {
              unit.warning(b.pos, "enter block")
              rec((stats ::: expr :: Nil) :: trees.head.tail :: trees.tail, uniqueSyms.head :: uniqueSyms)
            }
            case app@Apply(_, _) => {
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
              rec((fun :: paramss.flatten)  :: trees.head.tail :: trees.tail, uniqueSyms.head :: uniqueSyms)
            }
            case Select(_, _) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case Literal(_) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case Ident(_) => rec(trees.head.tail :: trees.tail, uniqueSyms)
            case vd@ValDef(_, name, tpt, rhs) => {
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
            case td@TypeDef(_, _, _, _) => rec(trees.head.tail :: trees.tail, uniqueSyms)
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