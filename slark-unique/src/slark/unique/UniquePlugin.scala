package slark
package unique

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Phase

//http://lampsvn.epfl.ch/trac/scala/browser/compiler-plugins/uniquerefs/trunk/src/plugin/UniqueComponent.scala

final class UniquePlugin(val global: Global) extends {
  val name = "unique"
  val description = "compiler plugin for unique parameter reference"
  val components = new UniqueTypePluginCompoment(global) :: Nil
} with Plugin

final class UniqueTypePluginCompoment(val global: Global) extends {
  val phaseName = "unique-type"
  val runsAfter = "typer" :: Nil
} with PluginComponent { component => 
  import global._
  
  def newPhase(prev: Phase): Phase = {
    new UniquePhase(prev)
  }
  
  final class UniquePhase(prev: Phase) extends StdPhase(prev) {
    
    def apply(unit: CompilationUnit): Unit = {
      def findUnique(tpeTree: TypeTree): Unit = {
        def annotatedAsUnique(tpe: Type): Boolean = {
          tpe.dealias.annotations.exists(a => a.atp =:= typeOf[Unique.unique]) || tpe.dealias.typeSymbol.annotations.exists(a => a.atp =:= typeOf[Unique.unique])
        }
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
      
      unit.warning(unit.body.pos, unit.body.toString)
      unit.body.foreach {
        case dd@DefDef(_, _, _, vparamss, tpt: TypeTree, rhs) => {
          vparamss.foreach {
            case ps => {
              ps.foreach {
                case vd@ValDef(_, _, tpt: TypeTree, _) => findUnique(tpt)
                case t => {}
              }
            }
          }
          findUnique(tpt)
        }
        case _ => {}
      }
    }
  }
}

final class UniqueRefPluginComponent(val global: Global) extends {
  val phaseName = "unique-param-ref"
  val runsAfter = "jvm" :: Nil
} with PluginComponent { component => 
  import global._
  
  def newPhase(prev: Phase): Phase = {
    new UniquePhase(prev)
  }
  
  final class UniquePhase(prev: Phase) extends StdPhase(prev) {
    def apply(unit: CompilationUnit): Unit = {
      unit.warning(unit.body.pos, unit.body.toString)
      unit.body.foreach {
        case DefDef(_, _, _, vparamss, _, rhs) => {
          vparamss.foreach {
            case ps => {
              ps.foreach {
                case p if p.hasSymbol && !p.symbol.annotations.isEmpty => unit.warning(p.pos, p.symbol.annotationsString)
                case _ => { }
              }
            }
          }
          
          rhs.foreach {
            case ValDef(_, _, _, t) if (t.hasSymbol && !t.symbol.annotations.isEmpty) => {
              unit.warning(t.pos, "assign")
            }
            case Apply(fun, ts) => {
              ts.foreach {
                case t if t.hasSymbol && !t.symbol.annotations.isEmpty => unit.warning(t.pos, "apply")
                case _ => {}
              }
            }
            case Select(p, _) if p.hasSymbol && !p.symbol.annotations.isEmpty => unit.warning(p.pos, "call")
            case _ => {}
          }
          
          rhs.foreach {
            case s@Select(p, q) => {
              s.symbol.paramss.foreach {
                ps => ps.foreach {
                  case p if !p.annotations.isEmpty => unit.warning(s.pos, p.annotationsString)
                  case _ => {}
                }
              }
            }
            case _ => {}
          }
          
          unit.warning(rhs.pos, rhs.toString)
        }
        case _ => {}
      }
    }
  }
}