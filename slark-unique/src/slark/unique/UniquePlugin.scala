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
  val components = new UniquePluginComponent(global) :: Nil
} with Plugin

final class UniquePluginComponent(val global: Global) extends {
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
                case _ => {}
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