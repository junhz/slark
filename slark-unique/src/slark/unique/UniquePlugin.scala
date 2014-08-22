package slark
package unique

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Phase

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
      unit.body.foreach {
        case t if t.hasSymbol && !t.symbol.annotations.isEmpty => {
          unit.warning(t.pos, t.symbol.annotationsString)
        }
        case _ => {}
      }
    }
  }
}