package slark

import scala.language.experimental.macros
import scala.language.dynamics

/**
 * @author a554114
 */
object Group extends Dynamic {
  def applyDynamicNamed[A](name: String)(args: (String, A)*): Map[String, A] = macro GroupMacros.applyDynamic[A]
}

object GroupMacros {
  import scala.reflect.macros.blackbox.Context
  
  def applyDynamic[A : c.WeakTypeTag](c: Context)(name: c.Expr[String])(args: c.Expr[(String, A)]*) = {
    import c.universe._
    val Literal(Constant(defName: String)) = name.tree
    defName match {
      case "apply" => Apply(Select(Ident(TermName("Map")), TermName("apply")), args.map(_.tree).toList)
      case _ => c.abort(name.tree.pos, s"value $defName is not a member of Object")
    }
  }
}