package slark
package notation

import combinator.parser._

/**
 * @author a554114
 */
object BnfNotations extends Notations {
  val grammer = new Notation {
    def apply(c: scala.reflect.macros.blackbox.Context)(params: List[c.Tree]): (c.Tree, List[c.Tree]) = {
      (c.universe.reify(???).tree, Nil)
    }
  }
}