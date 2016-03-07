package slark
package notation

import combinator.parser._

/**
 * @author a554114
 */
object BnfNotations extends Notations {
  def grammer(c: scala.reflect.macros.blackbox.Context) = new Notation[c.Tree] {
    def apply(params: Input): Result[c.Tree] = {
      Succ(c.universe.reify(???).tree, Nil)
    }
  }
}