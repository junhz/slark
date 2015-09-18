package slark.script

import java.util.NoSuchElementException

abstract class Script {
  
  def apply(args: String*): (=>Stream[String]) => Stream[String]

}

object Script {
  implicit final class Combinator(val self: (=>Stream[String]) => Stream[String]) extends AnyVal {
    def | (that: (=>Stream[String]) => Stream[String]) = self.andThen { that.apply(_) }
  }
}