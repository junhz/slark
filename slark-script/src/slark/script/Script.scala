package slark.script

import java.util.NoSuchElementException
import scala.collection.immutable.LinearSeq

abstract class Script {
  
  def apply(args: String*): (=>Seq[String]) => Seq[String]

}

object Script {
  implicit final class Combinator(val self: (=>Seq[String]) => Seq[String]) extends AnyVal {
    def | (that: (=>Seq[String]) => Seq[String]) = self.andThen { that.apply(_) }
  }
}