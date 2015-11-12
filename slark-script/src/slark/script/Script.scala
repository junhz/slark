package slark.script

abstract class Script {
  
  def apply(args: String*): (=>Seq[String]) => Seq[String]

}

object Script {
  implicit val intCanPrint: Seq[Int] => Seq[String] = _ map Integer.toString
  implicit final class Combinator(val self: (=>Seq[String]) => Seq[String]) extends AnyVal {
    def | (that: (=>Seq[String]) => Seq[String]) = self.andThen { that.apply(_) }
  }
}