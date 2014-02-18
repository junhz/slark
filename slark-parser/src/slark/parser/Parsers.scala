package slark
package parser

trait Parsers {

  type Input

  sealed trait ParseResult[+S]

  trait Parser[+S] {
    type Result = ParseResult[S]
    def parse(input: Input): Result
  }

  type Builder[T, S] = T => Parser[S]

  case class Fail(msg: String) extends ParseResult[Nothing]

  case class Succ[+S](result: S, next: Input) extends ParseResult[S]

  /**
   * A | fail("failed")
   */
  final def fail(msg: String): Parser[Nothing] = new Parser[Nothing] {
    override def parse(input: Input) = Fail(msg)
    override def toString = s"fail($msg)"
  }

  /**
   * A | B | default(symbol)
   */
  final def succ[T](sym: T): Parser[T] = new Parser[T] {
    override def parse(input: Input) = Succ(sym, input)
    override def toString = s"succ($sym)"
  }

  def p[T, S](self: T)(implicit fn: Builder[T, S]): Parser[S] = fn(self)

}