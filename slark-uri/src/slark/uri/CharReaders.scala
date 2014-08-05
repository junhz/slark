package slark
package uri

import combinator.parser._

trait CharReaders extends Readers.Linear[Char] { self: Parsers =>

  final class StringCharReader(str: String, index: Int) extends Reader {
    override def head = if (atEnd) ??? else str.charAt(index)
    override lazy val tail = if (atEnd) ??? else new StringCharReader(str, index + 1)
    override def atEnd = index >= str.length()
    override def toString = "\""+str.substring(index)+"\""
  }

  implicit val stringCharReader: String => StringCharReader = new StringCharReader(_, 0)

  final class StringParser(str: String) extends Parser[String] {
    lazy val pattern = stringCharReader(str)

    override def parse(input: Input) = {
      input.startWith(pattern) match {
        case None => Fail(NotStartWith(str, false) :: Nil)
        case Some(n) => Succ(str, n)
      }
    }

    /**
     * case insensitive:
     * pct-encode uppercase
     * scheme lowercase
     * host lowercase (ipfuture 'v' header)
     *
     * The other generic syntax components are assumed to be case-sensitive unless specifically defined otherwise by the scheme
     */
    def ignoreCase: Parser[String] = new Parser[String] {
      override def parse(input: Input) = {
        @tailrec
        def rec(lhs: StringCharReader, rhs: Input): Option[Input] = {
          if (lhs.atEnd) Some(rhs)
          else if (rhs.atEnd) None
          else {
            if (Character.toUpperCase(lhs.head).equals(Character.toUpperCase(rhs.head))) rec(lhs.tail, rhs.tail)
            else None
          }
        }

        rec(pattern, input) match {
          case Some(n) => Succ(str, n)
          case _ => Fail(NotStartWith(str, true) :: Nil)
        }
      }

      override def toString = "\""+str+"\".ignoreCase"
    }

    override def toString = "\""+str+"\""
  }

  implicit val stringParser: String => StringParser = new StringParser(_)

  class CharParser(c: Char) extends Parser[Char] {
    require(c >= 0 && c <= 127)

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt == c) Succ(c, input.tail)
      else Fail(NotMatch(c, cnt) :: Nil)
    }

    override def toString = s"'$c'"
  }

  implicit val charParser: Char => Parser[Char] = new CharParser(_)

  def %(start: Byte, end: Byte): Parser[Char] = new Parser[Char] {
    require(start >= 0 && end > start)

    override def parse(input: Input) = if (input.atEnd) Fail(EOF :: Nil) else {
      val cnt = input.head
      if (cnt >= start && cnt <= end) Succ(cnt, input.tail)
      else Fail(NotInRange(start.toChar, end.toChar, cnt) :: Nil)
    }

    override def toString = f"%%($start%02x, $end%02x)"
  }

  implicit class NumericParserContext(context: StringContext) {
    def hex(digits: Parser[Char]*): Parser[Int] = {
      NumericParserContext.make(context.parts, digits) -> { cs => context.s(cs: _*) match { case Natural0.Hex(i) => i } }
    }
    def dec(digits: Parser[Char]*): Parser[Int] = {
      NumericParserContext.make(context.parts, digits) -> { cs => context.s(cs: _*) match { case Natural0(i) => i } }
    }
  }
  object NumericParserContext {
    def make(parts: Seq[String], args: Seq[Parser[Char]]): Parser[List[Char]] =
      if (args.isEmpty) parts.head -> { _ => Nil }
      else (parts.head :^ args.head) >> { c => make(parts.tail, args.tail) -> { cs => c :: cs } }
  }

  case class NotStartWith(str: String, caseIgnored: Boolean) extends FailReason {
    override def toString = s"input not start with $str${if (caseIgnored) "(case ignored)" else ""}"
  }

  case class NotMatch(expected: Char, found: Char) extends FailReason {
    override def toString = s"$expected expected but $found found"
  }

  case class NotInRange(start: Char, end: Char, found: Char) extends FailReason {
    override def toString = s"$found not in range($start, $end)"
  }

}