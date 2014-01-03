package slark
package uri

import parser._

// TODO: extract trait of logic String to parser and case sensitive
trait CharReader { self: Parsers with ReaderApi =>

  private[this]type Builder[A, B] = A => Parser[B]

  type From = Char

  type Input = CharReader

  trait CharReader extends Reader with ReaderOpt[CharReader] {}

  final class StringCharReader(str: String, index: Int) extends CharReader {
    override def head = if (atEnd) ??? else str.charAt(index)
    override lazy val tail = if (atEnd) ??? else new StringCharReader(str, index + 1)
    override def atEnd = index >= str.length()
    override def toString = "\""+str.substring(index)+"\""
  }

  implicit val stringCharReader: String => CharReader = new StringCharReader(_, 0)

  final class StringParser(str: String) extends Parser[String] {
    lazy val pattern = stringCharReader(str)

    override def parse(input: Input) = {
      input.startWith(pattern) match {
        case None => Fail(s"can't match $str")
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
        def rec(lhs: CharReader, rhs: CharReader): Option[Input] = {
          if (lhs.atEnd) Some(rhs)
          else if (rhs.atEnd) None
          else {
            if (Character.toUpperCase(lhs.head).equals(Character.toUpperCase(rhs.head))) rec(lhs.tail, rhs.tail)
            else None
          }
        }

        rec(pattern, input) match {
          case Some(n) => Succ(str, n)
          case _ => Fail(s"can't match $str with case ignored")
        }
      }

      override def toString = "\""+str+"\".ignoreCase"
    }

    override def toString = "\""+str+"\""
  }

  implicit val stringParser: String => StringParser = new StringParser(_)

  class CharParser(c: Char) extends Parser[Char] {
    require(c >= 0 && c <= 127)

    override def parse(input: Input) = if (input.atEnd) Fail("at end of input") else {
      val cnt = input.head
      if (cnt == c) Succ(c, input.tail)
      else Fail(s"$c expected but $cnt found")
    }

    override def toString = s"'$c'"
  }

  implicit val charParser: Char => Parser[Char] = new CharParser(_)

}