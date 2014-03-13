package slark
package http

import combinator.parser._

trait OctetReader { self: Parsers with ReaderApi =>

  private[this]type Builder[A, B] = A => Parser[B]

  type From = Byte

  type Input = OctetReader

  trait OctetReader extends Reader with ReaderOpt[OctetReader]

  final class StringOctetReader(str: String, index: Int) extends OctetReader {
    override def head = if (atEnd) ??? else {
      val cnt = str.charAt(index)
      if (cnt > 127) throw new IllegalArgumentException(s"unexpected char $cnt, octet 0 - 127 allowed.")
      else cnt.toByte
    }
    override lazy val tail = if (atEnd) ??? else new StringOctetReader(str, index + 1)
    override def atEnd = index >= str.length()
    
    override def toString = "\"" +str.substring(index) + "\""
  }

  implicit val stringOctetReader: String => OctetReader = new StringOctetReader(_, 0)

  final class StringParser(str: String) extends AbstractParser[String] {
    lazy val pattern = stringOctetReader(str)

    override def parse(input: Input) = {
      input.startWith(pattern) match {
        case None => Fail(s"can't match $str")
        case Some(n) => Succ(str, n)
      }
    }

    def ignoreCase: Parser[String] = new AbstractParser[String] {
      override def parse(input: Input) = {
        @tailrec
        def rec(lhs: OctetReader, rhs: OctetReader): Option[Input] = {
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

    override def toString = "\""+str+"\".ignoreCase"
  }

  implicit val stringParser: String => StringParser = new StringParser(_)

  final class letterParser(char: Char) extends AbstractParser[Byte] {
    require(char >= 0 && char <= 127)

    override def parse(input: Input) = if (input.atEnd) Fail("at end of input") else {
      val cnt = input.head
      if (cnt - char == 0) Succ(cnt, input.tail)
      else Fail(s"$char wanted but ${cnt.toChar} found")
    }
  }

  implicit val letterParser: Char => Parser[Byte] = new letterParser(_)

  final class ByteListOctetReader(bytes: List[Byte]) extends OctetReader {
    override def head = if (atEnd) ??? else bytes.head
    override lazy val tail = if (atEnd) ??? else new ByteListOctetReader(bytes.tail)
    override def atEnd = bytes.isEmpty
  }

  implicit val byteListOctetReader: List[Byte] => OctetReader = new ByteListOctetReader(_)
}