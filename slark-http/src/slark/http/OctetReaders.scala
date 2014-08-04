package slark
package http

import combinator.parser._

trait OctetReaders extends Readers.Linear[Byte] { self: Parsers =>

  final class StringOctetReader(str: String, index: Int) extends Reader {
    override def head = if (atEnd) ??? else {
      val cnt = str.charAt(index)
      if (cnt > 127) throw new IllegalArgumentException(s"unexpected char $cnt, octet 0 - 127 allowed.")
      else cnt.toByte
    }
    override lazy val tail = if (atEnd) ??? else new StringOctetReader(str, index + 1)
    override def atEnd = index >= str.length()

    override def toString = "\""+str.substring(index)+"\""
  }

  implicit val stringOctetReader: String => Reader = new StringOctetReader(_, 0)

  final class StringParser(str: String) extends Parser[String] {
    lazy val pattern = stringOctetReader(str)

    override def parse(input: Input) = {
      input.startWith(pattern) match {
        case None => Fail(NotStartWith(str, false) :: Nil)
        case Some(n) => Succ(str, n)
      }
    }

    def ignoreCase: Parser[String] = new Parser[String] {
      override def parse(input: Input) = {
        @tailrec
        def rec(lhs: Reader, rhs: Reader): Option[Input] = {
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

    override def toString = "\""+str+"\".ignoreCase"
  }

  implicit val stringParser: String => StringParser = new StringParser(_)

  final class letterParser(char: Char) extends Parser[Byte] {
    require(char >= 0 && char <= 127)

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt - char == 0) Succ(cnt, input.tail)
      else Fail(NotMatch(char, cnt) :: Nil)
    }
  }

  implicit val letterParser: Char => Parser[Byte] = new letterParser(_)

  final class ByteListOctetReader(bytes: List[Byte]) extends Reader {
    override def head = if (atEnd) ??? else bytes.head
    override lazy val tail = if (atEnd) ??? else new ByteListOctetReader(bytes.tail)
    override def atEnd = bytes.isEmpty
  }

  implicit val byteListOctetReader: List[Byte] => Reader = new ByteListOctetReader(_)

  def letter(startChar: Char, endChar: Char): Parser[Byte] = new Parser[Byte] {
    require(startChar >= 0 && endChar > startChar && endChar <= 127)

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt >= startChar && cnt <= endChar) Succ(cnt, input.tail)
      else Fail(NotInRange(startChar, endChar, cnt) :: Nil)
    }
  }

  def acsii(byte: Byte): Parser[Byte] = new Parser[Byte] {
    require(byte >= 0 && byte <= 127)

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt == byte) Succ(cnt, input.tail)
      else Fail(NotMatch(byte, cnt) :: Nil)
    }
  }

  def %(start: Int, end: Int): Parser[Byte] = new Parser[Byte] {
    require((start & (1 << 31)) == 0 && end > start && ((end & 0xffffff00) == 0))

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt >= start && (cnt & 0xff) <= end) Succ(cnt, input.tail)
      else Fail(NotInRange(start, end, cnt) :: Nil)
    }

    override val toString = f"%%x$start%02X-$end%02X"
  }
  
  case class NotStartWith(str: String, caseIgnored: Boolean) extends FailReason {
    override def toString = s"input not start with $str${if (caseIgnored) "(case ignored)" else "" }"
  }
  
  case class NotMatch(expected: Int, found: Byte) extends FailReason {
    override def toString = f"%%$expected%02x expected but %%$found%02x found"
  }
  
  case class NotInRange(start: Int, end: Int, found: Byte) extends FailReason {
    override def toString = f"%%($start%02x, $end%02x) wanted but %%$found%02x found"
  }
}