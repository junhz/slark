package slark
package http

import combinator.parser._

trait OctetReaders extends Readers[Byte] { self: Parsers =>

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

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt - char == 0) Succ(cnt, input.tail)
      else Fail(s"$char wanted but ${cnt.toChar} found")
    }
  }

  implicit val letterParser: Char => Parser[Byte] = new letterParser(_)

  final class ByteListOctetReader(bytes: List[Byte]) extends Reader {
    override def head = if (atEnd) ??? else bytes.head
    override lazy val tail = if (atEnd) ??? else new ByteListOctetReader(bytes.tail)
    override def atEnd = bytes.isEmpty
  }

  implicit val byteListOctetReader: List[Byte] => Reader = new ByteListOctetReader(_)

  def letter(startChar: Char, endChar: Char): Parser[Byte] = new AbstractParser[Byte] {
    require(startChar >= 0 && endChar > startChar && endChar <= 127)

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt >= startChar && cnt <= endChar) Succ(cnt, input.tail)
      else Fail(s"out of range of ($startChar, $endChar)")
    }
  }

  def acsii(startByte: Byte, endByte: Byte): Parser[Byte] = new AbstractParser[Byte] {
    require(startByte >= 0 && endByte > startByte && endByte <= 127)

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt >= startByte && cnt <= endByte) Succ(cnt, input.tail)
      else Fail(s"acsii character out of range (octet $startByte - $endByte)")
    }
  }

  def acsii(byte: Byte): Parser[Byte] = new AbstractParser[Byte] {
    require(byte >= 0 && byte <= 127)

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt == byte) Succ(cnt, input.tail)
      else Fail(s"acsii character (octet $byte ) wanted but $cnt found)")
    }
  }

  def %(start: Int, end: Int): Parser[Byte] = new AbstractParser[Byte] {
    require((start & (1 << 31)) == 0 && end > start && ((end & 0xffffff00) == 0))

    override def parse(input: Input) = if (input.atEnd) eof else {
      val cnt = input.head
      if (cnt >= start && (cnt & 0xff) <= end) Succ(cnt, input.tail)
      else Fail(f"$toString wanted but %%x$cnt%02X found")
    }

    override val toString = f"%%x$start%02X-$end%02X"
  }

  val octet = new AbstractParser[Byte] {
    override def parse(input: Input) = if (input.atEnd) eof else Succ(input.head, input.tail)
  }
}