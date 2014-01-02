package test

import slark.parser._

package object myparser {

  object parsers extends Parsers with ReaderApi with CombinatorApi with CombinatorAst {
    type From = Char

    class StringReader(str: String, index: Int) extends Reader with ReaderOpt[StringReader] {
      override def atEnd = index >= str.length()
      override def head = if (atEnd) ??? else str.charAt(index)
      override lazy val tail = if (atEnd) ??? else new StringReader(str, index + 1)
      override def toString = "\"" + str.substring(index) + "\""
    }

    type Input = StringReader

    def stringReader(str: String): Input = new StringReader(str, 0)

    class SParser(str: String) extends Parser[String] {
      override def parse(input: Input) = {
        input.startWith(stringReader(str)) match {
          case None => Fail(s"can't match $str")
          case Some(n) => Succ(str, n)
        }
      }

      override def toString = "\""+str+"\""
    }

    implicit val sParserBuilder = new SParser(_)
  }
}