package test

import slark.parser._
import slark.http._

package object myhttp {

  object MyHttp extends Message.AbstractMessage(new CombinatorParsers with ReaderApi with OctetReader) with DateTime { self =>

    import parsers._

    override def trans1(input: Input) = {
      if (input.atEnd) None
      else {
        val cnt = input.head
        if (cnt < 0) None
        else Some(cnt.toChar, input.tail)
      }

    }
  }

}