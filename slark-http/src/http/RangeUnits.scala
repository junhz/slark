package junhz
package http

import parser._

trait RangeUnits { self: CombinatorParsers with ReaderApi with OctetReader with Literals =>

  trait RangeUnit

  object RangeUnit {
    def apply(token: List[Byte]): Option[RangeUnit] = ???
  }

}