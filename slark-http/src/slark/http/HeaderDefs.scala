package slark.http

import parser._

trait HeaderDefs {
  val httpParsers: CombinatorParsers with UriApi with ReaderApi with OctetReader with Literals with ListOfAst with Message
  import httpParsers._

  case class HeaderDef[S](name: String, grammar: Parser[S]) {
    def unapply(headerField: (String, List[Byte])): Option[S] = {
      if (!headerField._1.equalsIgnoreCase(name)) None
      else {
        grammar.parse(headerField._2) match {
          case Succ(r, n) if (n.atEnd) => Some(r)
          case _ => None
        }
      }
    }
  }

  val cache_control = HeaderDef("Cache-Control", 1(token ^ ("=" :^ (token | quoted_string)).?).`#`)

  val expect = HeaderDef("Expect", "100-continue".ignoreCase)

  val host = HeaderDef("Host", uriSymbols.host ^ (":" :^ uriSymbols.port).?)

  val max_forwards = HeaderDef("Max-Forwards", 1(digit).+)
}