package slark
package uri

import combinator.parser._

trait Path { self: Symbols[Parsers with ReaderApi with CharReader] with Literals =>

  import parsers._

  // ../a = a since top folder's top is itself
  /*val removeDot = new Extractor[List[String], List[String]] {
    override def unapply(segs: List[String]) = {
      @tailrec
      def rec(outSegs: List[String], inSegs: List[String]): List[String] = {
        if (inSegs.isEmpty) outSegs.reverse
        else if (inSegs.head == ".") rec(outSegs, inSegs.tail)
        else if (inSegs.head == "..") {
          if (outSegs.isEmpty) rec(Nil, inSegs.tail)
          else rec(outSegs.tail, inSegs.tail)
        } else {
          log(info"seg: ${inSegs.head}")
          rec(inSegs.head :: outSegs, inSegs.tail)
        }
      }

      Some(rec(Nil, segs))
    }
  }*/

  val segment = pchar.* -> (_.mkString)

  val segment_nz = (pchar)(1, `>`) -> (_.mkString)

  val segment_nz_nc = (":".! :^ pchar)(1, `>`) -> (_.mkString)

  val path_empty = succ(List[String]())

  val path_abempty = ("/" :^ segment).*

  val path_absolute = "/" :^ (segment_nz ^ ("/" :^ segment).*).? -> {
    case None => List[String]()
    case Some((p, ps)) => p :: ps
  }

  val path_rootless = (segment_nz ^ ("/" :^ segment).*) -> {
    case (p, ps) => p :: ps
  }

  val path_noscheme = (segment_nz_nc ^ ("/" :^ segment).*) -> {
    case (p, ps) => p :: ps
  }
}