package test

import slark.combinator.parser._
import slark.uri._

package object myuri {

  val symbols = new UriSymbols[Parsers with CharReaders] {
    protected[this] override def _parsers = new Parsers with CharReaders
    protected[this] override def _name = "my"
    override def _port = 10086
    protected[this] override def formatPath(path: List[String]): List[String] = path
  }
  
}