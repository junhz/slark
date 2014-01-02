package test

import slark.parser._
import slark.uri._

package object myuri {

  object MyScheme extends Scheme.AbstractScheme("my", 10086, new CombinatorParsers with ReaderApi with CharReader with Formats) {
    override def formatPath(path: List[String]): List[String] = path
  }
  
}