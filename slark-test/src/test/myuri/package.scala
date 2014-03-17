package test

import slark.combinator.parser._
import slark.uri._

package object myuri {

  object MyScheme extends Scheme.AbstractScheme("my", 10086, new Parsers with CharReaders) {
    override def formatPath(path: List[String]): List[String] = path
  }
  
}