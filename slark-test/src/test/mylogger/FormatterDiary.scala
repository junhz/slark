package test
package mylogger

import slark.logger._
import slark.DateTime

object FormatterDiary extends Diary {
  
  val content = Source {
    val trace = new Exception().getStackTrace()(0)
    Formatter.default.format(DateTime.since1970(0L), Level.error, trace.getClassName()+"."+trace.getMethodName(), "omg")
  } :: Nil
  
}