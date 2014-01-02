package test
package mylogger

import java.util.Date
import slark.logger._

object FormatterDiary extends Diary {
  
  val content = Source {
    val trace = new Exception().getStackTrace()(0)
    Formatter.default.format(new Date(0L), Level.error, trace.getClassName()+"."+trace.getMethodName(), "omg")
  } :: Nil
  
}