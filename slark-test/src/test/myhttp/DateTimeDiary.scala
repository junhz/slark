package test
package myhttp

import symbols._
import parsers._

object DateTimeDiary extends Diary {

  val content = Source(date1 parse "06 Nov 1994") :: 
  Source(time parse "08:49:37") :: 
  Source(rfc1123_date parse "Sun, 06 Nov 1994 08:49:37 GMT") :: 
  Source(http_date parse "Sun, 06 Nov 1994 08:49:37 GMT") :: 
  Source(http_date parse "Sunday, 06-Nov-94 08:49:37 GMT") :: 
  Source(http_date parse "Sun Nov  6 08:49:37 1994") :: Nil

}