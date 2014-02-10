package test

import slark.DateTime
import java.text.SimpleDateFormat
import java.util.TimeZone

object DateTimeDiary extends Diary {

  def time(src: String): Long = {
    val f = new SimpleDateFormat("yyyy.MM.dd")
    f.setTimeZone(TimeZone.getTimeZone("GMT"))
    f.parse(src).getTime()
  }
  
  val content = 
    Source(DateTime.since1970(0L)) :: 
    Source(DateTime.since1970(1L)) :: 
    Source(DateTime.since1970(1000L)) :: 
    Source(DateTime.since1970(1001L)) :: 
    Source(DateTime.since1970(60000L)) :: 
    Source(DateTime.since1970(60001L)) :: 
    Source(DateTime.since1970(3600000L)) :: 
    Source(DateTime.since1970(3600001L)) :: 
    Source(DateTime.since1970(time("1970.01.01"))) :: 
    Source(DateTime.since1970(time("1970.01.01") + 1L)) :: 
    Source(DateTime.since1970(time("1970.01.31"))) :: 
    Source(DateTime.since1970(time("1970.02.01"))) :: 
    Source(DateTime.since1970(time("1970.02.28"))) :: 
    Source(DateTime.since1970(time("1970.03.01"))) :: 
    Source(DateTime.since1970(time("1970.03.31"))) :: 
    Source(DateTime.since1970(time("1970.04.01"))) :: 
    Source(DateTime.since1970(time("1970.04.30"))) :: 
    Source(DateTime.since1970(time("1970.05.01"))) :: 
    Source(DateTime.since1970(time("1970.05.31"))) :: 
    Source(DateTime.since1970(time("1970.06.01"))) :: 
    Source(DateTime.since1970(time("1970.06.30"))) :: 
    Source(DateTime.since1970(time("1970.07.01"))) :: 
    Source(DateTime.since1970(time("1970.07.31"))) :: 
    Source(DateTime.since1970(time("1970.08.01"))) :: 
    Source(DateTime.since1970(time("1970.08.31"))) :: 
    Source(DateTime.since1970(time("1970.09.01"))) :: 
    Source(DateTime.since1970(time("1970.09.30"))) :: 
    Source(DateTime.since1970(time("1970.10.01"))) :: 
    Source(DateTime.since1970(time("1970.10.31"))) :: 
    Source(DateTime.since1970(time("1970.11.01"))) :: 
    Source(DateTime.since1970(time("1970.11.30"))) :: 
    Source(DateTime.since1970(time("1970.12.01"))) :: 
    Source(DateTime.since1970(time("1970.12.31"))) :: 
    Source(DateTime.since1970(time("1980.02.29"))) :: 
    Source(DateTime.since1970(time("2100.02.28"))) :: 
    Source(DateTime.since1970(time("2000.02.29"))) :: Nil
}