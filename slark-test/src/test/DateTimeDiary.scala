package test

import slark.DateTime
import java.text.SimpleDateFormat
import java.util.TimeZone
import java.util.Date

object DateTimeDiary extends Diary {

  def time(src: String): Long = {
    val f = new SimpleDateFormat("yyyy.MM.dd")
    f.setTimeZone(TimeZone.getTimeZone("GMT"))
    f.parse(src).getTime()
  }
  
  def str(time: Long): String = {
    val f = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
    f.setTimeZone(TimeZone.getTimeZone("GMT"))
    f.format(new Date(time))
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
    Source(DateTime.since1970(time("2000.02.29"))) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jan, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jan, 1, 0, 0, 0, 1).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jan, 1, 0, 0, 1, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jan, 1, 0, 1, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jan, 1, 1, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jan, 31, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Feb, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Feb, 28, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Mar, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Mar, 31, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Apr, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Apr, 30, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.May, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.May, 31, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jun, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jun, 30, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jul, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Jul, 31, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Aug, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Aug, 31, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Sep, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Sep, 30, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Oct, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Oct, 31, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Nov, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Nov, 30, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Dec, 1, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(1970, DateTime.Month.Dec, 31, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(2000, DateTime.Month.Feb, 29, 0, 0, 0, 0).timeMillis)) :: 
    Source(str(DateTime.apply(2100, DateTime.Month.Feb, 29, 0, 0, 0, 0).timeMillis)) :: Nil
}