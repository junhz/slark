package slark
package http

import parser._
import java.util.Calendar
import java.util.TimeZone

trait DateTime { self: Symbols[Parsers with CombinatorApi with CombinatorAst with ReaderApi with OctetReader] with Literals =>

  import parsers._

  def y2k(yy: Int, cntYYYY: Int): Int = {
    val cntYY = cntYYYY % 100
    val cntCentury = cntYYYY / 100
    if (yy - cntYY > 50) (cntCentury - 1) * 100 + yy
    else cntCentury * 100 + yy
  }
  
  def cntYYYY = slark.DateTime.since1970(System.currentTimeMillis()).year

  val wkday = ("Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun")
  val weekday = "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday" | "Saturday" | "Sunday"
  val month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
  val time = (2(digit) ^ ":" :^ 2(digit) ^ ":" :^ 2(digit)) -> {
    case ((Natural0(_HH), Natural0(mm)), Natural0(ss)) => (_HH, mm, ss)
  }

  val date1 = 2(digit) ^ sp :^ month ^ sp :^ 4(digit)
  val date2 = 2(digit) ^ "-" :^ month ^ "-" :^ 2(digit)
  val date3 = month ^ sp :^ (2(digit) | (sp :^ 1(digit)))
  val rfc1123_date = (wkday ^ ", " :^ date1 ^ sp :^ time ^: " GMT") -> {
    case ((wkday, ((Natural0(dd), _MMM), Natural0(yyyy))), (_HH, mm, ss)) => GmtDateTime(yyyy, _MMM, dd, _HH, mm, ss)
  }
  val rfc850_date = (weekday ^ ", " :^ date2 ^ sp :^ time ^: " GMT") -> {
    case ((weekday, ((Natural0(dd), _MMM), Natural0(yy))), (_HH, mm, ss)) => {
      val yyyy = y2k(yy, cntYYYY)

      GmtDateTime(yyyy, _MMM, dd, _HH, mm, ss)
    }
  }
  val asctime_date = (wkday ^ sp :^ date3 ^ sp :^ time ^ sp :^ 4(digit)) -> {
    case (((wkday, (_MMM, Natural0(dd))), (_HH, mm, ss)), Natural0(yyyy)) => GmtDateTime(yyyy, _MMM, dd, _HH, mm, ss)
  }

  case class GmtDateTime(yyyy: Int, _MMM: String, dd: Int, HH: Int, mm: Int, ss: Int) {
    
    def asRfc1123Date = s"$wkday, $dd ${_MMM} $yyyy $HH:$mm:$ss GMT"
  }

  val http_date = rfc1123_date | rfc850_date | asctime_date | fail("not a valid http-date")
  val delta_seconds = 1(digit).+

}