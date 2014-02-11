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
    case ((_, ((Natural0(dd), _MMM), Natural0(yyyy))), (_HH, mm, ss)) => Rfc1123Date(yyyy, _MMM, dd, _HH, mm, ss)
  }
  val rfc850_date = (weekday ^ ", " :^ date2 ^ sp :^ time ^: " GMT") -> {
    case ((_, ((Natural0(dd), _MMM), Natural0(yy))), (_HH, mm, ss)) => {
      val yyyy = y2k(yy, cntYYYY)

      Rfc1123Date(yyyy, _MMM, dd, _HH, mm, ss)
    }
  }
  val asctime_date = (wkday ^ sp :^ date3 ^ sp :^ time ^ sp :^ 4(digit)) -> {
    case (((_, (_MMM, Natural0(dd))), (_HH, mm, ss)), Natural0(yyyy)) => Rfc1123Date(yyyy, _MMM, dd, _HH, mm, ss)
  }

  case class Rfc1123Date(yyyy: Int, _MMM: String, dd: Int, _HH: Int, mm: Int, ss: Int) {
    private[this] val d: slark.DateTime = slark.DateTime.apply(yyyy, slark.DateTime.Month.shortName(_MMM), dd, _HH, mm, ss, 0)
    override def toString: String = f"${d.dayOfWeek.shortName}, ${d.dayOfMonth}%02d ${d.month.shortName} ${d.year}%04d ${d.hour}%02d:${d.minute}%02d:${d.second}%02d GMT"
  }

  val http_date = rfc1123_date | rfc850_date | asctime_date | fail("not a valid http-date")
  val delta_seconds = 1(digit).+

}