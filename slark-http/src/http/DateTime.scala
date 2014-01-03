package slark
package http

import parser._
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Calendar

trait DateTime { self: Symbols[Parsers with CombinatorApi with CombinatorAst with ReaderApi with OctetReader] with Literals =>

  import parsers._

  def monthOrd(_MMM: String): Int = _MMM match {
    case "Jan" => 1
    case "Feb" => 2
    case "Mar" => 3
    case "Apr" => 4
    case "May" => 5
    case "Jun" => 6
    case "Jul" => 7
    case "Aug" => 8
    case "Sep" => 9
    case "Oct" => 10
    case "Nov" => 11
    case "Dec" => 12
    case _ => throw new IllegalArgumentException("month abbr wanted")
  }

  def currentYY = Integer.valueOf(new SimpleDateFormat("yy").format(new Date()))
  def currentYYYY = Integer.valueOf(new SimpleDateFormat("yyyy").format(new Date()))

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
      val yyyy = if (yy - currentYY > 50) currentYYYY - currentYY - 100 + yy
      else currentYYYY - currentYY + yy

      GmtDateTime(yyyy, _MMM, dd, _HH, mm, ss)
    }
  }
  val asctime_date = (wkday ^ sp :^ date3 ^ sp :^ time ^ sp :^ 4(digit)) -> {
    case (((wkday, (_MMM, Natural0(dd))), (_HH, mm, ss)), Natural0(yyyy)) => GmtDateTime(yyyy, _MMM, dd, _HH, mm, ss)
  }

  case class GmtDateTime(yyyy: Int, _MMM: String, dd: Int, HH: Int, mm: Int, ss: Int) {
    val date = {
      val c = Calendar.getInstance()
      c.set(yyyy, monthOrd(_MMM), dd, HH, mm, ss)
      c.getTime()
    }
    val wkday: String = new SimpleDateFormat("EE").format(date)
    def asRfc1123Date = s"$wkday, $dd ${_MMM} $yyyy $HH:$mm:$ss GMT"
  }

  val http_date = rfc1123_date | rfc850_date | asctime_date | fail("not a valid http-date")
  val delta_seconds = 1(digit).+

}