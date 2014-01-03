package slark
package http

import parser._
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Calendar

trait DateTime { self: Symbols[Parsers with CombinatorApi with CombinatorAst with ReaderApi with OctetReader] with Literals =>

  import parsers._

  object Numbers {
    def unapply(src: List[Byte]): Option[Int] = {
      @tailrec
      def rec(rest: List[Byte], result: Int): Option[Int] = {
        if (rest.isEmpty) Some(result)
        else {
          val cnt = rest.head - '0'
          if (cnt < 0 || cnt > 9) None
          else rec(rest.tail, result * 10 + cnt)
        }
      }

      rec(src, 0)
    }
  }

  object Month {
    def apply(month: Int): String = month match {
      case 1 => "Jan"
      case 2 => "Feb"
      case 3 => "Mar"
      case 4 => "Apr"
      case 5 => "May"
      case 6 => "Jun"
      case 7 => "Jul"
      case 8 => "Aug"
      case 9 => "Sep"
      case 10 => "Oct"
      case 11 => "Nov"
      case 12 => "Dec"
      case _ => throw new IllegalArgumentException("number 1 - 12 wanted")
    }

    def unapply(month: String): Option[Int] = month match {
      case "Jan" => Some(1)
      case "Feb" => Some(2)
      case "Mar" => Some(3)
      case "Apr" => Some(4)
      case "May" => Some(5)
      case "Jun" => Some(6)
      case "Jul" => Some(7)
      case "Aug" => Some(8)
      case "Sep" => Some(9)
      case "Oct" => Some(10)
      case "Nov" => Some(11)
      case "Dec" => Some(12)
      case _ => None
    }
  }

  def currentYY = Integer.valueOf(new SimpleDateFormat("yy").format(new Date()))
  def currentYYYY = Integer.valueOf(new SimpleDateFormat("yyyy").format(new Date()))

  val wkday = ("Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun")
  val weekday = "Monday" | "Tuesday" | "Wednesday" | "Thursday" | "Friday" | "Saturday" | "Sunday"
  val month = "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"
  val time = 2(digit) ^ ":" :^ 2(digit) ^ ":" :^ 2(digit)

  object Time {
    def unapply(time: ((List[Byte], List[Byte]), List[Byte])): Option[(Int, Int, Int)] = time match {
      case ((Numbers(_HH), Numbers(mm)), Numbers(ss)) => Some(_HH, mm, ss)
      case _ => None
    }
  }

  val date1 = 2(digit) ^ sp :^ month ^ sp :^ 4(digit)
  val date2 = 2(digit) ^ "-" :^ month ^ "-" :^ 2(digit)
  val date3 = month ^ sp :^ (2(digit) | (sp :^ 1(digit)))
  val rfc1123_date = (wkday ^ ", " :^ date1 ^ sp :^ time ^: " GMT") -> {
    case ((wkday, ((Numbers(dd), Month(_MM)), Numbers(yyyy))), Time(_HH, mm, ss)) => GmtDateTime(yyyy, _MM, dd, _HH, mm, ss)
  }
  val rfc850_date = (weekday ^ ", " :^ date2 ^ sp :^ time ^: " GMT") -> {
    case ((weekday, ((Numbers(dd), Month(_MM)), Numbers(yy))), Time(_HH, mm, ss)) => {
      val yyyy = if (yy - currentYY > 50) currentYYYY - currentYY - 100 + yy
      else currentYYYY - currentYY + yy

      GmtDateTime(yyyy, _MM, dd, _HH, mm, ss)
    }
  }
  val asctime_date = (wkday ^ sp :^ date3 ^ sp :^ time ^ sp :^ 4(digit)) -> {
    case (((wkday, (Month(_MM), Numbers(dd))), Time(_HH, mm, ss)), Numbers(yyyy)) => GmtDateTime(yyyy, _MM, dd, _HH, mm, ss)
  }

  // TODO: better performance: http://en.wikipedia.org/wiki/Gregorian_calendar
  // http://www.wikihow.com/Calculate-the-Day-of-the-Week
  case class GmtDateTime(yyyy: Int, MM: Int, dd: Int, HH: Int, mm: Int, ss: Int) {
    val date = {
      val c = Calendar.getInstance()
      c.set(yyyy, MM, dd, HH, mm, ss)
      c.getTime()
    }
    val wkday: String = new SimpleDateFormat("EE").format(date)
    def asRfc1123Date = s"$wkday, $dd ${Month(MM)} $yyyy $HH:$mm:$ss GMT"
  }

  val http_date = rfc1123_date | rfc850_date | asctime_date | fail("not a valid http-date")
  val delta_seconds = 1(digit).+

}