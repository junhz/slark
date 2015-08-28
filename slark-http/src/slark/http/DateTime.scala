package slark
package http

import combinator.parser._

trait DateTime { self: Literals =>

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
  val time = (digit{2} ^ ":" :^ digit{2} ^ ":" :^ digit{2})

  val date1 = digit{2} ^ sp :^ month ^ sp :^ digit{4}
  val date2 = digit{2} ^ "-" :^ month ^ "-" :^ digit{2}
  val date3 = month ^ sp :^ (digit{2} | (sp :^ digit{1}))
  val rfc1123_date = (wkday ^ ", " :^ date1 ^ sp :^ time ^: " GMT") -> {
    case _ ^ (Natural0(d) ^ m ^ Natural0(y)) ^ (Natural0(h) ^ Natural0(mi) ^ Natural0(s)) => Rfc1123Date(y, m, d, h, mi, s)
  }
  val rfc850_date = (weekday ^ ", " :^ date2 ^ sp :^ time ^: " GMT") -> {
    case _ ^ (Natural0(d) ^ m ^ Natural0(y)) ^ (Natural0(h) ^ Natural0(mi) ^ Natural0(s)) => Rfc1123Date(y2k(y, cntYYYY), m, d, h, mi, s)
  }
  val asctime_date = (wkday ^ sp :^ date3 ^ sp :^ time ^ sp :^ digit{4}) -> {
    case _ ^ (m ^ Natural0(d)) ^ (Natural0(h) ^ Natural0(mi) ^ Natural0(s)) ^ Natural0(y) => Rfc1123Date(y, m, d, h, mi, s)
  }

  case class Rfc1123Date(yyyy: Int, _MMM: String, dd: Int, _HH: Int, mm: Int, ss: Int) {
    private[this] val d: slark.DateTime = slark.DateTime.apply(yyyy, slark.DateTime.Month.shortName(_MMM), dd, _HH, mm, ss, 0)
    override def toString: String = f"${d.dayOfWeek.shortName}, ${d.dayOfMonth}%02d ${d.month.shortName} ${d.year}%04d ${d.hour}%02d:${d.minute}%02d:${d.second}%02d GMT"
  }

  val http_date = rfc1123_date | rfc850_date | asctime_date | fail(InvalidDate)
  val delta_seconds = digit(1, `>`)

  case object InvalidDate extends FailReason
  
}