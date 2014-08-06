package slark

final class DateTime private[DateTime] (
  val year: Int,
  val month: DateTime.Month,
  val dayOfMonth: Int,
  val dayOfWeek: DateTime.Weekday,
  val hour: Int,
  val minute: Int,
  val second: Int,
  val millisecond: Int,
  val timeMillis: Long) {

  override final def toString = s"$year.${month.shortName}.$dayOfMonth $hour:$minute:$second.$millisecond ${dayOfWeek.shortName}"
}

/**
 * assume that world start at 0001.01.01 00:00:00.000
 */
object DateTime {
  final class Month private[DateTime] (val shortName: String, val order: Int)
  object Month {
    val Jan = new Month("Jan", 1)
    val Feb = new Month("Feb", 2)
    val Mar = new Month("Mar", 3)
    val Apr = new Month("Apr", 4)
    val May = new Month("May", 5)
    val Jun = new Month("Jun", 6)
    val Jul = new Month("Jul", 7)
    val Aug = new Month("Aug", 8)
    val Sep = new Month("Sep", 9)
    val Oct = new Month("Oct", 10)
    val Nov = new Month("Nov", 11)
    val Dec = new Month("Dec", 12)

    def order(order: Int): Month = order match {
      case 1 => Jan
      case 2 => Feb
      case 3 => Mar
      case 4 => Apr
      case 5 => May
      case 6 => Jun
      case 7 => Jul
      case 8 => Aug
      case 9 => Sep
      case 10 => Oct
      case 11 => Nov
      case 12 => Dec
    }

    def shortName(shortName: String): Month = shortName match {
      case "Jan" => Jan
      case "Feb" => Feb
      case "Mar" => Mar
      case "Apr" => Apr
      case "May" => May
      case "Jun" => Jun
      case "Jul" => Jul
      case "Aug" => Aug
      case "Sep" => Sep
      case "Oct" => Oct
      case "Nov" => Nov
      case "Dec" => Dec
    }
  }
  final class Weekday private[DateTime] (val shortName: String, val fullName: String, val order: Int)
  object Weekday {
    val Mon = new Weekday("Mon", "Monday", 1)
    val Tue = new Weekday("Tue", "Tuesday", 2)
    val Wed = new Weekday("Wed", "Wednesday", 3)
    val Thu = new Weekday("Thu", "Thursday", 4)
    val Fri = new Weekday("Fri", "Friday", 5)
    val Sat = new Weekday("Sat", "Saturday", 6)
    val Sun = new Weekday("Sun", "Sunday", 7)

    def order(order: Int): Weekday = order match {
      case 1 => Mon
      case 2 => Tue
      case 3 => Wed
      case 4 => Thu
      case 5 => Fri
      case 6 => Sat
      case 7 => Sun
    }

    def shortName(shortName: String): Weekday = shortName match {
      case "Mon" => Mon
      case "Tue" => Tue
      case "Wed" => Wed
      case "Thu" => Thu
      case "Fri" => Fri
      case "Sat" => Sat
      case "Sun" => Sun
    }

    def fullName(fullName: String): Weekday = fullName match {
      case "Monday" => Mon
      case "Tuesday" => Tue
      case "Wednesday" => Wed
      case "Thursday" => Thu
      case "Friday" => Fri
      case "Saturday" => Sat
      case "Sunday" => Sun
    }
  }

  /**
   * days from 1601.1.1 to 1970.1.1
   */
  private[this] val `0001.1.1-1970.1.1` = 719162

  private[this] def daysOfMonths(yearOfQuadYear: Int, quadYearOfCentury: Int, centuryOfQuadCentury: Int): List[Int] = {
    val isLeap =
      if (yearOfQuadYear == 3) {
        if (quadYearOfCentury == 24) {
          if (centuryOfQuadCentury == 3) true
          else false
        } else true
      } else false
    31 :: (if (isLeap) 29 else 28) :: 31 :: 30 :: 31 :: 30 :: 31 :: 31 :: 30 :: 31 :: 30 :: 31 :: Nil
  }

  private[this] trait TimeUnit {
    def unpickle(in: Long): (Int, Long)
    def pickle(src: Int, out: Long): Option[Long]
  }

  private[this] object MillisecondOfSecond extends TimeUnit {
    val millisecondsOfSecond = 1000
    override def unpickle(in: Long) = ((in % millisecondsOfSecond).toInt, in / millisecondsOfSecond)
    override def pickle(src: Int, out: Long): Option[Long] =
      if (src < millisecondsOfSecond && src >= 0) Some(out * millisecondsOfSecond + src) else None
  }

  private[this] object SecondOfMinute extends TimeUnit {
    val secondsOfMinute = 60
    override def unpickle(in: Long) = ((in % secondsOfMinute).toInt, in / secondsOfMinute)
    override def pickle(src: Int, out: Long): Option[Long] =
      if (src < secondsOfMinute && src >= 0) Some(out * secondsOfMinute + src) else None
  }

  private[this] object MinuteOfHour extends TimeUnit {
    val minutesOfHour = 60
    override def unpickle(in: Long) = ((in % minutesOfHour).toInt, in / minutesOfHour)
    override def pickle(src: Int, out: Long): Option[Long] =
      if (src < minutesOfHour && src >= 0) Some(out * minutesOfHour + src) else None
  }

  private[this] object HourOfDay extends TimeUnit {
    val hoursOfDay = 24
    override def unpickle(in: Long) = ((in % hoursOfDay).toInt, in / hoursOfDay)
    override def pickle(src: Int, out: Long): Option[Long] =
      if (src < hoursOfDay && src >= 0) Some(out * hoursOfDay + src) else None
  }

  private[this] object QuadCenturies extends TimeUnit {
    val daysOfQuadCentury = 146097
    override def unpickle(in: Long) = ((in / daysOfQuadCentury).toInt, in % daysOfQuadCentury)
    override def pickle(src: Int, out: Long): Option[Long] = Some(src * daysOfQuadCentury + out)
  }

  private[this] object CenturyOfQuadCentury extends TimeUnit {
    /**
     * ignore the year divisible by 400
     */
    protected val daysOfCentury = 36524
    override def unpickle(in: Long) = {
      val centuries = in / daysOfCentury
      val rest = in % daysOfCentury
      if (centuries == 4 && rest == 0) (3, daysOfCentury)
      else (centuries.toInt, rest)
    }
    override def pickle(src: Int, out: Long): Option[Long] = Some(src * daysOfCentury + out)
  }

  private[this] object QuadYearOfCentury extends TimeUnit {
    /**
     * ignore the year divisible by 100
     */
    protected val daysOfQuadYears = 1461
    override def unpickle(in: Long) = ((in / daysOfQuadYears).toInt, in % daysOfQuadYears)
    override def pickle(src: Int, out: Long): Option[Long] = Some(src * daysOfQuadYears + out)
  }

  private[this] object YearOfQuadYear extends TimeUnit {
    /**
     * ignore leap year
     */
    protected val daysOfYear = 365
    override def unpickle(in: Long) = {
      val years = in / daysOfYear
      val rest = in % daysOfYear
      if (years == 4 && rest == 0) (3, 365)
      else (years.toInt, rest)
    }
    override def pickle(src: Int, out: Long): Option[Long] = Some(src * daysOfYear + out)
  }

  def since1970(timeMillis: Long): DateTime = {
    def unpickle(timeUnits: List[TimeUnit], in: Long): (List[Int], Long) = {
      @tailrec
      def rec(restTimeUnits: List[TimeUnit], results: List[Int], in: Long): (List[Int], Long) = {
        if (restTimeUnits.isEmpty) (results, in)
        else {
          val (r, i) = restTimeUnits.head.unpickle(in)
          rec(restTimeUnits.tail, r :: results, i)
        }
      }
      rec(timeUnits, Nil, in)
    }

    val time = MillisecondOfSecond :: SecondOfMinute :: MinuteOfHour :: HourOfDay :: Nil
    val date = QuadCenturies :: CenturyOfQuadCentury :: QuadYearOfCentury :: YearOfQuadYear :: Nil
    val (hourOfDay :: minuteOfHour :: secondOfMinute :: millisecondOfSecond :: Nil, daysSince1970) = unpickle(time, timeMillis)
    val (yearOfQuadYear :: quadYearOfCentury :: centuryOfQuadCentury :: quadCenturies :: Nil, dayOfYear) = unpickle(date, daysSince1970 + `0001.1.1-1970.1.1`)

    def jump(height: Int, floors: List[Int]): (Int, Int) = {
      @tailrec
      def rec(rest: Int, floorOrd: Int, restFloors: List[Int]): (Int, Int) = {
        if (restFloors.isEmpty) (floorOrd, rest)
        else if (rest > restFloors.head) rec(rest - restFloors.head, floorOrd + 1, restFloors.tail)
        else (floorOrd, rest)
      }
      rec(height, 1, floors)
    }
    val (monthOrd, dayOrd) = jump(dayOfYear.toInt + 1, daysOfMonths(yearOfQuadYear, quadYearOfCentury, centuryOfQuadCentury))
    new DateTime(
      year = 1 /*since 0001*/ + quadCenturies * 400 + centuryOfQuadCentury * 100 + quadYearOfCentury * 4 + yearOfQuadYear,
      month = Month.order(monthOrd),
      dayOfMonth = dayOrd,
      dayOfWeek = Weekday.order((3 + daysSince1970.toInt) % 7 + 1),
      hour = hourOfDay,
      minute = minuteOfHour,
      second = secondOfMinute,
      millisecond = millisecondOfSecond,
      timeMillis = timeMillis)
  }

  def apply(year: Int, month: Month, day: Int, hour: Int, minute: Int, second: Int, millisecond: Int): DateTime = {
    val yearsSince0001 = year - 1 /*since 0001*/
    val yearOfQuadYear = yearsSince0001 % 4
    val quadYearOfCentury = yearsSince0001 % 100 / 4
    val centuryOfQuadCentury = yearsSince0001 % 400 / 100
    val quadCenturies = yearsSince0001 / 400

    def fold(floors: List[Int], level: Int, height: Int): Option[Int] = {
      @tailrec
      def rec(restFloors: List[Int], restLevel: Int, totalHeight: Int): Option[Int] = {
        if (restLevel == 1) {
          if (restFloors.head < height) None
          else Some(totalHeight + height)
        } else if (restFloors.isEmpty) None
        else rec(restFloors.tail, restLevel - 1, totalHeight + restFloors.head)
      }
      rec(floors, level, 0)
    }
    val dayOfYear = fold(daysOfMonths(yearOfQuadYear, quadYearOfCentury, centuryOfQuadCentury), month.order, day) match {
      case None => throw new IllegalArgumentException
      case Some(s) => s - 1
    }

    def pickle(srcs: List[Int], timeUnits: List[TimeUnit], out: Long): Option[Long] = {
      if (srcs.isEmpty && timeUnits.isEmpty) Some(out)
      else if (srcs.isEmpty ^ timeUnits.isEmpty) None
      else timeUnits.head.pickle(srcs.head, out) match {
        case Some(r) => pickle(srcs.tail, timeUnits.tail, r)
        case _ => None
      }
    }

    val date = YearOfQuadYear :: QuadYearOfCentury :: CenturyOfQuadCentury :: QuadCenturies :: Nil
    val time = HourOfDay :: MinuteOfHour :: SecondOfMinute :: MillisecondOfSecond :: Nil

    val daysSince1970 = pickle(yearOfQuadYear :: quadYearOfCentury :: centuryOfQuadCentury :: quadCenturies :: Nil, date, -`0001.1.1-1970.1.1`) match {
      case None => throw new IllegalArgumentException
      case Some(s) => s + dayOfYear
    }
    val timeMillis = pickle(hour :: minute :: second :: millisecond :: Nil, time, daysSince1970) match {
      case None => throw new IllegalArgumentException
      case Some(s) => s
    }
    new DateTime(
      year = year,
      month = month,
      dayOfMonth = day,
      dayOfWeek = Weekday.order((3 + daysSince1970.toInt) % 7 + 1),
      hour = hour,
      minute = minute,
      second = second,
      millisecond = millisecond,
      timeMillis = timeMillis)
  }

}
