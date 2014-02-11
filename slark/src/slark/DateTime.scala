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

  private[this] trait Calc {
    def apply(cnt: Long): (Int, Long)
    def unapply(input: Int, cnt: Long): Option[Long]
  }

  private[this] object MillisecondOfSecond extends Calc {
    val millisecondsOfSecond = 1000
    override def apply(cnt: Long) = ((cnt % millisecondsOfSecond).toInt, cnt / millisecondsOfSecond)
    override def unapply(input: Int, cnt: Long): Option[Long] =
      if (input < millisecondsOfSecond && input >= 0) Some(cnt * millisecondsOfSecond + input) else None
  }

  private[this] object SecondOfMinute extends Calc {
    val secondsOfMinute = 60
    override def apply(cnt: Long) = ((cnt % secondsOfMinute).toInt, cnt / secondsOfMinute)
    override def unapply(input: Int, cnt: Long): Option[Long] =
      if (input < secondsOfMinute && input >= 0) Some(cnt * secondsOfMinute + input) else None
  }

  private[this] object MinuteOfHour extends Calc {
    val minutesOfHour = 60
    override def apply(cnt: Long) = ((cnt % minutesOfHour).toInt, cnt / minutesOfHour)
    override def unapply(input: Int, cnt: Long): Option[Long] =
      if (input < minutesOfHour && input >= 0) Some(cnt * minutesOfHour + input) else None
  }

  private[this] object HourOfDay extends Calc {
    val hoursOfDay = 24
    override def apply(cnt: Long) = ((cnt % hoursOfDay).toInt, cnt / hoursOfDay)
    override def unapply(input: Int, cnt: Long): Option[Long] =
      if (input < hoursOfDay && input >= 0) Some(cnt * hoursOfDay + input) else None
  }

  private[this] object QuadCenturies extends Calc {
    val daysOfQuadCentury = 146097
    override def apply(cnt: Long) = ((cnt / daysOfQuadCentury).toInt, cnt % daysOfQuadCentury)
    override def unapply(input: Int, cnt: Long): Option[Long] = Some(input * daysOfQuadCentury + cnt)
  }

  private[this] object CenturyOfQuadCentury extends Calc {
    /**
     * ignore the year divisible by 400
     */
    protected val daysOfCentury = 36524
    override def apply(cnt: Long) = {
      val centuries = cnt / daysOfCentury
      val rest = cnt % daysOfCentury
      if (centuries == 4 && rest == 0) (3, daysOfCentury)
      else (centuries.toInt, rest)
    }
    override def unapply(input: Int, cnt: Long): Option[Long] = Some(input * daysOfCentury + cnt)
  }

  private[this] object QuadYearOfCentury extends Calc {
    /**
     * ignore the year divisible by 100
     */
    protected val daysOfQuadYears = 1461
    override def apply(cnt: Long) = ((cnt / daysOfQuadYears).toInt, cnt % daysOfQuadYears)
    override def unapply(input: Int, cnt: Long): Option[Long] = Some(input * daysOfQuadYears + cnt)
  }

  private[this] object YearOfQuadYear extends Calc {
    /**
     * ignore leap year
     */
    protected val daysOfYear = 365
    override def apply(cnt: Long) = {
      val years = cnt / daysOfYear
      val rest = cnt % daysOfYear
      if (years == 4 && rest == 0) (3, 365)
      else (years.toInt, rest)
    }
    override def unapply(input: Int, cnt: Long): Option[Long] = Some(cnt + daysOfYear * input)
  }

  def since1970(timeMillis: Long): DateTime = {
    def calc(calcs: List[Calc], input: Long): (List[Int], Long) = {
      @tailrec
      def rec(rest: List[Calc], results: List[Int], cnt: Long): (List[Int], Long) = {
        if (rest.isEmpty) (results, cnt)
        else {
          val (r, c) = rest.head.apply(cnt)
          rec(rest.tail, r :: results, c)
        }
      }
      rec(calcs, Nil, input)
    }

    val time = MillisecondOfSecond :: SecondOfMinute :: MinuteOfHour :: HourOfDay :: Nil
    val date = QuadCenturies :: CenturyOfQuadCentury :: QuadYearOfCentury :: YearOfQuadYear :: Nil
    val (hourOfDay :: minuteOfHour :: secondOfMinute :: millisecondOfSecond :: Nil, daysSince1970) = calc(time, timeMillis)
    val (yearOfQuadYear :: quadYearOfCentury :: centuryOfQuadCentury :: quadCenturies :: Nil, dayOfYear) = calc(date, daysSince1970 + `0001.1.1-1970.1.1`)

    def floor(src: Int, floors: List[Int]): (Int, Int) = {
      @tailrec
      def rec(rest: Int, floorOrd: Int, restFloors: List[Int]): (Int, Int) = {
        if (restFloors.isEmpty) (floorOrd, rest)
        else if (rest > restFloors.head) rec(rest - restFloors.head, floorOrd + 1, restFloors.tail)
        else (floorOrd, rest)
      }
      rec(src, 1, floors)
    }
    val (monthOrd, dayOrd) = floor(dayOfYear.toInt + 1, daysOfMonths(yearOfQuadYear, quadYearOfCentury, centuryOfQuadCentury))
    return new DateTime(
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
    val yearsSince0001 = year - 1
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

    def calc(inputs: List[Int], calcs: List[Calc], out: Long): Option[Long] = {
      if (inputs.isEmpty && calcs.isEmpty) Some(out)
      else if (inputs.isEmpty ^ calcs.isEmpty) None
      else calcs.head.unapply(inputs.head, out) match {
        case Some(r) => calc(inputs.tail, calcs.tail, r)
        case _ => None
      }
    }

    val date = YearOfQuadYear :: QuadYearOfCentury :: CenturyOfQuadCentury :: QuadCenturies :: Nil
    val time = HourOfDay :: MinuteOfHour :: SecondOfMinute :: MillisecondOfSecond :: Nil

    val daysSince1970 = calc(yearOfQuadYear :: quadYearOfCentury :: centuryOfQuadCentury :: quadCenturies :: Nil, date, -`0001.1.1-1970.1.1`) match {
      case None => throw new IllegalArgumentException
      case Some(s) => s + dayOfYear
    }
    val timeMillis = calc(hour :: minute :: second :: millisecond :: Nil, time, daysSince1970) match {
      case None => throw new IllegalArgumentException
      case Some(s) => s
    }
    return new DateTime(
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
