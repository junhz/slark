package slark

final class DateTime private[DateTime](val year: Int, val month: DateTime.Month, val dayOfMonth: Int, val dayOfWeek: DateTime.Weekday, 
    val hour: Int, val minute: Int, val second: Int, val millisecond: Int, timeMillis: Long) {

  override final def toString = s"$year.${month.shortName}.$dayOfMonth $hour:$minute:$second.$millisecond ${dayOfWeek.shortName}"
}

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
  val `0001.1.1-1970.1.1` = 719162

  trait Calc {
    def apply(results: List[Int], cnt: Long): (List[Int], Long)
  }

  object MillisecondOfSecond extends Calc {
    val millisecondsOfSecond = 1000
    override def apply(results: List[Int], cnt: Long) = ((cnt % millisecondsOfSecond).toInt :: results, cnt / millisecondsOfSecond)
  }

  object SecondOfMinute extends Calc {
    val secondsOfMinute = 60
    override def apply(results: List[Int], cnt: Long) = ((cnt % secondsOfMinute).toInt :: results, cnt / secondsOfMinute)
  }

  object MinuteOfHour extends Calc {
    val minutesOfHour = 60
    override def apply(results: List[Int], cnt: Long) = ((cnt % minutesOfHour).toInt :: results, cnt / minutesOfHour)
  }

  object HourOfDay extends Calc {
    val hoursOfDay = 24
    override def apply(results: List[Int], cnt: Long) = ((cnt % hoursOfDay).toInt :: results, cnt / hoursOfDay)
  }

  object QuadCenturies extends Calc {
    val daysOfQuadCentury = 146097
    override def apply(results: List[Int], cnt: Long) = ((cnt / daysOfQuadCentury).toInt :: results, cnt % daysOfQuadCentury)
  }

  object CenturyOfQuadCentury extends Calc {
    /**
     * ignore the year divisible by 400
     */
    protected val daysOfCentury = 36524
    override def apply(results: List[Int], cnt: Long) = {
      val centuries = cnt / daysOfCentury
      val rest = cnt % daysOfCentury
      if (centuries == 4 && rest == 0) (3 :: results, daysOfCentury)
      else (centuries.toInt :: results, rest)
    }
  }

  object QuadYearOfCencury extends Calc {
    /**
     * ignore the year divisible by 100
     */
    protected val daysOfQuadYears = 1461
    override def apply(results: List[Int], cnt: Long) = ((cnt / daysOfQuadYears).toInt :: results, cnt % daysOfQuadYears)
  }

  object YearOfQuadYear extends Calc {
    /**
     * ignore leap year
     */
    protected val daysOfYear = 365
    override def apply(results: List[Int], cnt: Long) = {
      val years = cnt / daysOfYear
      val rest = cnt % daysOfYear
      if (years == 4 && rest == 0) (3 :: results, 365)
      else (years.toInt :: results, rest)
    }
  }

  def since1970(timeMillis: Long): DateTime = {
    def calc(calcs: List[Calc], input: Long): (List[Int], Long) = {
      @tailrec
      def rec(rest: List[Calc], results: List[Int], cnt: Long): (List[Int], Long) = {
        if (rest.isEmpty) (results, cnt)
        else {
          val (r, c) = rest.head.apply(results, cnt)
          rec(rest.tail, r, c)
        }
      }
      rec(calcs, Nil, input)
    }

    val time = MillisecondOfSecond :: SecondOfMinute :: MinuteOfHour :: HourOfDay :: Nil
    val date = QuadCenturies :: CenturyOfQuadCentury :: QuadYearOfCencury :: YearOfQuadYear :: Nil
    val (hourOfDay :: minuteOfHour :: secondOfMinute :: millisecondOfSecond :: Nil, daysSince1970) = calc(time, timeMillis)
    val (yearOfQuadYear :: quadYearOfCencury :: centuryOfQuadCencury :: quadCenturiesSince0001 :: Nil, dayOfYear) = calc(date, daysSince1970 + `0001.1.1-1970.1.1`)
    val isLeap =
      if (yearOfQuadYear == 3) {
        if (quadYearOfCencury == 24) {
          if (centuryOfQuadCencury == 3) true
          else false
        } else true
      } else false
    val daysOfMonths = 31 :: (if (isLeap) 29 else 28) :: 31 :: 30 :: 31 :: 30 :: 31 :: 31 :: 30 :: 31 :: 30 :: 31 :: Nil

    def floor(src: Int, floors: List[Int]): (Int, Int) = {
      @tailrec
      def rec(rest: Int, floorOrd: Int, restFloors: List[Int]): (Int, Int) = {
        if (restFloors.isEmpty) (floorOrd, rest)
        else if (rest > restFloors.head) rec(rest - restFloors.head, floorOrd + 1, restFloors.tail)
        else (floorOrd, rest)
      }
      rec(src, 1, floors)
    }
    val (monthOrd, dayOrd) = floor(dayOfYear.toInt + 1, daysOfMonths)
    return new DateTime(
      year = 1 + quadCenturiesSince0001 * 400 + centuryOfQuadCencury * 100 + quadYearOfCencury * 4 + yearOfQuadYear,
      month = Month.order(monthOrd),
      dayOfMonth = dayOrd,
      dayOfWeek = Weekday.order((3 + daysSince1970.toInt) % 7 + 1),
      hour = hourOfDay,
      minute = minuteOfHour,
      second = secondOfMinute,
      millisecond = millisecondOfSecond,
      timeMillis = timeMillis
    )
  }
  
  // calc option timeMillis and validate
  def apply(year: Int, month: Month, day: Int, hour: Int, minute: Int, second: Int, millisecond: Int): DateTime = {
    val isLeap = (year % 400 == 0) || ((year % 100 != 0) && (year % 4 == 0))
    val daysOfMonths = 31 :: (if (isLeap) 29 else 28) :: 31 :: 30 :: 31 :: 30 :: 31 :: 31 :: 30 :: 31 :: 30 :: 31 :: Nil
    
  }
  
}
