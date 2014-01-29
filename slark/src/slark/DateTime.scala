package slark

/**
 * milliseconds from 1970.1.1 0:0:0.000
 */
final class DateTime(val millis: Long) {
  val (year, month, day, hour, minute, second, millisecond) = {
    DateTime.fire(DateTime.time, millis) match {
      case days :: hours :: minutes :: seconds :: milliseconds :: Nil =>
        DateTime.fire(DateTime.date, days + DateTime.daysFrom1601To1970) match {
          case days :: months :: years :: quoteYears :: centuries :: quoteCenturies :: Nil =>
            ((1601 + quoteCenturies * 400 + centuries * 100 + quoteYears * 4 + years).toInt,
              months,
              days,
              hours,
              minutes,
              seconds,
              milliseconds)
        }
    }
  }
  override def toString = s"$year.$month.$day $hour:$minute:$second.$millisecond"
}

protected[this] object DateTime {

  val time = Milliseconds :: Seconds :: Minutes :: Hours :: Nil
  val date = QuoteCenturies :: Centuries :: QuoteYears :: Years :: MonthsAndDays :: Nil

  val daysFrom1601To1970 = 134774

  def fire(calcs: List[Calc], input: Long): List[Long] = {
    @tailrec
    def rec(rest: List[Calc], results: List[Long], cnt: Long): List[Long] = {
      if (rest.isEmpty) throw new IllegalArgumentException
      else rest.head.calc(results, cnt) match {
        case End(rs) => rs ::: results
        case Next(r, n) => rec(rest.tail, r :: results, n)
      }
    }
    rec(calcs, Nil, input)
  }

  trait Calc {
    def calc(results: List[Long], cnt: Long): CalcResult
  }
  trait CalcResult
  case class End(results: List[Long]) extends CalcResult
  case class Next(result: Long, next: Long) extends CalcResult

  object Milliseconds extends Calc {
    val millisecondsOfSecond = 1000
    override def calc(results: List[Long], cnt: Long): CalcResult = Next(cnt % millisecondsOfSecond, cnt / millisecondsOfSecond)
  }

  object Seconds extends Calc {
    val secondsOfMinute = 60
    override def calc(results: List[Long], cnt: Long): CalcResult = Next(cnt % secondsOfMinute, cnt / secondsOfMinute)
  }

  object Minutes extends Calc {
    val minutesOfHour = 60
    override def calc(results: List[Long], cnt: Long): CalcResult = Next(cnt % minutesOfHour, cnt / minutesOfHour)
  }

  object Hours extends Calc {
    val hoursOfDay = 24
    override def calc(results: List[Long], cnt: Long): CalcResult = End(cnt / hoursOfDay :: cnt % hoursOfDay :: Nil)
  }

  object QuoteCenturies extends Calc {
    val daysOfQuoteCentury = 146097
    override def calc(results: List[Long], cnt: Long): CalcResult = Next(cnt / daysOfQuoteCentury, cnt % daysOfQuoteCentury)
  }

  object Centuries extends Calc {
    /**
     * ignore the year divisible by 400
     */
    protected val daysOfCentury = 36524
    override def calc(results: List[Long], cnt: Long): CalcResult = {
      if (cnt == 0) End(1L /*day*/ :: 1L /*month*/ :: 0L /*years*/ :: 0L /*quote years*/ :: 0L /*centuries*/ :: Nil)
      else {
        val centuries = cnt / daysOfCentury
        val rest = cnt % daysOfCentury
        if (centuries == 4 && rest == 0)
          End(31L /*day*/ :: 12L /*month*/ :: 3L /*years*/ :: 24L /*quote years*/ :: 3L /*centuries*/ :: Nil)
        else Next(centuries, rest)
      }
    }
  }

  object QuoteYears extends Calc {
    /**
     * ignore the year divisible by 100
     */
    protected val daysOfQuoteYears = 1461
    override def calc(results: List[Long], cnt: Long): CalcResult = {
      if (cnt == 0) End(1L /*day*/ :: 1L /*month*/ :: 0L /*years*/ :: 0L /*quote years*/ :: Nil)
      else {
        val quoteYears = cnt / daysOfQuoteYears
        val rest = cnt % daysOfQuoteYears
        Next(quoteYears, rest)
      }
    }
  }

  object Years extends Calc {
    /**
     * ignore leap year
     */
    protected val daysOfYear = 365
    override def calc(results: List[Long], cnt: Long): CalcResult = {
      if (cnt == 0) End(1L /*day*/ :: 1L /*month*/ :: 0L /*years*/ :: Nil)
      else {
        val years = cnt / daysOfYear
        val rest = cnt % daysOfYear
        if (years == 4 && rest == 0) End(31L /*day*/ :: 12L /*month*/ :: 3L /*years*/ :: Nil)
        else Next(years, rest)
      }
    }
  }

  object MonthsAndDays extends Calc {
    val monthsOfLeap = 31 :: 60 :: 91 :: 121 :: 152 :: 182 :: 213 :: 244 :: 274 :: 305 :: 335 :: 366 :: Nil
    val months = 31 :: 59 :: 90 :: 120 :: 151 :: 181 :: 212 :: 243 :: 273 :: 304 :: 334 :: 365 :: Nil
    override def calc(results: List[Long], cnt: Long): CalcResult = {
      val isLeap = results match {
        case 3 /*years*/ :: rest => rest match {
          case 24 /*quote years*/ :: rest => rest match {
            case 3 /*centuries*/ :: rest => true
            case _ => false
          }
          case _ => true
        }
        case _ => false
      }

      @tailrec
      def rec(restMonths: List[Int], mord: Int, lastMonth: Int): (Int, Int) = {
        if (restMonths.isEmpty) throw new IllegalArgumentException
        else if (restMonths.head > cnt) (mord, (cnt - lastMonth + 1).toInt)
        else rec(restMonths.tail, mord + 1, restMonths.head)
      }

      rec(if (isLeap) monthsOfLeap else months, 1, 0) match {
        case (mord, dord) => End(dord.toLong :: mord.toLong :: Nil)
      }
    }
  }
}