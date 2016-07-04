package slark.optimizer

import java.math.BigInteger
import java.math.BigDecimal
import java.math.RoundingMode

final class Rational private (val numerator: BigInteger, val denominator: BigInteger) extends Ordered[Rational] {
  import Rational._
  
  def +(that: Rational): Rational = apply(this.numerator * that.denominator + this.denominator * that.numerator, this.denominator * that.denominator)
  def -(that: Rational): Rational = apply(this.numerator * that.denominator - this.denominator * that.numerator, this.denominator * that.denominator)
  def *(that: Rational): Rational = apply(this.numerator * that.numerator, this.denominator * that.denominator)
  def unary_-(): Rational = new Rational(-this.numerator, this.denominator)
  def toInt(): Int = numerator.divide(denominator).intValue()
  def toLong(): Long = numerator.divide(denominator).longValue()
  def toFloat(): Float = new BigDecimal(numerator).divide(new BigDecimal(denominator), 45, RoundingMode.HALF_UP).floatValue()
  def toDouble(): Double = new BigDecimal(numerator).divide(new BigDecimal(denominator), 324, RoundingMode.HALF_UP).doubleValue()
  
  def compare(that: Rational): Int = (this.numerator * that.denominator) compareTo (this.denominator * that.numerator)
  
  def abs(): Rational = new Rational(numerator.abs(), denominator)
  def signum(): Int = numerator.signum()
  
  def /(that: Rational): Rational = {
    if (that.isZero) throw new ArithmeticException("divide by zero")
    else {
      if (that.isPositive) apply(this.numerator * that.denominator, this.denominator * that.numerator)
      else apply(this.numerator * that.denominator.negate(), this.denominator * that.numerator.negate())
    }
  }
  
  def *(that: BigInteger): Rational = apply(this.numerator * that, this.denominator)
  
  def /(that: BigInteger): Rational = {
    if (that.signum() == 0) throw new ArithmeticException("divide by zero")
    else {
      if (that.signum() > 0) apply(this.numerator, this.denominator * that)
      else apply(this.numerator, this.denominator * that.negate())
    }
  }
  
  def fraction = new Rational(numerator.mod(denominator), denominator)
  
  def isInteger = denominator.compareTo(BigInteger.ONE) == 0
  
  def ceil = if (isInteger) this 
             else if (isPositive) new Rational(numerator.divide(denominator).add(BigInteger.ONE), BigInteger.ONE) 
                  else new Rational(numerator.divide(denominator), BigInteger.ONE)
  
  def floor = if (isInteger) this 
              else if (isPositive) new Rational(numerator.divide(denominator), BigInteger.ONE) 
                   else new Rational(numerator.divide(denominator).add(BigInteger.ONE.negate()), BigInteger.ONE)
  
  override def toString = if (denominator.compareTo(BigInteger.ONE) == 0) s"$numerator" else s"$numerator/$denominator"
  
  override def equals(that: Any) = that match {
    case r: Rational => (this eq r) || (this.compare(r) == 0)
    case _ => false
  }
  
  def isPositive = numerator.signum() > 0
  def isZero = numerator.signum() == 0
  def isNegative = numerator.signum() < 0
  def negate = new Rational(numerator.negate(), denominator)
}

object Rational extends Numeric[Rational] with Fractional[Rational] {
  
  private[Rational] implicit class BigIntegerOps(val self: BigInteger) extends AnyVal {
    def +(that: BigInteger): BigInteger = self add that
    def -(that: BigInteger): BigInteger = self subtract that
    def *(that: BigInteger): BigInteger = self multiply that
    def unary_-(): BigInteger = self.negate()
  }
  
  override val zero = new Rational(BigInteger.ZERO, BigInteger.ONE)
  override val one = new Rational(BigInteger.ONE, BigInteger.ONE)
  
  override def plus(x: Rational, y: Rational): Rational = x + y
  override def minus(x: Rational, y: Rational): Rational = x - y
  override def times(x: Rational, y: Rational): Rational = x * y
  override def negate(x: Rational): Rational = -x
  override def fromInt(x: Int): Rational = new Rational(BigInteger.valueOf(x), BigInteger.ONE)
  override def toInt(x: Rational): Int = x.toInt()
  override def toLong(x: Rational): Long = x.toLong()
  override def toFloat(x: Rational): Float = x.toFloat()
  override def toDouble(x: Rational): Double = x.toDouble()
  
  override def compare(x: Rational, y: Rational): Int = x compareTo y
  
  override def abs(x: Rational): Rational = x.abs()
  override def signum(x: Rational): Int = x.signum()
  
  override def div(x: Rational, y: Rational): Rational = x / y
  
  def apply(x: BigInteger, y: BigInteger): Rational = {
    if (y.signum() == 0) {
      throw new ArithmeticException("divide by zero")
    } else if (x.signum() == 0) {
      zero
    }/* else if (x.compareTo(y) == 0) {
      one
    }*/ else {
      val gcd = x.gcd(y)
      new Rational(x.divide(gcd), y.divide(gcd))
    }
  }
  
  def apply(x: Int, y: Int): Rational = {
    apply(BigInteger.valueOf(x), BigInteger.valueOf(y))
  }
}