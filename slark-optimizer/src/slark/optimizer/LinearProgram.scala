package slark.optimizer

import javax.swing.SpringLayout.Constraints

/**
 * @author a554114
 */
case class LinearProgram(val obj: LinearProgram.Objection, val consts: Vector[LinearProgram.Constraint]) {
  
  def withConstraint(const: LinearProgram.Constraint) = new LinearProgram(obj, consts :+ const)
  
  override def toString = {
    val objStr = {
      val coeStr = View.Array(obj.coefficients).indexed.map {
        case (i, r) => if (r.isZero) "" else if (r.isPositive) s"+ ${r}x$i" else s"- ${r.negate}x$i"
      }.mkString(" ")
      s"${obj.name} $coeStr"
    }
    val constStr = consts.map {
      const => {
        val coeStr = View.Array(obj.coefficients).indexed.map {
          case (i, _) => {
            val r = const.coefficients(i)
            if (r.isZero) "" else if (r.isPositive) s" + ${r}x$i" else s" - ${r.negate}x$i"
          }
        }.mkString
        s"$coeStr ${const.name} ${const.constant}"
      }
    }
    (objStr +: constStr).mkString("\r\n")
  }
}

object LinearProgram {
  trait Objection {
    def name: String
    def coefficients: Array[Rational]
  }
  case class Min(coefficients: Array[Rational]) extends Objection {
    def name = "Min"
  }
  case class Max(coefficients: Array[Rational]) extends Objection {
    def name = "Max"
  }
  
  trait Constraint {
    def name: String
    def coefficients: Int => Rational
    def constant: Rational
    def isEquality: Boolean
  }
  case class `≤`(coefficients: Int => Rational, constant: Rational) extends Constraint {
    def name: String = "≤"
    def isEquality = false
  }
  case class `≥`(coefficients: Int => Rational, constant: Rational) extends Constraint {
    def name: String = "≥"
    def isEquality = false
  }
  case class `=`(coefficients: Int => Rational, constant: Rational) extends Constraint {
    def name: String = "="
    def isEquality = true
  }
}