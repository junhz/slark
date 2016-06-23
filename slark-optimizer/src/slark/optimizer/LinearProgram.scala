package slark.optimizer

import javax.swing.SpringLayout.Constraints

/**
 * @author a554114
 */
case class LinearProgram(obj: LinearProgram.Objection, coefficients: View.Indexed[Rational], consts: Vector[LinearProgram.Constraint]) {
  
  def withConstraint(const: LinearProgram.Constraint) = 
    new LinearProgram(obj, coefficients, consts :+ LinearProgram.Constraint(const.coefficients.fill(coefficients.length, Rational.zero),
                                                                            const.relation,
                                                                            const.constant))
  
  override def toString = {
    val objStr = {
      val coeStr = coefficients.indexed.map {
        case (i, r) => if (r.isZero) "" else if (r.isPositive) s"+ ${r}x$i" else s"- ${r.negate}x$i"
      }.mkString(" ")
      s"${obj} $coeStr"
    }
    val constStr = consts.map {
      const => {
        val coeStr = coefficients.indexed.map {
          case (i, _) => {
            val r = const.coefficients(i)
            if (r.isZero) "" else if (r.isPositive) s" + ${r}x$i" else s" - ${r.negate}x$i"
          }
        }.mkString
        s"$coeStr ${const.relation} ${const.constant}"
      }
    }
    (objStr +: constStr).mkString("\r\n")
  }
}

object LinearProgram {
  trait Objection
  case object Min extends Objection
  case object Max extends Objection
  
  case class Constraint(coefficients: View.Indexed[Rational], relation: Relation, constant: Rational) {
    def isViolated(xs: View.Indexed[Rational]): Boolean = {
      val nxs = xs.fill(coefficients.length, Rational.zero)
      val lhs = View.Range(0, coefficients.length).fold(Rational.zero, (i: Int, r: Rational) => r + coefficients(i) * nxs(i))
      relation.isViolated(lhs, constant)
    }
  }
  trait Relation {
    def isEquality: Boolean
    def isViolated(lhs: Rational, rhs: Rational): Boolean
  }
  case object `<=` extends Relation {
    def isEquality = false
    def isViolated(lhs: Rational, rhs: Rational) = lhs <= rhs
  }
  case object `>=` extends Relation {
    def isEquality = false
    def isViolated(lhs: Rational, rhs: Rational) = lhs >= rhs
  }
  case object `=` extends Relation {
    def isEquality = true
    def isViolated(lhs: Rational, rhs: Rational) = lhs == rhs
  }
  case object `>` extends Relation {
    def isEquality = false
    def isViolated(lhs: Rational, rhs: Rational) = lhs > rhs
  }
  case object `<` extends Relation {
    def isEquality = false
    def isViolated(lhs: Rational, rhs: Rational) = lhs < rhs
  }
}