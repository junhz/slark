package slark.optimizer

import javax.swing.SpringLayout.Constraints

/**
 * @author a554114
 */
case class LinearProgram(obj: LinearProgram.Objection, 
                         coefficients: View.Indexed[Rational], 
                         consts: Vector[LinearProgram.Constraint],
                         varSize: Int) {
  
  def subjectTo(const: LinearProgram.Constraint) = 
    new LinearProgram(obj, 
                      coefficients, 
                      consts :+ LinearProgram.Constraint(const.coefficients.fill(varSize, Rational.zero),
                                                         const.relation,
                                                         const.constant),
                      varSize);
  
  override def toString = {
    def toString(coefficients: View.Indexed[Rational]) = View.Range(0, varSize).map(i => {
      val r = coefficients(i)
      if (r.isZero) "" else if (r.isPositive) s" + ${r}x$i" else s" - ${r.negate}x$i"
    }).mkString
    val objStr = s"${obj} ${toString(coefficients)}"
    val constStr = consts.map {
      const => s"${toString(const.coefficients)} ${const.relation} ${const.constant}"
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
    
    def negate() = Constraint(coefficients.map(_.negate), relation.negate(), constant.negate)
  }
  trait Relation {
    def isEquality: Boolean
    def isViolated(lhs: Rational, rhs: Rational): Boolean
    def negate(): Relation
  }
  case object `<=` extends Relation {
    def isEquality = false
    def isViolated(lhs: Rational, rhs: Rational) = lhs > rhs
    def negate = `>=`
  }
  case object `>=` extends Relation {
    def isEquality = false
    def isViolated(lhs: Rational, rhs: Rational) = lhs < rhs
    def negate = `<=`
  }
  case object `=` extends Relation {
    def isEquality = true
    def isViolated(lhs: Rational, rhs: Rational) = (lhs compareTo rhs) != 0
    def negate = this
  }
  case object `>` extends Relation {
    def isEquality = false
    def isViolated(lhs: Rational, rhs: Rational) = lhs <= rhs
    def negate = `<`
  }
  case object `<` extends Relation {
    def isEquality = false
    def isViolated(lhs: Rational, rhs: Rational) = lhs >= rhs
    def negate = `>`
  }
  
  def ofGoal(obj: Objection, coefficients: View.Indexed[Rational]): LinearProgram = {
    LinearProgram(obj, coefficients, Vector.empty, coefficients.length)
  }
}