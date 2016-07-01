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
                      varSize)

  override def toString = {
    val rhs = coefficients +: View.OfVector(consts).map(_.coefficients)
    val view = (Tapped("max", "") +: View.OfVector(consts).map(const => Tapped(const.constant.toString(), s" ${const.relation.negate()} "))) +:
               View.OfRange(0, varSize).map(col => View.OfRange(0, consts.length + 1).map(row => {
                 val r = rhs(row)(col)
                 r.signum() match {
                   case 0 => Tapped("   ", "")
                   case 1 => Tapped(" + ", s"${r}x$col")
                   case -1 => Tapped(" - ", s"${r.negate}x$col")
                 }
               }))
    
    val str = view.map(_.toArray).toArray
    val len =  View.OfArray(str).map(col => View.OfArray(col).map(_.length).max).toArray
    View.OfRange(0, consts.length + 1).map(row => View.OfRange(0, varSize + 1).map(col => str(col)(row).fill(len(col))).mkString).mkString("\r\n")
  }
}

object LinearProgram {
  trait Objection
  case object Min extends Objection
  case object Max extends Objection
  
  case class Constraint(coefficients: View.Indexed[Rational], relation: Relation, constant: Rational) {
    def isViolated(xs: View.Indexed[Rational]): Boolean = {
      val nxs = xs.fill(coefficients.length, Rational.zero)
      val lhs = View.OfRange(0, coefficients.length).fold(Rational.zero, (i: Int, r: Rational) => r + coefficients(i) * nxs(i))
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