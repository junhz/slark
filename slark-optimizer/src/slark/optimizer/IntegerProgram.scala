package slark.optimizer

import java.math.BigInteger

/**
 * @author a554114
 */
object IntegerProgram {
  trait SolveResult
  case class Optimized(z: Rational, xs: View.Indexed[Rational]) extends SolveResult {
    override def toString = s"$z <- (${xs.mkString(",")})"
  }
  case object Infeasible extends SolveResult
  case object Unbounded extends SolveResult
  
  def format(problem: LinearProgram): LinearProgram = {
    import LinearProgram._
    
    def lcm(arr: View.Indexed[Rational]) = arr.fold(BigInteger.ONE, (a, r: BigInteger) => a.denominator.multiply(r).divide(a.denominator.gcd(r)))
    
    val consts = problem.consts map(const => {
      val lcm = const.coefficients.fold(BigInteger.ONE, (r, i: BigInteger) => r.denominator.multiply(i).divide(r.denominator.gcd(i)))
      val coe = const.coefficients.map(_ * lcm)
      val c = Rational.fromBigInt(lcm) * const.constant
      const.relation match {
        case `<`  => Constraint(coe, <=, c.ceil - Rational.one)
        case `>`  => Constraint(coe, >=, c.floor + Rational.one)
        case `<=` => Constraint(coe, <=, c.floor)
        case `>=` => Constraint(coe, >=, c.ceil)
        case `=`  => if (c.isInteger) Constraint(coe, `=`, c.ceil) else throw new IllegalArgumentException("invalid constraint")
      }
    })
    
    LinearProgram(problem.obj, problem.coefficients, consts)
  }
}