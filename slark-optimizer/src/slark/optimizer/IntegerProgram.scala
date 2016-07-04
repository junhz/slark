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
    
    val consts = problem.consts map {
      case Constraint(ai, <, bi) => {
        val factor = lcm(ai)
        Constraint(ai, <=, ((bi * factor).ceil - Rational.one) / factor)
      }
      case Constraint(ai, >, bi) => {
        val factor = lcm(ai)
        Constraint(ai, >=, ((bi * factor).floor + Rational.one) / factor)
      }
      case c => c
    }
    
    LinearProgram(problem.obj, problem.coefficients, consts)
  }
}