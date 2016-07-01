package slark.optimizer

import java.math.BigInteger

/**
 * @author a554114
 */
trait CuttingPlane {
  import CuttingPlane._
  
  def apply(problem: Simplex.StandardForm): Simplex.StandardForm
  
  final def solve(problem: LinearProgram): SolveResult = {
    import LinearProgram._
    
    def lcm(arr: View.Indexed[Rational]) = arr.fold(BigInteger.ONE, (a, r: BigInteger) => a.denominator.multiply(r).divide(a.denominator.gcd(r)))
    
    val consts = problem.consts map {
      case Constraint(ai, <, bi) => {
        val factor = Rational(lcm(ai), BigInteger.ONE)
        Constraint(ai, <=, ((bi * factor).ceil - Rational.one) / factor)
      }
      case Constraint(ai, >, bi) => {
        val factor = Rational(lcm(ai), BigInteger.ONE)
        Constraint(ai, >=, ((bi * factor).floor + Rational.one) / factor)
      }
      case c => c
    }
    
    solve(Simplex.format(LinearProgram(problem.obj, problem.coefficients, consts, problem.varSize)))
  }
  
  final def solve(originProblem: Simplex.StandardForm): SolveResult = {
    Primal.solve(originProblem) match {
      case Simplex.Optimized(st) => {
        var cnt: SolveResult = Infeasible
        var problem = st
        var end = false
        while (!end) {
          val cut = apply(problem)
          Dual.solve(cut) match {
            case Simplex.Optimized(p) => {
              p.b.forall(_.isInteger) match {
                case true => {
                  end = true
                  cnt = {
                    val xs = View.empty().fill(originProblem.m + originProblem.n, Rational.zero).toArray
                    var row = 0
                    while (row < p.n) {
                      p.nbv(row) match {
                        case Simplex.DecideVar(ord) => xs(ord) = p.b(row)
                        case _ => ()
                      }
                      row += 1
                    }
                    Optimized(p.z, View.OfArray(xs))
                  }
                }
                case false => problem = p
              }
            }
            case _ => {
              end = true
              cnt = Infeasible
            }
          }
        }
        cnt
      }
      case Simplex.Infeasible => Infeasible
      case Simplex.Unbounded => Unbounded
    }
  }
  
}

object CuttingPlane {
  trait SolveResult
  case class Optimized(z: Rational, xs: View.Indexed[Rational]) extends SolveResult {
    override def toString = s"$z <- (${xs.mkString(",")})"
  }
  case object Infeasible extends SolveResult
  case object Unbounded extends SolveResult
}