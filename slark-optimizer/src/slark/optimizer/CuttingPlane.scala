package slark.optimizer

import java.math.BigInteger

/**
 * @author a554114
 */
trait CuttingPlane {
  import IntegerProgram._
  
  def apply(problem: Simplex.StandardForm): Simplex.StandardForm
  
  final def solve(problem: LinearProgram): SolveResult = solve(Simplex.format(IntegerProgram.format(problem)))
  
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