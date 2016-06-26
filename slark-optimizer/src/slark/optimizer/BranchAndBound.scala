package slark.optimizer

import scala.collection.mutable.PriorityQueue
import java.math.BigInteger

trait BranchAndBound { self =>
  import BranchAndBound._
  def cuttingPlanes: List[CuttingPlane]
  
  def and(cp: CuttingPlane) = new BranchAndBound {
    val cuttingPlanes = cp :: self.cuttingPlanes
  }
  
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
        var lowerBound: SolveResult = Infeasible
        val queue = PriorityQueue(st)(new Ordering[Simplex.StandardForm] {
          // -z desc
          def compare(x: Simplex.StandardForm, y: Simplex.StandardForm): Int = y.z.compare(x.z)
        })
        def isActive(p: Simplex.StandardForm) = lowerBound match {
          // -z
          case Optimized(z, _) => z > p.z
          case _               => true
        }
        while (!queue.isEmpty) {
          val problem = queue.dequeue()
          lowerBound = if (isActive(problem)) {
            Dual.solve(cuttingPlanes.foldLeft(problem)((p, cp) => cp(p))) match {
              case Simplex.Optimized(p) if (isActive(p)) => {
                View.Range(0, p.n.length).some(!p.b(_).isInteger).minBy(row => (p.b(row).fraction - Rational(1, 2)).abs()) match {
                  case Some(row) => {
                    val col = p.n(row)
                    val ai = p.a(row)
                    val bi = p.b(row)
                    queue.enqueue(p.strict(ai.map(_.negate).update(col, Rational.zero), bi.floor - bi),
                                  p.strict(ai.update(col, Rational.zero), bi - bi.ceil))
                    lowerBound
                  }
                  case None => {
                    val xs = Array.fill(p.varSize)(Rational.zero)
                    View.Range(0, p.n.length).foreach {
                      row => xs(p.n(row)) = p.b(row)
                    }
                    Optimized(p.z, xs)
                  }
                }
              }
              case _ => lowerBound
            }
          } else {
            queue.clear()
            lowerBound
          }
        }
        lowerBound
      }
      case Simplex.Infeasible => Infeasible
      case Simplex.Unbounded => Unbounded
    }
  }
}

/**
 * @author a554114
 */
object BranchAndBound {
  
  trait SolveResult
  case class Optimized(z: Rational, xs: Array[Rational]) extends SolveResult {
    override def toString = s"$z <- (${xs.mkString(",")})"
  }
  case object Infeasible extends SolveResult
  case object Unbounded extends SolveResult
  
  def apply() = new BranchAndBound {
    val cuttingPlanes = Nil
  }
  
}