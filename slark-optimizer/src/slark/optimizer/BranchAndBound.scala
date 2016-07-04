package slark.optimizer

import scala.collection.mutable.PriorityQueue
import java.math.BigInteger

trait BranchAndBound { self =>
  import IntegerProgram._
  def cuttingPlanes: List[CuttingPlane]
  
  def and(cp: CuttingPlane) = new BranchAndBound {
    val cuttingPlanes = cp :: self.cuttingPlanes
  }
  
  final def solve(problem: LinearProgram): SolveResult = solve(Simplex.format(IntegerProgram.format(problem)))
  
  final def solve(originProblem: Simplex.StandardForm): SolveResult = {
    Primal.solve(originProblem) match {
      case Simplex.Optimized(st) => {
        var lowerBound: Simplex.SolveResult = Simplex.Infeasible
        val queue = PriorityQueue(st)(new Ordering[Simplex.StandardForm] {
          // -z desc
          def compare(x: Simplex.StandardForm, y: Simplex.StandardForm): Int = y.z.compare(x.z)
        })
        def isActive(p: Simplex.StandardForm) = lowerBound match {
          // -z
          case Simplex.Optimized(s) => s.z > p.z
          case _               => true
        }
        while (!queue.isEmpty) {
          val problem = queue.dequeue()
          lowerBound = if (isActive(problem)) {
            Dual.solve(cuttingPlanes.foldLeft(problem)((p, cp) => cp(p))) match {
              case Simplex.Optimized(p) if (isActive(p)) => {
                View.OfRange(0, p.n).some(!p.b(_).isInteger).minBy(row => (p.b(row).fraction - Rational(1, 2)).abs()) match {
                  case Some(row) => {
                    val ai = p.a(row)
                    val bi = p.b(row)
                    queue.enqueue(p.newSlack(ai.map(_.negate), bi.floor - bi),
                                  p.newSlack(ai, bi - bi.ceil))
                    lowerBound
                  }
                  case None => Simplex.Optimized(p)
                }
              }
              case _ => lowerBound
            }
          } else {
            queue.clear()
            lowerBound
          }
        }
        lowerBound match {
          case Simplex.Optimized(p) => {
            val xs = View.empty().fill(originProblem.m + originProblem.n, Rational.zero).toArray
            var row = 0
            while (row < p.n) {
              p.nbv(row) match {
                case Simplex.DecideVar(ord) => xs(ord) = p.b(row)
                case _                      => ()
              }
              row += 1
            }
            Optimized(p.z, View.OfArray(xs))
          }
          case _ => Infeasible
        }
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
  
  def apply() = new BranchAndBound {
    val cuttingPlanes = Nil
  }
  
}