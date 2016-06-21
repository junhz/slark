package slark.optimizer

import scala.collection.mutable.PriorityQueue
import java.math.BigInteger

trait BranchAndBound { self =>
  import BranchAndBound._
  def cuttingPlanes: List[CuttingPlane]
  
  def and(cp: CuttingPlane) = new BranchAndBound {
    val cuttingPlanes = cp :: self.cuttingPlanes
  }
  
  def solve(originProblem: Simplex.StandardForm): SolveResult = {
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
                val xs = View.Array(p.c).indexed.range(0, originProblem.originalSize).map {
                  case (col, c) => c.isZero match {
                    case true => {
                      val row = View.Cols(p.a)(col).indexed.maxBy(_._2)._1
                      p.b(row)
                    }
                    case false => Rational.zero
                  }
                }.toArray
                View.Array(xs).indexed.some(!_._2.isInteger).minBy(x => (x._2.decimal - Rational(1, 2)).abs()) match {
                  case Some((col, x)) => {
                    queue.enqueue(p.strict(i => if (i == col) Rational.one else Rational.zero, x.floor),
                                  p.strict(i => if (i == col) Rational.one.negate else Rational.zero, x.ceil.negate))
                    lowerBound
                  }
                  case None => Optimized(p.z, xs)
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