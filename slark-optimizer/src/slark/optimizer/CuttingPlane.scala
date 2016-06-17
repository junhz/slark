package slark.optimizer

/**
 * @author a554114
 */
trait CuttingPlane {
  import CuttingPlane._
  
  def apply(problem: Simplex.StandardForm): Simplex.StandardForm
  
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
              val xs = View.Array(p.c).indexed.range(0, originProblem.originalSize).map {
                case (col, c) => c.isZero match {
                  case true => {
                    val row = View.Cols(p.a)(col).indexed.maxBy(_._2)._1
                    p.b(row)
                  }
                  case false => Rational.zero
                }
              }.toArray
              xs.forall(_.isInteger) match {
                case true => {
                  end = true
                  cnt = Optimized(p.z, xs)
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
  case class Optimized(z: Rational, xs: Array[Rational]) extends SolveResult {
    override def toString = s"$z <- (${xs.mkString(",")})"
  }
  case object Infeasible extends SolveResult
  case object Unbounded extends SolveResult
}