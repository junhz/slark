package slark.optimizer

/**
 * @author a554114
 */
object Dual extends Simplex {
  import Simplex._
  
  val selector = new Selector {
    def apply(tableau: Array[Array[Rational]]): (Int, Int) = {
      val leave = View.Cols(tableau)(0).indexed some {
        case (row, c) => row > 0 && c.isNegative
      } minBy {
        case (row, c) => c
      }
      leave match {
        case Some((row, _)) => {
          val enter = View.Rows(tableau)(0).indexed map {
            case (col, b) => (col, b, tableau(row)(col))
          } some {
            case (row, b, a) => row > 0 && b.signum() * a.signum() > 0
          } minBy {
            case (row, b, a) => b / a
          }
          enter match {
            case Some((col, _, _)) => (row, col)
            case None              => (row, -1)
          }
        }
        case None => (-1, -1)
      }
    }
  }
  
  val phase2 = new Phase {
    def solve(problem: StandardForm): SolveResult = {
      problem.c.forall(!_.isPositive) match {
        case true => {
          val tableau =
            (problem.z +: problem.c) +:
            View.Array(problem.a).indexed.map({
              case (i, arr) => problem.b(i) +: arr
            }).toArray
          show(tableau)
          var selected = selector(tableau)
          while (selected._1 >= 0 && selected._2 >= 0) {
            val (row, col) = selected
            pivot(tableau, row, col)
            selected = selector(tableau)
          }
          selected match {
            case (-1, -1) => Optimized(StandardForm(tableau.tail.map(_.tail),
                                                    View.Cols(tableau)(0).toArray.tail,
                                                    tableau(0).tail,
                                                    tableau(0)(0),
                                                    problem.originalSize))
            case _ => Unbounded
          }
        }
        case false => Infeasible
      }
    }
  }
  
  def solve(problem: StandardForm): SolveResult = {
    phase2.solve(problem)
  }
}