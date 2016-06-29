package slark.optimizer

/**
 * @author a554114
 */
object Dual extends Simplex {
  import Simplex._
  
  val selector = new Selector {
    def apply(tableau: Array[Array[Rational]]): (Int, Int) = {
      val leave = View.Range(1, tableau.length) some {
        row => tableau(row)(0).isNegative
      } minBy {
        row => tableau(row)(0)
      }
      leave match {
        case Some(row) => {
          val enter = View.Range(1, tableau(0).length) some {
            col => tableau(row)(col).isNegative
          } minBy {
            col => tableau(0)(col) / tableau(row)(col)
          }
          enter match {
            case Some(col) => (row, col)
            case None      => (row, -1)
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
          val tableau = {
            val view = 
              (problem.z +: problem.c) +:
              View.Range(0, problem.nonBasicCount).map(row => problem.b(row) +: problem.a(row))
            view.map(_.toArray).toArray
          }
          val basic = problem.bv.toArray
          val nonBasic = problem.nbv.toArray
          
          //show(tableau)
          var selected = selector(tableau)
          while (selected._1 >= 0 && selected._2 >= 0) {
            val (row, col) = selected
            pivot(tableau, row, col)
            //show(tableau)
            val enter = nonBasic(row - 1)
            val leave = basic(col - 1)
            nonBasic(row - 1) = leave
            basic(col - 1) = enter
            selected = selector(tableau)
          }
          selected match {
            case (-1, -1) => {
              Optimized(StandardForm(View.Rows(tableau).tail.map(_.tail),
                                     View.Cols(tableau, 1)(0).tail,
                                     View.Rows(tableau)(0).tail,
                                     tableau(0)(0),
                                     View.Array(nonBasic),
                                     View.Array(basic)))
            }
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