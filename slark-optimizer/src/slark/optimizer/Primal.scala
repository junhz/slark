package slark.optimizer

/**
 * @author a554114
 */
object Primal extends Simplex { self =>
  import Simplex._
  
  trait Selector extends Simplex.Selector {
    def enterStart: Int
    def leaveStart: Int

    final def apply(tableau: Array[Array[Rational]]): (Int, Int) = {
      val enter = View.Range(enterStart, tableau(0).length) some {
        col => tableau(0)(col).isPositive
      } maxBy {
        col => tableau(0)(col)
      }
      enter match {
        case Some(col) => {
          val leave = View.Range(leaveStart, tableau.length) some {
            row => tableau(row)(0).signum() * tableau(row)(col).signum() > 0
          } minBy {
            row => tableau(row)(0) / tableau(row)(col)
          }
          leave match {
            case Some(row) => (row, col)
            case None      => (-1, col)
          }
        }
        case None => (-1, -1)
      }
    }
  }
  
  val phase1 = new Phase {
    def solve(problem: StandardForm): SolveResult = {
      View.Range(0, problem.constraintSize).some(row => problem.b(row).isNegative)
                                           .minBy(row => problem.b(row)) match {
        case Some(row) => {
          val colSize = problem.varSize + problem.slackSize + 2
          val tableau =
            ((View.empty[Rational]().fill(colSize, Rational.zero).updated(colSize - 1, Rational.one.negate)) +:
              ((problem.z +: problem.c) :+ Rational.zero) +:
               View.Range(0, problem.constraintSize).map(row => (problem.b(row) +: problem.a(row)) :+ Rational.one.negate)
              ).map(_.toArray).toArray
          //show(tableau)
          val selector = new self.Selector {
            def enterStart: Int = 1
            def leaveStart: Int = 2
          }
          var selected = (row + 2, colSize - 1)
          while (selected._1 >= 0 && selected._2 >= 0) {
            val (row, col) = selected
            pivot(tableau, row, col)
            selected = selector(tableau)
          }
          selected match {
            case (-1, -1) => tableau(0)(0).isZero match {
              case true => Optimized(StandardForm(View.Rows(tableau).tail.tail.map(_.range(1, colSize - 1)),
                                                  View.Cols(tableau)(0).tail.tail,
                                                  View.Rows(tableau)(1).range(1, colSize - 1),
                                                  tableau(1)(0),
                                                  problem.varSize,
                                                  problem.slackSize,
                                                  problem.constraintSize))
              case false => Infeasible
            }
            case _ => Unbounded
          }
        }
        case None => Optimized(problem)
      }
    }
  }
  
  val phase2 = new Phase {
    def solve(problem: StandardForm): SolveResult = {
      problem.b.forall(!_.isNegative) match {
        case true => {
          val tableau =
            ((problem.z +: problem.c) +:
             View.Range(0, problem.constraintSize).map(row => problem.b(row) +: problem.a(row))
            ).map(_.toArray).toArray
          //show(tableau)
          val selector = new self.Selector {
            def enterStart: Int = 1
            def leaveStart: Int = 1
          }
          var selected = selector(tableau)
          while (selected._1 >= 0 && selected._2 >= 0) {
            val (row, col) = selected
            pivot(tableau, row, col)
            selected = selector(tableau)
          }
          selected match {
            case (-1, -1) => {
              Optimized(StandardForm(View.Rows(tableau).tail.map(_.tail),
                                     View.Cols(tableau)(0).tail,
                                     View.Rows(tableau)(0).tail,
                                     tableau(0)(0),
                                     problem.varSize,
                                     problem.slackSize,
                                     problem.constraintSize))
            }
            case _ => Unbounded
          }
        }
        case false => Infeasible
      }
    }
  }
  
  def solve(problem: StandardForm): SolveResult = {
    phase1.solve(problem) match {
      case Optimized(p) => println("phase1 optimized"); phase2.solve(p)
      case r => r
    }
  }
}