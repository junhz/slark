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
      val enter = View.Rows(tableau)(0).indexed some {
        case (col, c) => col >= enterStart && c.isPositive
      } maxBy {
        case (col, c) => c
      }
      enter match {
        case Some((col, c)) => {
          val leave = View.Cols(tableau)(0).indexed map {
            case (row, b) => (row, b, tableau(row)(col))
          } some {
            case (row, b, a) => row >= leaveStart && b.signum() * a.signum() == 1
          } minBy {
            case (row, b, a) => b / a
          }
          leave match {
            case Some((row, _, _)) => (row, col)
            case None => (-1, col)
          }
        }
        case None => (-1, -1)
      }
    }
  }
  
  val phase1 = new Phase {
    def solve(problem: StandardForm): SolveResult = {
      problem.b.indexed.some(t => t._2.isNegative).minBy(_._2) match {
        case Some((row, _)) => {
          val colSize = problem.varSize + problem.slackSize + 2
          val tableau =
            ((View.empty[Rational]().fill(colSize, Rational.zero).updated(colSize - 1, Rational.one.negate)) +:
              ((problem.z +: problem.c) :+ Rational.zero) +:
              problem.a.indexed.map({
                case (i, arr) => (problem.b(i) +: arr) :+ Rational.one.negate
              })).map(_.toArray).toArray
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
            problem.a.indexed.map({
              case (i, arr) => problem.b(i) +: arr
            })).map(_.toArray).toArray
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