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
      View.Array(problem.b).indexed.some(t => t._2.isNegative).minBy(_._2) match {
        case Some((row, _)) => {
          val tableau =
            (Array.fill(problem.c.length + 2)(Rational.zero).updated(problem.c.length + 1, Rational.one.negate)) +:
              (problem.z.negate +: problem.c ++: Array.fill(1)(Rational.zero)) +:
              View.Array(problem.a).indexed.map({
                case (i, arr) => problem.b(i) +: arr ++: Array.fill(1)(Rational.one.negate)
              }).toArray
          //show(tableau)
          val selector = new self.Selector {
            def enterStart: Int = 1
            def leaveStart: Int = 2
          }
          var selected = (row + 2, problem.c.length + 1)
          while (selected._1 >= 0 && selected._2 >= 0) {
            val (row, col) = selected
            pivot(tableau, row, col)
            selected = selector(tableau)
          }
          selected match {
            case (-1, -1) => tableau(0)(0).isZero match {
              case true => Optimized(StandardForm(tableau.tail.tail.map(_.tail.dropRight(1)),
                                                  View.Cols(tableau)(0).toArray.tail.tail,
                                                  tableau(1).tail.dropRight(1),
                                                  tableau(1)(0),
                                                  problem.originalSize))
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
            (problem.z +: problem.c) +:
            View.Array(problem.a).indexed.map({
              case (i, arr) => problem.b(i) +: arr
            }).toArray
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
              var col = 1
              while (col < tableau(0).length) {
                if (tableau(0)(col).isZero) {
                  var row = 1
                  while (row < tableau.length) {
                    var factor = tableau(row)(col)
                    if (factor.isZero) ()
                    else {
                      var i = 0
                      while (i < tableau(0).length) {
                        tableau(row)(i) /= factor
                        i += 1
                      }
                    }
                    row += 1
                  }
                } else ()
                col += 1
              }
              Optimized(StandardForm(tableau.tail.map(_.tail),
                                     View.Cols(tableau)(0).toArray.tail,
                                     tableau(0).tail,
                                     tableau(0)(0),
                                     problem.originalSize))
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