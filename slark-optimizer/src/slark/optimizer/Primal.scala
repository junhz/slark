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
      val enter = View.OfRange(enterStart, tableau(0).length) some {
        col => tableau(0)(col).isPositive
      } maxBy {
        col => tableau(0)(col)
      }
      enter match {
        case Some(col) => {
          val leave = View.OfRange(leaveStart, tableau.length) some {
            row => tableau(row)(col).isPositive
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
      problem.nbv.count(_.isInstanceOf[Simplex.ArtifactVar]) match {
        case 0 => Optimized(problem)
        case artifactCount => {
          val tableau = {
            val tableauView =
              View.empty[Rational]().fill(problem.m + 1, Rational.zero) +:
                (problem.z +: problem.c) +:
                View.OfRange(0, problem.n).map(row => (problem.b(row) +: problem.a(row)))
            val array = tableauView.map(_.toArray).toArray
            var row = 0
            while (row < problem.n) {
              problem.nbv(row) match {
                case ArtifactVar(_) => {
                  var col = 0
                  while (col < problem.m + 1) {
                    array(0)(col) += array(row + 2)(col)
                    col += 1
                  }
                }
                case _              => ()
              }
              row += 1
            }
            array
          }
          val basic = problem.bv.toArray
          val nonBasic = problem.nbv.toArray

          //show(tableau)
          val selector = new self.Selector {
            def enterStart: Int = 1
            def leaveStart: Int = 2
          }
          var selected = selector(tableau)
          while (selected._1 >= 0 && selected._2 >= 0) {
            val (row, col) = selected
            pivot(tableau, row, col)
            //show(tableau)
            val enter = nonBasic(row - 2)
            val leave = basic(col - 1)
            nonBasic(row - 2) = leave
            basic(col - 1) = enter
            selected = selector(tableau)
          }
          selected match {
            case (-1, -1) => tableau(0)(0).isZero match {
              case true => {
                def pick[T](indexed: View.Indexed[T], indices: View.Indexed[Int]) = indices.map(indexed(_))
                val basicIdx = View.OfArray(View.OfRange(0, problem.m).some(!basic(_).isInstanceOf[ArtifactVar]).toArray)
                Optimized(StandardForm(View.OfArray(tableau).tail.tail.map(ai => pick(View.OfArray(ai).tail, basicIdx)),
                  View.OfArray(tableau).tail.tail.map(_(0)),
                  pick(View.OfArray(tableau(1)).tail(), basicIdx),
                  tableau(1)(0),
                  View.OfArray(nonBasic),
                  pick(View.OfArray(basic), basicIdx)))
              }
              case false => Infeasible
            }
            case _ => Unbounded
          }
        }
      }
    }
  }
  
  val phase2 = new Phase {
    def solve(problem: StandardForm): SolveResult = {
      problem.b.forall(!_.isNegative) match {
        case true => {
          val tableau = {
            val view = 
              (problem.z +: problem.c) +:
              View.OfRange(0, problem.n).map(row => problem.b(row) +: problem.a(row))
            view.map(_.toArray).toArray
          }
          val basic = problem.bv.toArray
          val nonBasic = problem.nbv.toArray
          
          //show(tableau)
          val selector = new self.Selector {
            def enterStart: Int = 1
            def leaveStart: Int = 1
          }
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
              Optimized(StandardForm(View.OfArray(tableau).tail.map(View.OfArray(_).tail),
                                     View.OfArray(tableau).tail.map(_(0)),
                                     View.OfArray(View.OfArray(tableau)(0)).tail,
                                     tableau(0)(0),
                                     View.OfArray(nonBasic),
                                     View.OfArray(basic)))
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