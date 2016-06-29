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
      val tableau = {
        val tableauView =
          View.empty[Rational]().fill(problem.basicCount + 1, Rational.zero) +:
          (problem.z +: problem.c) +:
          View.Range(0, problem.constraintSize).map(row => (problem.b(row) +: problem.a(row)))
        val array = tableauView.map(_.toArray).toArray
        View.Range(0, problem.constraintSize).foreach(row => {
          View.Range(0, problem.basicCount + 1).foreach(col => array(0)(col) += array(row + 2)(col))
        })
        array
      }
      val basic = problem.bv.toArray
      val nonBasic = View.Range(problem.varSize, problem.varSize + problem.constraintSize).toArray
      
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
            // TODO: negative
            val basicIndex = new Array[Int](problem.basicCount - problem.constraintSize)
            var idx = 0
            View.Range(0, problem.basicCount).foreach {
              col => if (basic(col) < problem.varSize) { basicIndex(idx) = col; idx += 1 } else ()
            }
            val basicIdx = View.Array(basicIndex)
            Optimized(StandardForm(View.Rows(tableau).tail.tail.map(ai => pick(ai.tail, basicIdx)),
                                   View.Cols(tableau, 1)(0).tail.tail,
                                   pick(View.Array(tableau(1)).tail(), basicIdx),
                                   tableau(1)(0),
                                   View.Array(nonBasic),
                                   pick(View.Array(basic), basicIdx)))
          }
          case false => Infeasible
        }
        case _ => Unbounded
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
              View.Range(0, problem.constraintSize).map(row => problem.b(row) +: problem.a(row))
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
    phase1.solve(problem) match {
      case Optimized(p) => println("phase1 optimized"); phase2.solve(p)
      case r => r
    }
  }
}