package slark.optimizer

import java.math.BigInteger

/**
 * @author a554114
 */
trait Simplex {
  import Simplex._
  def solve(problem: StandardForm): SolveResult
}

object Simplex {
  case class StandardForm(a: View.Indexed[View.Indexed[Rational]], 
                          b: View.Indexed[Rational], 
                          c: View.Indexed[Rational], 
                          z: Rational,
                          n: View.Indexed[Int]) {
    val varSize = c.length
    val constraintSize = b.length
    def strict(ai: View.Indexed[Rational], bi: Rational) = {
      val nai = ai.fill(varSize + 1, Rational.zero).update(varSize, Rational.one)
      StandardForm(a.map(_ :+ Rational.zero) :+ nai, b :+ bi, c :+ Rational.zero, z, n :+ varSize)
    }
    
    def strict(constraints: View.Travesal[(View.Indexed[Rational], Rational)]) = {
      val consts = constraints.toList.toArray(Array[(View.Indexed[Rational], Rational)]())
      val size = consts.length
      val na = View.Range(0, size).map(i => consts(i)._1.fill(varSize + size, Rational.zero).update(varSize + i, Rational.one))
      StandardForm(a.map(_ .fill(varSize + size, Rational.zero)) :++ na, b :++ View.Array(consts).map(_._2), c.fill(varSize + size, Rational.zero), z, n :++ View.Range(varSize, varSize + size))
    }
    
    def format(ai: View.Indexed[Rational], bi: Rational): (View.Indexed[Rational], Rational) = {
      val nai = ai.fill(varSize, Rational.zero).toArray
      var nbi = bi
      View.Range(0, n.length).foreach(
        row => {
          val col = n(row)
          val factor = nai(col)
          var idx = 0
          while (idx < varSize) {
            nai(idx) -= factor * a(row)(idx)
            idx += 1
          }
          nbi -= factor * b(row)
        }
      )
      (View.Array(nai), nbi)
    }
    // TODO: use lhs rhs instead
    override def toString = {
      def name(idx: Int) = if (idx < varSize) s"x$idx" else s"s${idx - varSize}"
      val max = {
        val coeStr = View.Range(0, varSize).map(i => {
          val ci = c(i)
          if (ci.isZero) "" else { if(ci.isPositive) s"+ $ci${name(i)}" else s"- ${ci.negate}${name(i)}" }
        }).mkString(" ")
        s"max $z $coeStr"
      }
      
      val subjectTo = if (constraintSize > 0) {
        val firstIdx = Array.fill(constraintSize)(-1)
        View.Range(0, constraintSize).foreach(row => View.Range(0, varSize).foreach(col => {
          if(firstIdx(row) < 0 && a(row)(col).isZero) firstIdx(row) = col
          else ()
        }))
        val aStr = View.Range(0, constraintSize).map(row => View.Range(0, varSize).map(col => {
          val aij = a(row)(col)
          if (aij.isZero) ""
          else {
            val n = name(col)
            if (col == firstIdx(row)) {
              if (col == 0) s"$aij$n"
              else if (aij.isPositive) s"  $aij$n" else s"- ${aij.negate}$n"
            } else if (aij.isPositive) s"+ $aij$n" else s"- ${aij.negate}$n"
          }
        })).map(_.toArray).toArray
        val ajMaxStrLen = View.Cols(aStr, varSize).map(_.map(_.length).max).toArray
        View.Range(0, constraintSize).map(row => {
          val lhs = View.Range(0, varSize).map(col => {
            val s = aStr(row)(col)
            new String(Array.fill(ajMaxStrLen(col) - s.length())(' ')) + s
          }).mkString(" ")
          s"$lhs = ${b(row)}"
        })
      } else View.empty[String]()
      
      (n.toString() +: max +: subjectTo).mkString("\r\n")
    }
  }

  sealed trait SolveResult

  case class Optimized(problem: StandardForm) extends SolveResult {
    override def toString = s"Optimized\r\n$problem"
  }
  case object Infeasible extends SolveResult
  case object Unbounded extends SolveResult
  
  trait Phase {

    def solve(problem: StandardForm): SolveResult

    final def pivot(matrix: Array[Array[Rational]], row: Int, col: Int): Unit = {
      //println(s"pivot at ($row, $col)")
      var r = 0
      while (r < matrix.length) {
        val vector = matrix(r)
        var c = 0
        val factor = vector(col)
        while (c < vector.length) {
          if (r == row) {
            vector(c) /= factor
          } else {
            vector(c) -= matrix(row)(c) * factor / matrix(row)(col)
          }
          c += 1
        }
        r += 1
      }
      //show(matrix)
    }

    final def show(matrix: Array[Array[Rational]]): Unit = {
      val textLen = View.Cols(matrix, matrix(0).length).map(arr => arr.map(_.toString().length()).max).toArray
      val s = View.Rows(matrix).map(arr => View.Range(0, textLen.length).map(col => {
        val s = arr(col).toString()
        new String(Array.fill(textLen(col) - s.length)(' ')) + s
      }).mkString(" "))
      println(s.mkString("\r\n"))
    }
  }
  
  trait Selector {
    def apply(tableau: Array[Array[Rational]]): (Int, Int)
  }
  
  def format(problem: LinearProgram): StandardForm = {
    import LinearProgram._
    val decideSize = problem.varSize
    val consts = problem.consts map {
      const => if (const.constant.isNegative) const.negate() else const
    }
    val constraintSize = consts.length
    val slackIdx = new Array[Int](constraintSize)
    var slackSize = 0
    View.Range(0, constraintSize).foreach(i => {
      val const = consts(i)
      if (const.relation.isEquality) slackIdx(i) = -1 
      else {
        slackIdx(i) = slackSize
        slackSize += 1
      }
    })
    val varSize = decideSize + slackSize
    val c = problem.obj match {
      case Max => problem.coefficients.fill(varSize, Rational.zero)
      case Min => problem.coefficients.map(_.negate).fill(varSize, Rational.zero)
    }
    val b = View.Vector(consts).map(_.constant)
    
    val a = View.Range(0, constraintSize).map { i => {
      val const = consts(i)
      const match {
        case Constraint(ai, <=, _) => ai.fill(varSize, Rational.zero).update(slackIdx(i) + decideSize, Rational.one)
        case Constraint(ai, `=`, _) => ai.fill(varSize, Rational.zero)
        case Constraint(ai, >=, _) => ai.fill(varSize, Rational.zero).update(slackIdx(i) + decideSize, Rational.one.negate)
        case _ => throw new IllegalArgumentException("only >=, <=, = are acceptable")
      }
    }}
    StandardForm(a, b, c, Rational.zero, View.empty())
  }
}