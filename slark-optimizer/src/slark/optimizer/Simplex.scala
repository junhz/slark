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
                          nbv: View.Indexed[Int],
                          bv: View.Indexed[Int]) {
    val basicCount = bv.length
    val nonBasicCount = nbv.length
    val constraintSize = b.length
    val varSize = basicCount + nonBasicCount
    def strict(ai: View.Indexed[Rational], bi: Rational) = {
      val nai = ai.fill(basicCount, Rational.zero)
      StandardForm(a :+ nai, b :+ bi, c, z, nbv :+ varSize, bv)
    }
    
    def strict(constraints: View.Travesal[(View.Indexed[Rational], Rational)]) = {
      val consts = constraints.toList.toArray(Array[(View.Indexed[Rational], Rational)]())
      val size = consts.length
      val na = View.Array(consts).map(_._1.fill(basicCount, Rational.zero))
      StandardForm(a :++ na, b :++ View.Array(consts).map(_._2), c, z, nbv :++ View.Range(varSize, varSize + size), bv)
    }
    
    def add(coe: View.Indexed[Rational], bi: Rational): StandardForm = {
      val ncoe = coe.fill(varSize, Rational.zero)
      val nai = View.Range(0, basicCount).map(i => ncoe(bv(i))).toArray
      var nbi = bi
      View.Range(0, basicCount).foreach(
        row => {
          val col = nbv(row)
          val factor = ncoe(col)
          var idx = 0
          while (idx < basicCount) {
            nai(idx) -= factor * a(row)(idx)
            idx += 1
          }
          nbi -= factor * b(row)
        }
      )
      StandardForm(a :+ View.Array(nai), b :+ nbi, c, z, nbv :+ varSize, bv)
    }
    // TODO: use lhs rhs instead
    override def toString = {
      def name(idx: Int) = s"x${bv(idx)}"
      val max = {
        val coeStr = View.Range(0, basicCount).map(i => {
          val ci = c(i)
          if (ci.isZero) "" else { if(ci.isPositive) s"+ $ci${name(i)}" else s"- ${ci.negate}${name(i)}" }
        }).mkString(" ")
        s"max $z $coeStr"
      }
      
      val subjectTo = if (constraintSize > 0) {
        val firstIdx = Array.fill(constraintSize)(-1)
        View.Range(0, constraintSize).foreach(row => View.Range(0, basicCount).foreach(col => {
          if(firstIdx(row) < 0 && a(row)(col).isZero) firstIdx(row) = col
          else ()
        }))
        val aStr = View.Range(0, constraintSize).map(row => View.Range(0, basicCount).map(col => {
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
        val ajMaxStrLen = View.Cols(aStr, basicCount).map(_.map(_.length).max).toArray
        View.Range(0, constraintSize).map(row => {
          val lhs = View.Range(0, basicCount).map(col => {
            val s = aStr(row)(col)
            new String(Array.fill(ajMaxStrLen(col) - s.length())(' ')) + s
          }).mkString(" ")
          s"$lhs = ${b(row)} - x${nbv(row)}"
        })
      } else View.empty[String]()
      
      (nbv.toString() +: max +: subjectTo).mkString("\r\n")
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
      val pr = View.Array(matrix(row)).toArray
      while (r < matrix.length) {
        val vector = matrix(r)
        var c = 0
        val factor = vector(col)
        while (c < vector.length) {
          if (r == row) {
            if (c == col) vector(c) = Rational.one / factor
            else vector(c) /= factor
          } else {
            if (c == col) vector(c) = -factor / pr(col)
            else vector(c) -= pr(c) * factor / pr(col)
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
    StandardForm(a, b, c, Rational.zero, View.empty(), View.Range(0, varSize))
  }
}