import breeze.linalg.{sum, DenseMatrix, DenseVector}
import breeze.numerics.pow

import scala.annotation.tailrec

package object optimization {

  type VD = DenseVector[Double]
  type MD = DenseMatrix[Double]

  def norm2(x: VD, y:VD): Double =  sum(pow(x - y, 2.0))
  def doubleEqual(x: VD, y: VD, eps: Double): Boolean = (x - y).toArray.forall(_ <= eps)

  object NewtonMethod{
    /*
    * Hesse Matrix shoukd be positive definite.
    * */
    def optimize(g: VD => VD, h: VD => MD, x: VD, alpha: Double, maxit: Int = 200, eps: Double) = {
      @tailrec
      def f(value: VD, loop: Int = 0, diffs: Double = Double.MaxValue): VD = {
        val next = value - h(value).t * g(value) * alpha
        val diff = norm2(value, next)
        if(diff > diffs) sys.error("The solution won't converge.")
        else if(doubleEqual(next, value, eps) || loop >= maxit) next
        else f(next, loop + 1, diff)
      }
      f(x)
    }
    

  }

}
