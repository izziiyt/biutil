case class SWG(gi:Int, ge:Int, mm:Int, mt:Int)
  extends AlignmentTools{
  
  import Direction._
  
  def apply(seq1:Array[AminoAcid], seq2:Array[AminoAcid], ouf:String):Int = {
    val n = seq1.length
    val m = seq2.length
    
    val D = Array.ofDim[Int](n+1, m+1)
    val P = Array.ofDim[Int](n+1, m+1)
    val Q = Array.ofDim[Int](n+1, m+1)
    val F = Array.ofDim[Direction](n, m)
    
    var MAX = 0
    var MAXI = 0
    var MAXJ = 0

    for (i <- 0 to m) {
      D(0)(i) = 0
      Q(0)(i) = Int.MinValue
      P(0)(i) = Int.MinValue
    }
    
    for (i <- 0 to n) {
      D(i)(0) = 0
      Q(i)(0) = Int.MinValue
      P(i)(0) = Int.MinValue
    }

    for (i <- 1 to n; j <- 1 to m) {
      P(i)(j) = max(P(i-1)(j) + ge, D(i-1)(j) + gi)
      Q(i)(j) = max(Q(i)(j-1) + ge, D(i)(j-1) + gi)
      val pair =
        if (seq1(i-1) == seq2(j-1)) Array(0, P(i-1)(j), Q(i)(j-1), D(i-1)(j-1)) zip Array(Invoke, Vertical, Parallel, Match)
        else Array(0, P(i-1)(j), Q(i)(j-1), D(i-1)(j-1)) zip Array(Invoke, Vertical, Parallel, MissMatch)

      val tmp = pair.maxBy(x => x._1) match {
        case (_, Invoke) => (0, Invoke)
        case (x, Match) => (x + mt, Match)
        case (x, MissMatch) => (x + mm, MissMatch)
        case (x, Vertical) => (x, Vertical)
        case (x, Parallel) => (x, Parallel)
      }
      
      D(i)(j) = tmp._1
      F(i-1)(j-1) = tmp._2
      
      if (D(i)(j) > MAX) {
        MAX = D(i)(j)
        MAXI = i-1
        MAXJ = j-1
      }
    }
    MAX
  }

  def max(x:Int,y:Int) = if(x > y) x else y

}

abstract class AlignmentTools{
  def gi:Int
  def ge:Int
  def mm:Int
  def mt:Int
}

//case class AlignmentResult(seq1:Array[AminoAcid],seq2:Array[AminoAcid],score:Int)

abstract class Direction

object Direction {

  case object Parallel extends Direction

  case object Vertical extends Direction

  case object Match extends Direction

  case object MissMatch extends Direction

  case object Invoke extends Direction
}

