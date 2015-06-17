package alignment

case class SWG(gi:Int, ge:Int, mm:Int, mt:Int)
  extends AlignmentTools{
  require(ge < 0)
  require(gi < ge)
  require(mm < mt)

  val MIN = -1000000000

  import Direction._

  def apply(seq1:Array[AminoAcid], seq2:Array[AminoAcid],mode:String = "production"):Double = {
    val n = seq1.length
    val m = seq2.length
    
    val D = Array.ofDim[Int](n+1, m+1)
    val P = Array.ofDim[Int](n+1, m+1)
    val Q = Array.ofDim[Int](n+1, m+1)
    val F = Array.ofDim[Direction](n+1, m+1)
    
    var MAX = 0
    var MAXI = 0
    var MAXJ = 0

    for (i <- 0 to m) {
      D(0)(i) = 0
      Q(0)(i) = MIN
      P(0)(i) = MIN
      F(0)(i) = Terminal
    }
    
    for (i <- 1 to n) {
      D(i)(0) = 0
      Q(i)(0) = MIN
      P(i)(0) = MIN
      F(i)(0) = Terminal
    }

    for (i <- 1 to n; j <- 1 to m) {
      P(i)(j) = max(P(i-1)(j) + ge, D(i-1)(j) + gi)
      Q(i)(j) = max(Q(i)(j-1) + ge, D(i)(j-1) + gi)
      val pair =
        if (seq1(i-1) == seq2(j-1))
          Array(0, P(i-1)(j), Q(i)(j-1), D(i-1)(j-1) + mt) zip Array(Terminal, Down, Slide, Match)
        else
          Array(0, P(i-1)(j), Q(i)(j-1), D(i-1)(j-1) + mm) zip Array(Terminal, Down, Slide, MissMatch)
      val tmp = pair.maxBy(x => x._1)
      D(i)(j) = tmp._1
      F(i)(j) = tmp._2
      if (tmp._1 >= MAX) {
        MAX = tmp._1
        MAXI = i
        MAXJ = j
      }
    }
    if(mode == "test") {
      println("D")
      D.foreach(x => println(x.mkString("\t")))
      println("P")
      P.foreach(x => println(x.mkString("\t")))
      println("Q")
      Q.foreach(x => println(x.mkString("\t")))
      println("F")
      F.foreach(x => println(x.mkString("\t")))
    }
    MAX.toDouble * 2 / (n + m)
  }

  protected def max(x:Int,y:Int) = if(x > y) x else y

}

object SWG{
  def default = new SWG(-10,-1,-10,15)
}

abstract class AlignmentTools{
  def gi:Int
  def ge:Int
  def mm:Int
  def mt:Int
}

//case class AlignmentResult(seq1:Array[AminoAcid],seq2:Array[AminoAcid],score:Int)

sealed abstract class Direction

object Direction {

  case object Slide extends Direction

  case object Down extends Direction

  case object Match extends Direction

  case object MissMatch extends Direction

  case object Terminal extends Direction
}

