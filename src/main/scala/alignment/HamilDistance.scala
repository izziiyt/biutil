package alignment

case class HamilDistance(mt:Int, mm:Int){
  import Direction._
  require(mt > mm)

  val MIN = -1000000000

  def apply(seq1:Array[AminoAcid], seq2:Array[AminoAcid],mode:String = "production"):Int = {
    val n = seq1.length
    val m = seq2.length

    val D = Array.ofDim[Int](n+1, m+1)
    val F = Array.ofDim[Direction](n+1, m+1)

    var MAX = 0
    var MAXI = 0
    var MAXJ = 0

    for (i <- 0 to m) {
      D(0)(i) = 0
      F(0)(i) = Terminal
    }

    for (i <- 1 to n) {
      D(i)(0) = 0
      F(i)(0) = Terminal
    }

    for (i <- 1 to n; j <- 1 to m) {
      val tmp = {
        if (seq1(i - 1) == seq2(j - 1)) {
          val scr = D(i - 1)(j - 1) + mt
          if (scr > 0) (scr, Match) else (0, Terminal)
        }
        else {
          val scr = D(i - 1)(j - 1) + mm
          if (scr > 0) (scr, MissMatch) else (0, Terminal)
        }
      }
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
      println("F")
      F.foreach(x => println(x.mkString("\t")))
    }
    MAX
  }

  protected def max(x:Int,y:Int) = if(x > y) x else y

//case class AlignmentResult(seq1:Array[AminoAcid],seq2:Array[AminoAcid],score:Int)

  sealed abstract class Direction

  object Direction {

    case object Match extends Direction

    case object MissMatch extends Direction

    case object Terminal extends Direction
  }
}
