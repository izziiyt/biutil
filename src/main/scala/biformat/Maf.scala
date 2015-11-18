package biformat

import java.io._
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource
import alignment.Base

object Maf {
  def readMaf(mf: String, per: Int = 512): Array[List[Array[Base]]] = {
    val it = MafIterator.fromMSA(mf)
    val totalunit = it.reduceLeft { (n, u) => n + u }
    val bases = totalunit.seqs
    val tmp = div(bases, per)
    tmp
  }

  def convert(mf: String, spmf: String, per: Int, species: Int): Unit = {
    val it = MafIterator.fromMSA(mf).iterator
    val w = new PrintWriter(spmf)
    var tmp = it.next()
    while (it.hasNext) {
      tmp = tmp + it.next()
      while (tmp.length > per) {
        val (x, y) = tmp.sliceAt(per)
        w.println(x.seqs.map(z => z.mkString("")).reduce(_ + "," + _))
        tmp = y
      }
    }
    if (tmp.length > 0) w.println(tmp.seqs.map(z => z.mkString("")).reduce(_ + "," + _))
    w.close()
  }

  def div(seqs: List[Array[Base]], size: Int): Array[List[Array[Base]]] = {
    @tailrec
    def f(xs: List[Array[Base]], ys: List[List[Array[Base]]], index: Int): Array[List[Array[Base]]] = {
      if (xs.head.isEmpty) ys.reverse.toArray
      else {
        val (target, reserve) = xs.map { x => x.splitAt(index) }.unzip
        f(reserve, target :: ys, index)
      }
    }
    f(seqs, Nil, size)
  }

  case class MafUnit(lines: List[MafLine]) {
    require(lines.forall(_.length == lines.head.length))

    lazy val length = lines.head.length

    def +(that: MafUnit) = new MafUnit((lines, that.lines).zipped.map(_ ++ _))

    def sliceAt(n: Int) = (
      new MafUnit(lines.map(_.take(n))),
      new MafUnit(lines.map(_.drop(n)))
      )

    def seqs:List[Array[Base]] = lines.map(_.seq)
  }

  class MafIterator protected (val its: Iterator[MafUnit]) extends Iterable[MafUnit] with TraversableOnce[MafUnit] {
    def iterator: Iterator[MafUnit] = its
  }

  case class MafLine(name: String, subname: String, start: Long, strand: String, seq: Array[Base]) {
    lazy val length = seq.length
    def ++(that: MafLine) = MafLine(name, subname, Long.MinValue, "", seq ++ that.seq)
    def drop(n: Int) = MafLine(name,subname,start,strand,seq.drop(n))
    def take(n: Int) = MafLine(name,subname,start,strand,seq.take(n))
  }

  object MafLine {
    def fromString(xs: Seq[String]) = {
      val names = xs(1).split('.')
      MafLine(names(0), if (names.length == 2) names(1) else "", xs(2).toLong, xs(4), xs(6).toCharArray.map(Base.fromChar))
    }
  }

  object MafIterator {
    def fromMSA(f: String, sep: String = """\p{javaWhitespace}+""") = new MafIterator (new Iterator[MafUnit] {
      val s = new BufferedSource(
        if (f.endsWith(".gz")) new GZIPInputStream(new FileInputStream(f), 1024 * 1024)
        else new FileInputStream(f)
      , 1024 * 1024
      )
      val lines = s.getLines()

      protected var nextOne: Option[MafUnit] = nexti()

      def hasNext = nextOne.isDefined

      def next(): MafUnit = {
        if (!hasNext) sys.error("Nothing in next.")
        else {
          val tmp = nextOne.get
          nextOne = nexti()
          tmp
        }
      }

      def nexti(): Option[MafUnit] = {
        val buf = new ListBuffer[MafLine]()
        if (s.isEmpty) return None
        for (line <- lines; if line != "" && !line.startsWith("#"); p = line.split(sep)) {
          p(0) match {
            case "s" =>
              buf += MafLine.fromString(p)
            case "a" if buf.nonEmpty =>
              val tmp = Some(MafUnit(buf.toList))
              return tmp
            case _ => Unit
          }
        }
        if (buf.nonEmpty) Some(MafUnit(buf.toList))
        else {
          s.close()
          None
        }
      }
    }
    )
  }

}