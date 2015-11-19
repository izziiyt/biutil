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

  /*def convert(mf: String, spmf: String, per: Int, species: Int): Unit = {
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
  }*/

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

  case class MafUnit(lines: Map[String, MafLine]) {
    lazy val length = lines.values.head.length

    require(lines.values.forall(_.length == length))

    def start(target: String) = lines(target).start

    /**
      * Please check whether appendable before use.
      * */
    def +(that: MafUnit): MafUnit = {
      val newlines = scala.collection.mutable.Map.empty[String, MafLine]
      that.lines.iterator.foreach{
        case (name, value)  =>
          this.lines.get(name) match {
            case None =>
              newlines += name -> MafLine(name, value.subname, value.start, value.strand,
                Array.fill(this.length)(Base.N) ++ value.seq)
            case _ =>
              newlines += name -> (this.lines(name) + value)
          }
      }
      this.lines.iterator.foreach{
        case (name, value)  =>
          that.lines.get(name) match {
            case None =>
              newlines += name -> MafLine(name, value.subname, value.start, value.strand,
                 value.seq ++ Array.fill(that.length)(Base.N))
            case _ =>
              Unit
          }
      }
      MafUnit(newlines.toMap)
    }

    def appendable(that: MafUnit, target: String): Boolean = lines(target).appendableWith(that.lines(target))

    def sliceAt(n: Int): Pair[MafUnit,MafUnit] = {
      val (a, b) = lines.iterator.map {
        case (x, y) =>
          val tmp = y.splitAt(n)
          Pair(x -> tmp._1, x -> tmp._2)
      }.toList.unzip
      (MafUnit(a.toMap), MafUnit(b.toMap))
    }

    def seqs: List[Array[Base]] = lines.valuesIterator.map(_.seq).toList
  }

  object MafUnit {
    def apply(lines: Iterable[MafLine]): MafUnit = MafUnit(lines.map(x => x.name -> x).toMap)
  }

  final class MafIterator protected (val its: Iterator[MafUnit]) extends Iterable[MafUnit] with TraversableOnce[MafUnit] {
    def iterator: Iterator[MafUnit] = its
  }

  case class MafLine(name: String, subname: String, start: Long, strand: String, seq: Array[Base]) {
    lazy val length = seq.length
    lazy val end = start + length
    def appendableWith(that: MafLine): Boolean =
      this.name == that.name && this.subname == that.subname && this.end == that.start && this.strand == that.strand
    def +(that: MafLine) = MafLine(name, subname, start, strand, seq ++ that.seq)
    def drop(n: Int): MafLine = MafLine(name,subname,start,strand,seq.drop(n))
    def take(n: Int): MafLine = MafLine(name,subname,start + n,strand,seq.take(n))
    def splitAt(n: Int): Pair[MafLine, MafLine] = {
      val (x, y) = seq.splitAt(n)
      (MafLine(name, subname, start, strand, x), MafLine(name, subname, start + n, strand, y))
    }
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
              return Some(MafUnit(buf.toList))
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