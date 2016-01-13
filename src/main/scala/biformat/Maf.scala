package biformat

import java.util.NoSuchElementException

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import alignment.Base
import scala.io.Source

/**
  * provides some method for manage Maf format files.
  * */
object Maf {
  @deprecated
  def readMaf(mf: String, per: Int = 512): Array[List[Array[Base]]] = {
    val it = MafIterator.fromMSA(Source.fromFile(mf), "hg19")
    val totalunit = it.reduceLeft { (n, u) => n + u }
    val bases = totalunit.seqs
    val tmp = div(bases, per)
    tmp
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
}

/**
  * a component appeared in .maf-fomat files.
  * iterably managed by MafIterator [[MafIterator]]
  * @param lines species -> [[alignment.Base]] array
  * @param target target species
  */
case class MafUnit(lines: Map[String, MafLine], target: String) extends Block {
  val length = lines.values.head.length

  require(lines.values.forall(_.length == length))

  def start = lines(target).start
  def end = lines(target).end

  /**
    * @return MafUnit with lines with target's [[alignment.Base.D]] removed.
    */
  def Dremoved: MafUnit = {
    val indices = lines(target).seq.zipWithIndex.filter{case (b, i) => b != Base.D}.unzip._2
    def f(ml: MafLine): MafLine =
      MafLine(ml.name, ml.subname, ml.start, ml.num, ml.strand, indices.map(ml.seq(_)).toArray)
    val newlines = lines.iterator.map{case (k,v) => k -> f(v)}.toMap
    MafUnit(newlines, target)
  }

  /**
    * Please check whether [[appendableWith]] returns true before use.
    * @param that next MafUnit
    * @return concatenated MafUnit
    */
  def +(that: MafUnit): MafUnit = {
    val newlines = scala.collection.mutable.Map.empty[String, MafLine]
    that.lines.iterator.foreach{
      case (name, value)  =>
        this.lines.get(name) match {
          case None =>
            newlines += name -> MafLine(name, value.subname, value.start, value.num, value.strand,
              Array.fill(this.length)(Base.D) ++ value.seq)
          case _ =>
            newlines += name -> (this.lines(name) + value)
        }
    }
    this.lines.iterator.foreach{
      case (name, value)  =>
        that.lines.get(name) match {
          case None =>
            newlines += name -> MafLine(name, value.subname, value.start, value.num, value.strand,
              value.seq ++ Array.fill(that.length)(Base.D))
          case _ =>
            Unit
        }
    }
    MafUnit(newlines.toMap, target)
  }
  /**
    * @param that next MafUnit
    * @return whether this and that are able to be concatenated
    */
  def appendableWith(that: Block): Boolean = that match {
    case MafUnit(xlines, xtarget) =>
      val a = lines(target)
      val b = xlines(target)
      a.subname == b.subname && a.end == b.start && a.strand == b.strand
    case _ => false
  }

  def seqs: List[Array[Base]] = lines.valuesIterator.map(_.seq).toList
}

/**
  * Factory of MafUnit
  */
object MafUnit {
  /**
    * @constructor
    * @param lines MafLines
    * @param target target species
    * @return
    */
  def apply(lines: Iterable[MafLine], target: String): MafUnit = MafUnit(lines.map(x => x.name -> x).toMap, target)
}

final class MafIterator protected (val its: Iterator[MafUnit], target: String) extends BlockIterator[MafUnit] {

  def merge(maxSize: Int = 10000) = new MafIterator(mergedIterator(maxSize), target)

  def append(x: MafUnit, y: MafUnit): MafUnit = x + y
}

/**
  *
  * @param name species
  * @param subname chromosome
  * @param start head index(0-origin) in chromosome
  * @param num # of nucleotides
  * @param strand '+' or '-'
  * @param seq Array[Base]
  */
case class MafLine(name: String, subname: String, start: Long, num: Long, strand: String, seq: Array[Base]) {
  def length = seq.length
  def end = start + num
  def +(that: MafLine) = MafLine(name, subname, start, num + that.num, strand, seq ++ that.seq)
}

object MafLine {
  /**
    *
    * @param xs strings separated by white spaces
    * @return
    */
  def fromString(xs: Seq[String]) = {
    val names = xs(1).split('.')
    MafLine(names(0), if (names.length == 2) names(1) else "", xs(2).toLong, xs(3).toLong, xs(4), xs(6).toCharArray.map(Base.fromChar))
  }
}

object MafIterator {
  /**
    * @constructor
      * We recommend you to get Source by [[biformat.bigSource]] because ordinary maf-format files are big.
    * */
  def fromMSA(s : Source, target: String, sep: String = """\p{javaWhitespace}+""") =
    new MafIterator (new Iterator[MafUnit] {

      val lines = s.getLines()

      protected var nextOne: Option[MafUnit] = nexti()

      def hasNext = nextOne.isDefined

      def next(): MafUnit = {
        if (!hasNext) throw new NoSuchElementException
        else {
          val tmp = nextOne.get
          nextOne = nexti()
          tmp
        }
      }
      def nexti(): Option[MafUnit] = {
        val buf = new ListBuffer[MafLine]()
        if (s.isEmpty) return None
        for (line <- lines; if line.nonEmpty && !line.startsWith("#"); p = line.split(sep)) {
          p(0) match {
            case "s" =>
              buf += MafLine.fromString(p)
            case "a" if buf.nonEmpty =>
              return Some(MafUnit(buf.toList, target))
            case _ =>
          }
        }
        if (buf.nonEmpty) Some(MafUnit(buf.toList, target))
        else {
          s.close()
          None
        }
      }
    }
      , target
    )
}
