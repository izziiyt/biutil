package biformat

import biformat.BlockIterator.{GenBlockIterator, MergedIterator}
import biformat.MafIterator.MafUnit
import scala.collection.mutable.ListBuffer
import alignment.Base
import scala.io.Source

/**
  * provides some method for manage Maf format files.
  * */

abstract class MafIterator extends BlockIterator[MafUnit]{
  def append(x: MafUnit, y: MafUnit): MafUnit = x + y
  def merged(_maxSize: Int, _its: BlockIterator[MafUnit]) = new MafIterator with MergedIterator[MafUnit] {
    val maxSize = _maxSize
    val its = _its
  }
  def merged(_maxSize: Int) = merged(_maxSize, this)
}

object MafIterator {

  implicit def toMafIterator(it: Iterator[MafUnit]): MafIterator = new MafIterator{
    def hasNext = it.hasNext
    def next() = it.next()
  }

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

  /**
    * @constructor
      * We recommend you to get Source by [[biformat.bigSource]] because ordinary maf-format files are big.
    * */
  def fromSource(s : Source, target: String, sep: String = """\p{javaWhitespace}+""") =
    new MafIterator with GenBlockIterator[MafUnit] {

    val lines = s.getLines()

    protected var nextOne: Option[MafUnit] = None

    def gen(): Option[MafUnit] = {
      val buf = new ListBuffer[MafLine]()
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
      else None
    }
  }
}
