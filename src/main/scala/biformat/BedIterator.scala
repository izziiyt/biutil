package biformat

import biformat.BedIterator.BedLine
import biformat.BlockIterator.{GenBlockIterator, MergedIterator,FilteredBlockIterator}
import breeze.linalg.{max, min}
import scala.io.Source

abstract class BedIterator extends BlockIterator[BedLine]{
  def append(x: BedLine, y: BedLine) = throw new UnsupportedOperationException
  protected def merged(_maxSize: Int, _its: BlockIterator[BedLine]) = new BedIterator with MergedIterator[BedLine]{
    val maxSize = _maxSize
    val its = _its
  }
  def merged(_maxSize: Int) = merged(_maxSize, this)
  def intersection(that: BedIterator): BedIterator = new BedIterator with FilteredBlockIterator[BedLine,BedLine]{
    val bit = that
    val wit = this
  }
  def union(that: BedIterator): BedIterator = BedIterator.union(this, that)

}

object BedIterator {

  implicit def toBedIterator(it: Iterator[BedLine]): BedIterator = new BedIterator {
    override def next(): BedLine = it.next()
    override def hasNext: Boolean = it.hasNext
  }

  /**
    *
    * @param bit1 filtered with bit2
    * @param bit2 filters bit1
    * @return bit1 filtered with bit2
    */
  /*protected def intersection(bit1: BedIterator, bit2: BedIterator): BedIterator =
    new BedIterator with GenBlockIterator[BedLine]{

      protected var b1Buf: Option[BedLine] = if (bit1.hasNext) Some(bit1.next()) else None
      protected var b2Buf: Option[BedLine] = if (bit2.hasNext) Some(bit2.next()) else None

      protected def gen(): Option[BedLine] = {
        @tailrec
        def f(b1op: Option[BedLine], b2op: Option[BedLine]): (Option[BedLine], Option[BedLine], Option[BedLine]) = {
          def nextb1() = if (bit1.hasNext) Some(bit1.next()) else None
          def nextb2() = if (bit2.hasNext) Some(bit2.next()) else None
          (b1op, b2op) match {
            case (Some(bed1), Some(bed2)) =>
              if (!bed1.hasIntersection(bed2)) {
                if(bed1 > bed2) f(b1op, nextb2()) else f(nextb1(), b2op)
              }
              else{
                val tmp = bed1.intersection(bed2)
                if (bed1.end > bed2.end) (Some(tmp), b1op, nextb2()) else (Some(tmp), nextb1(), b2op)
              }
            case (None, _) | (_, None) =>
              (None, None, None)
          }
        }
        val (v1, v2, v3) = f(b1Buf, b2Buf)
        b1Buf = v2
        b2Buf = v3
        v1
      }
    }*/

  protected def union(bit1: BedIterator, bit2: BedIterator): BedIterator =
    new BedIterator with GenBlockIterator[BedLine]{

      protected var b1Buf: Option[BedLine] = if (bit1.hasNext) Some(bit1.next()) else None
      protected var b2Buf: Option[BedLine] = if (bit2.hasNext) Some(bit2.next()) else None

      protected def gen(): Option[BedLine] = {
        def f(b1op: Option[BedLine], b2op: Option[BedLine]): (Option[BedLine], Option[BedLine], Option[BedLine]) = {
          def nextb1() = if (bit1.hasNext) Some(bit1.next()) else None
          def nextb2() = if (bit2.hasNext) Some(bit2.next()) else None
          (b1op, b2op) match {
            case (Some(bed1), Some(bed2)) =>
              if (!bed1.hasIntersection(bed2)) {
                if(bed1 > bed2) (Some(bed2), b1op, nextb2()) else (Some(bed1), nextb1(), b2op)
              }
              else{
                (Some(bed1.union(bed2)), nextb1(), nextb2())
              }
            case _ => (None, None, None)
          }
        }
        val (v1, v2, v3) = f(b1Buf, b2Buf)
        b1Buf = v2
        b2Buf = v3
        v1
      }
    }

  val DefaultSep = """\p{javaWhitespace}+"""

  def fromSource(s: Source, sep: String = DefaultSep) =
    new BedIterator with GenBlockIterator[BedLine] {

      val lines = s.getLines()

      protected def gen(): Option[BedLine] = {
        for(line <- lines; if !line.startsWith("#") && line.nonEmpty){
          return Some(BedLine(line, DefaultSep))
        }
        None
      }
    }

  /**
    *
    * @param chr chromosome name
    * @param start starting index(0-origin)
    * @param end end index(0-origin
    * @param name
    * @param score 0 <= _ => 1000
    * @param strand + or -
    * @param blockCount number of exons
    *
    */

  case class BedLine(chr: String, start: Int, end: Int, name: String = "", score: Int = 0, strand: Char = '.',
                     thickStart: Int = 0, thickEnd: Int = 0, itemRgb: (Int, Int, Int) =
                     (0,0,0) , blockCount: Int = 0,
                     blockSize: Array[Int] = Array.empty, blockStarts: Array[Int] = Array.empty) extends Block {

    def length = end - start
    def appendableWith(that: Block): Boolean = false
    override def toString = {
      val tmp = chr + '\t' + start + '\t' + end + '\t' + name + '\t' + score + '\t' + strand + '\t' +
        thickStart + '\t' + thickEnd + '\t' + itemRgb._1 + ',' + itemRgb._2 + ',' + itemRgb._3 + ',' + '\t' +
        blockCount
      if(blockSize.nonEmpty) tmp + '\t' + blockSize.mkString(",") + '\t' + blockStarts.mkString(",")
      else tmp
    }

    def interSection[T <: Block](that:T) = that match {
      case BedLine(_, _start, _end, _,_,_,_,_,_,_,_,_) if this.hasIntersection(that) =>
        Some(BedLine(chr, max(start, _start), min(end, _end), name, score, strand,
          thickStart, thickEnd, itemRgb, blockCount, blockSize, blockStarts).asInstanceOf[this.type])
      case _ => None
    }

    def union(that:BedLine): BedLine = {
      BedLine(this.chr, min(this.start, that.start), max(this.end, that.end))
    }

  }


  object BedLine {

    def apply(line: String, sep: String): BedLine = {
      val ps = line.split(sep)
      if(ps.length == 12){
        val rgbbase = ps(8).split(',').map(_.toInt)
        BedLine(ps(0), ps(1).toInt, ps(2).toInt, ps(3), ps(4).toInt, ps(5).head, ps(6).toInt, ps(7).toInt,
          (rgbbase(0), rgbbase(1),rgbbase(2)), ps(9).toInt, ps(10).split(',').map(_.toInt),
          ps(11).split(',').map(_.toInt))
      }
      else
        BedLine(ps(0), ps(1).toInt, ps(2).toInt)
    }
  }
}

