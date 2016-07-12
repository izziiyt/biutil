package biformat

import biformat.BedIterator.BedLine
import biformat.BlockIterator.{GenBlockIterator, MergedIterator}
import scala.io.Source

abstract class BedIterator extends BlockIterator[BedLine]{
  def append(x: BedLine, y: BedLine) = throw new UnsupportedOperationException
  protected def merged(_maxSize: Int, _its: BlockIterator[BedLine]) = new BedIterator with MergedIterator[BedLine]{
    val maxSize = _maxSize
    val its = _its
  }
  def merged(_maxSize: Int) = merged(_maxSize, this)
}

object BedIterator {

  implicit def toBedIterator(it: Iterator[BedLine]): BedIterator = new BedIterator {
    override def next(): BedLine = it.next()
    override def hasNext: Boolean = it.hasNext
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

    def hasOverlap(that:BedLine): Boolean = this.start < that.end && this.end > that.start
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

