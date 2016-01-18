package biformat

import biformat.BedIterator.BedLine
import biformat.BlockIterator.{GenBlockIterator, MergedIterator}

import scala.io.Source

abstract class BedIterator extends BlockIterator[BedLine]{
  def append(x: BedLine, y: BedLine) = throw new UnsupportedOperationException
  def merged(_maxSize: Int) = new {
    val maxSize = _maxSize
    val its = this
  } with BedIterator with MergedIterator[BedLine]
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

    protected var nextOne: Option[BedLine] = None

    protected def gen(): Option[BedLine] = {
      for(line <- lines; if !line.startsWith("#") && line.nonEmpty){
        return Some(BedLine(line, DefaultSep))
      }
      None
    }
  }

  case class BedLine(chr: String, start: Long, end: Long, name:String, score: Double, strand: Char) extends Block {
    def length = (end - start).toInt
    def appendableWith(that: Block): Boolean = false
    override def toString = chr + '\t' + start + '\t' + end + '\t' + name + '\t' + score.toInt + '\t' + strand
    def hasOverlap(that:BedLine): Boolean = this.start < that.end && this.end > that.start
  }

  object BedLine {
    def apply(line: String, sep: String): BedLine = {
      val ps = line.split(sep)
      BedLine(ps(0), ps(1).toLong, ps(2).toLong, ps(3), ps(4).toDouble, ps(5).head)
    }
  }
}

