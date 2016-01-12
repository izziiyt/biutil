package biformat

import java.util.NoSuchElementException

import scala.io.Source

final class BedIterator(val its: Iterator[BedLine]) extends BlockIterator[BedLine] {
  def merge(n: Int): BedIterator = new BedIterator(mergedIterator(n))
  def append(x: BedLine, y: BedLine) = throw new UnsupportedOperationException
}

object BedIterator {
  val DefaultSep = """\p{javaWhitespace}+"""

  def fromSource(s: Source, sep: String = DefaultSep) = new BedIterator (
    new Iterator[BedLine] {

      val lines = s.getLines()

      protected var nextOne: Option[BedLine] = gen()

      def hasNext: Boolean = nextOne.isDefined

      def next(): BedLine = {
        if (!hasNext) throw new NoSuchElementException
        else {
          val tmp = nextOne.get
          nextOne = gen()
          tmp
        }
      }

      protected def gen(): Option[BedLine] = {
        for(line <- lines; if !line.startsWith("#") && line.nonEmpty){
          return Some(BedLine(line, DefaultSep))
        }
        None
      }

    })
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