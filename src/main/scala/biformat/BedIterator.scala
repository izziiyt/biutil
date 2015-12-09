package biformat

import scala.io.Source

final class BedIterator(val its: Iterator[BedLine]) extends BlockIterator[BedLine] {
  def merge(n: Int): BedIterator = new BedIterator(mergedIterator(n))
  def append(x: BedLine, y: BedLine) = sys.error("BedLine can't be appendable.")
}

object BedIterator {
  val DefaultSep = """\p{javaWhitespace}+"""

  def fromSource(s: Source, sep: String = DefaultSep) = new BedIterator (
    new Iterator[BedLine] {

      val lines = s.getLines()

      protected var nextOne: Option[BedLine] = gen()

      def hasNext: Boolean = nextOne.isDefined

      def next(): BedLine = {
        if (!hasNext) sys.error("Nothing in next.")
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
case class BedLine(chr: String, start: Long, end: Long, score: Double) extends Block {
  def length = (end - start).toInt
  def appendableWith(that: Block): Boolean = false
}

object BedLine {
  def apply(line: String, sep: String): BedLine = {
    val ps = line.split(sep)
    BedLine(ps(0), ps(1).toLong, ps(2).toLong, ps(4).toDouble)
  }
}