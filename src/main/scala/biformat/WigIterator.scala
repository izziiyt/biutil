package biformat

import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import WigIterator.WigUnit
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource

final class WigIterator protected (val its: Iterator[WigUnit]) extends biformat.BlockIterator[WigUnit] {

  override def merge: WigIterator = new WigIterator(mergedIterator)

  override def append(x: WigUnit, y: WigUnit): WigUnit = WigUnit.append(x,y)

  /*def merge: WigIterator = new WigIterator (
    new Iterator[WigUnit] {

      val buf = new ArrayBuffer[Double]()
      var tmpChr = ""
      var tmpLast = Long.MinValue
      var tmpStart = Long.MinValue

      protected var nextOne: Option[WigUnit] = gen()

      def next(): WigUnit = {
        if (!hasNext) sys.error("Nothing in next.")
        else {
          val tmp = nextOne.get
          nextOne = gen()
          tmp
        }
      }

      def hasNext = nextOne.isDefined

      protected def gen(): Option[WigUnit] = {
        for(unit <- its) {
          unit match {
            case VariableStep(_, _, _) =>
              sys.error("merge doesn't support VariableStep.")
            case FixedStep(chrom, start, step, span, lines) =>
              if(tmpChr == chrom && tmpLast == start) {
                tmpLast += unit.length
                buf ++= lines.flatMap(Array.fill(span)(_))
              }
              else if(tmpChr.nonEmpty || tmpLast != Long.MinValue){
                val tmp = Some(FixedStep(tmpChr, tmpStart, 1, 1, buf.toArray))
                tmpStart = start
                tmpLast = start + unit.length
                buf.clear()
                tmpChr = chrom
                return tmp
              }
              else{
                tmpStart = start
                tmpChr = chrom
                tmpLast = start + unit.length
                buf ++= lines.flatMap(Array.fill(span)(_))
              }
          }
        }
        if(buf.nonEmpty) {
          val tmp = Some(FixedStep(tmpChr, tmpStart, 1, 1, buf.toArray))
          buf.clear()
          tmp
        }
        else None
      }
    }
  )*/
}

object WigIterator {

  val DefaultSep = """\p{javaWhitespace}+"""

  sealed trait WigUnit extends Block {
    type T
    def chrom: String
    def span: Int
    def lines: Array[T]
    def length = lines.length * span
    def marginalize(wing: Int): WigUnit
    def +(that: WigUnit): WigUnit
  }

  object WigUnit {
    def append(x: WigUnit, y: WigUnit) = x + y
  }

  case class VariableStep(chrom: String, span: Int, lines: Array[(Long, Double)]) extends WigUnit {
    lazy val start = lines.head._1
    lazy val end = lines.last._1 + span
    type T = (Long, Double)
    override lazy val length = super.length
    def marginalize(wing: Int) = this
    override def appendableWith(that: Block): Boolean = {
      that match {
        case x: VariableStep => false
        case _ => false
      }
    }

    def +(that: WigUnit): WigUnit = sys.error("sorry")
  }

  object VariableStep {
    def apply(line: String, sep: String = DefaultSep): VariableStep = {
      val p = line.split(sep)
      val chrom = p(1).split("=").last
      val span = if(p.length == 3) p(2).split("=").last.toInt else 1
      VariableStep(chrom, span, null)
    }
  }

  case class FixedStep(chrom: String, start: Long, step: Int, span: Int, lines: Array[Double]) extends WigUnit{
    type T = Double
    lazy val end = start + length
    override lazy val length = super.length
    def marginalize(wing: Int): FixedStep = {
      require(step == 1 && span == 1)
      @tailrec
      def f(headi: Int, lasti: Int, sum: Double,
            result: ArrayBuffer[Double] = new ArrayBuffer[Double]()): ArrayBuffer[Double] = {
        if(lasti >= lines.length) result :+ sum
        else f(headi + 1, lasti + 1, sum - lines(headi) + lines(lasti), result :+ sum)
      }
      val init = (0 to 2 * wing).map(lines(_)).sum
      val tmp = f(0, 2 * wing + 1, init).toArray
      FixedStep(chrom,start + wing, 1, 1, tmp)
    }
    def appendableWith(that: Block): Boolean = {
      that match {
        case FixedStep(x,y,z,w,_) => this.end == y && this.chrom == x
        case _ => false
      }
    }

    def +(that: WigUnit): WigUnit =
      that match {
        case FixedStep(_,_,_,_,ys) => FixedStep(chrom, start, step, span, lines ++ ys)
        case _ => sys.error("sorry")
      }
  }

  object FixedStep {
    def apply(line: String, sep: String = DefaultSep): FixedStep = {
      val p = line.split(sep)
      val chrom = p(1).split("=").last
      val start = p(2).split("=").last.toLong
      val step = p(3).split("=").last.toInt
      val span = if(p.length == 3) p(2).split("=").last.toInt else 1
      FixedStep(chrom, start, step, span, null)
    }
  }

  def fromFile[T](f: String, sep: String = DefaultSep) = new WigIterator (
    new Iterator[WigUnit] {
      val s = new BufferedSource(
        if (f.endsWith(".gz")) new GZIPInputStream(new FileInputStream(f), 1024 * 1024)
        else new FileInputStream(f)
        , 1024 * 1024
      )

      if (s.isEmpty) sys.error("we can't find path to " + f)

      val lines = s.getLines()

      protected var nextunit: WigUnit = null

      // define first unit's step mode
      val line = lines.find(line => line.startsWith("fixed") || line.startsWith("variable"))
      if(line.isEmpty) sys.error("Input file may not be biformat.maf.wig format.")
      val p = line.get.split(sep)
      p(0) match {
        case "fixedStep" =>
          nextunit = FixedStep(line.get)
        case "variableStep" =>
          nextunit = VariableStep(line.get)
      }

      protected var nextOne: Option[WigUnit] = gen()

      def hasNext: Boolean = nextOne.isDefined

      def next(): WigUnit = {
        if (!hasNext) sys.error("Nothing in next.")
        else {
          val tmp = nextOne.get
          nextOne = gen()
          tmp
        }
      }

      protected def gen(): Option[WigUnit] =
        nextunit match {
          case VariableStep(chrom, span, _) =>
            val buf = new ArrayBuffer[(Long, Double)]()
            for (line <- lines; if line.nonEmpty && !line.startsWith("#"); p = line.split(sep)) {
              p(0) match {
                case "fixedStep" =>
                  nextunit = FixedStep(line)
                  VariableStep(chrom, span, buf.toArray)
                case "variableStep" =>
                  nextunit = VariableStep(line)
                  VariableStep(chrom, span, buf.toArray)
                case _ =>
                  buf += Pair(p(0).toLong, p(1).toDouble)
              }
            }
            if (buf.nonEmpty) Some(VariableStep(chrom, span, buf.toArray))
            else {
              s.close()
              None
            }
          case FixedStep(chrom, start, step, span, _) =>
            val buf = new ArrayBuffer[Double]()
            for (line <- lines; if line.nonEmpty && !line.startsWith("#"); p = line.split(sep)) {
              p(0) match {
                case "fixedStep" =>
                  nextunit = FixedStep(line)
                  val tmp = FixedStep(chrom, start, step, span, buf.toArray)
                  buf.clear()
                  return Some(tmp)
                case "variableStep" =>
                  nextunit = VariableStep(line)
                  val tmp = FixedStep(chrom, start, step, span, buf.toArray)
                  buf.clear()
                  return Some(tmp)
                case _ =>
                  buf += p(0).toDouble
              }
            }
            if (buf.nonEmpty) Some(FixedStep(chrom, start, step, span, buf.toArray))
            else {
              s.close()
              None
            }
        }
    })


}
