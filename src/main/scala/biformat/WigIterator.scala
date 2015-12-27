package biformat

import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import biformat.WigIterator.{FixedStep, VariableStep, WigUnit}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.BufferedSource

/**
  * can be managed with functional programming order
  * @param its body of iterator
  */
final class WigIterator protected (val its: Iterator[WigUnit]) extends biformat.BlockIterator[WigUnit] {
  /**
    * concatenates adjacent Wigunit
    * @param n maximum length
    * @return concatenated WigUnits Iterator
    */
  override def merge(n: Int = 10000): WigIterator = new WigIterator(mergedIterator(n))

  /**
    * non safe opperation
    * @param x one WigUnit
    * @param y the other WigUnit
    * @return assemble of x and y
    */
  override def append(x: WigUnit, y: WigUnit): WigUnit = WigUnit.append(x,y)

  def filterWithBed(x:Iterable[BedLine]): WigIterator = new WigIterator(
    new Iterator[WigUnit] {

      val bit = x.iterator

      protected var bedBuf: Option[BedLine] = if (bit.hasNext) Some(bit.next()) else None

      protected var wigBuf: Option[WigUnit] = if (its.hasNext) Some(its.next()) else None

      protected var nextOne: Option[WigUnit] = gen()

      def next(): WigUnit = {
        if (!hasNext) sys.error("Nothing in next.")
        else {
          val tmp = nextOne.get
          nextOne = gen()
          tmp
        }
      }

      def hasNext: Boolean = nextOne.isDefined

      protected def gen(): Option[WigUnit] = {
        @tailrec
        def f(wigop: Option[WigUnit], bedop: Option[BedLine]): (Option[WigUnit], Option[WigUnit], Option[BedLine]) = {
          def nextb = if (bit.hasNext) Some(bit.next()) else None
          def nextw = if (its.hasNext) Some(its.next()) else None
          (wigop, bedop) match {
            case (None, _) => (None, None, None)
            case (_, None) => (None, None, None)
            case (Some(wig), Some(bed)) =>
              if (wig.chrom != bed.chr) f(wigop, nextb)
              else wig.interSection(bed) match {
                case None =>
                  if (wig.end <= bed.start) f(nextw, bedop) else f(wigop, nextb)
                case tmp =>
                  if (bed.end < wig.end) (tmp, wigop, nextb) else (tmp, nextw, bedop)
              }
          }
        }
        val (v1, v2, v3) = f(wigBuf, bedBuf)
        wigBuf = v2
        bedBuf = v3
        v1
      }
    }
  )

  def hist(size: Int = 100, max: Double = 1.0): Array[Int] =  {
    val vec = Array.fill[Int](size)(0)
    its.foreach{
      case VariableStep(_, _, lines) =>
        lines.foreach{case (_,x) => vec((x * size / max).toInt) += 1}
      case FixedStep(_, _, _, _, _) =>
        sys.error("not supported.")
    }
    vec
  }

}

object WigIterator {

  val DefaultSep = """\p{javaWhitespace}+"""

  /**
    * be managed in [[WigIterator]]
    */
  sealed trait WigUnit extends Block {
    type T
    def chrom: String
    def span: Int
    def lines: Array[T]
    def length = lines.length * span
    def marginalize(wing: Int): WigUnit
    def +(that: WigUnit): WigUnit
    def interSection(bed: BedLine): Option[WigUnit]
  }

  object WigUnit {
    def append(x: WigUnit, y: WigUnit) = x + y
  }

  /**
    * @param chrom chromosome name
    * @param span span paramter
    * @param lines all (Long, Double) pairs
    */

  case class VariableStep(chrom: String, span: Int, lines: Array[(Long, Double)]) extends WigUnit {
    type T = (Long, Double)
    def start = lines.head._1
    def end = lines.last._1 + span
    def marginalize(wing: Int) = {
      require(span == 1)
      @tailrec
      def f(xs: List[(Long, Double)],
            ys: ListBuffer[(Long, Double)],
            zs: List[(Long, Double)]): List[(Long, Double)] =
      {
        def lwlim = ys.head._1
        def uplim = lwlim + (2 * wing)
        if(ys.last._1 == uplim) f(xs, ys.tail, (lwlim + wing, ys.foldLeft(0.0){(n, x) => n + x._2}) :: zs)
        else if(xs.isEmpty) zs.reverse
        else if(xs.head._1 <= uplim) f(xs.tail, ys :+ xs.head, zs)
        else f(xs.tail, ListBuffer(xs.head), zs)
      }
      val tmp = f(lines.tail.toList, ListBuffer(lines.head), Nil)
      VariableStep(chrom, span, tmp.toArray)
    }

    override def appendableWith(that: Block): Boolean = {
      that match {
        case VariableStep(ch, sp, _) => this.chrom == ch && this.span == sp
        case _ => false
      }
    }

    def +(that: WigUnit): WigUnit = {
      that match {
        case VariableStep(_,_,xs) => VariableStep(chrom, span, lines ++ xs)
        case _ => sys.error("VariableStep only.")
      }
    }

    def interSection(bed: BedLine): Option[VariableStep] = {
      if (end <= bed.start || start >= bed.end || chrom != bed.chr) None
      else
        Some(VariableStep(chrom, span,
          lines.dropWhile(_._1 < bed.start).takeWhile(_._1 < bed.end))
        )
    }
  }

  object VariableStep {
    def apply(line: String, sep: String = DefaultSep): VariableStep = {
      val p = line.split(sep)
      val chrom = p(1).split("=").last
      val span = if(p.length == 3) p(2).split("=").last.toInt else 1
      VariableStep(chrom, span, null)
    }
  }

  /**
    * Variable-Step type WigUnit
    * @param chrom chromosome name
    * @param start 0-origin start index
    * @param step step parameter
    * @param span span parameter
    * @param lines all Doubles
    */
  case class FixedStep(chrom: String, start: Long, step: Int, span: Int, lines: Array[Double]) extends WigUnit{
    type T = Double
    def end = start + length
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
        case _ => sys.error("FixedStep only.")
      }

    def interSection(bed: BedLine): Option[FixedStep] = {
      sys.error("not supported.")
      Some(this)
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
                  buf += Tuple2(p(0).toLong, p(1).toDouble)
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
