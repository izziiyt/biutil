package biformat

import biformat.BedIterator.BedLine
import biformat.BlockIterator.{MergedIterator, GenBlockIterator}
import biformat.WigIterator.{FixedStep, VariableStep, WigUnit}
import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.Source

abstract class WigIterator extends BlockIterator[WigUnit]{

  /**
    * non safe opperation
    * @param x one WigUnit
    * @param y the other WigUnit
    * @return assemble of x and y
    */
  def append(x: WigUnit, y: WigUnit): WigUnit = WigUnit.append(x,y)

  def merged(_maxSize: Int) = new {
    val maxSize = _maxSize
    val its = this
  } with WigIterator with MergedIterator[WigUnit]

  def filterWithBed(bit: Iterator[BedLine])(implicit wit: WigIterator = this): WigIterator =
    new WigIterator with GenBlockIterator[WigUnit]{

    protected var bedBuf: Option[BedLine] = if (bit.hasNext) Some(bit.next()) else None

    protected var wigBuf: Option[WigUnit] = if (wit.hasNext) Some(wit.next()) else None

    protected var nextOne: Option[WigUnit] = None

    /*def next(): WigUnit = {
      if (!hasNext) throw new NoSuchElementException
      else {
        val tmp = nextOne.get
        nextOne = gen()
        tmp
      }
    }

    def hasNext: Boolean = nextOne.isDefined
*/
    protected def gen(): Option[WigUnit] = {
      @tailrec
      def f(wigop: Option[WigUnit], bedop: Option[BedLine]): (Option[WigUnit], Option[WigUnit], Option[BedLine]) = {
        def nextb = if (bit.hasNext) Some(bit.next()) else None
        def nextw = if (wit.hasNext) Some(wit.next()) else None
        (wigop, bedop) match {
          case (Some(wig), Some(bed)) =>
            if (wig.chrom != bed.chr) f(wigop, nextb)
            else wig.interSection(bed) match {
              case None =>
                if (wig.end <= bed.start) f(nextw, bedop) else f(wigop, nextb)
              case tmp =>
                if (bed.end < wig.end) (tmp, wigop, nextb) else (tmp, nextw, bedop)
            }
          case (None, _) | (_, None) => (None, None, None)
        }
      }
      val (v1, v2, v3) = f(wigBuf, bedBuf)
      wigBuf = v2
      bedBuf = v3
      v1
    }
  }

  def hist(size: Int = 100, max: Double = 1.0): Array[Int] =  {
    val vec = Array.fill[Int](size)(0)
    this.foreach{
      case VariableStep(_, _, lines) =>
        lines.foreach{case (_,x) => vec((x * size / max).toInt) += 1}
      case FixedStep(_, _, _, _, _) =>
        throw new UnsupportedOperationException
    }
    vec
  }
}

object WigIterator {

  val DefaultSep = """\p{javaWhitespace}+"""

  implicit def toWigIteretor(it: Iterator[WigUnit]): WigIterator = new WigIterator {
    override def next(): WigUnit = it.next()
    override def hasNext: Boolean = it.hasNext
  }

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
            zs: List[(Long, Double)]): List[(Long, Double)] = {
        def lwlim = ys.head._1
        def uplim = lwlim + (2 * wing)
        if (ys.last._1 == uplim) f(xs, ys.tail, (lwlim + wing, ys.foldLeft(0.0) { (n, x) => n + x._2 }) :: zs)
        else if (xs.isEmpty) zs.reverse
        else if (xs.head._1 <= uplim) f(xs.tail, ys :+ xs.head, zs)
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
        case VariableStep(_, _, xs) => VariableStep(chrom, span, lines ++ xs)
        case _ => throw new UnsupportedOperationException
      }
    }

    def interSection(bed: BedLine): Option[VariableStep] = {
      if (end <= bed.start || start >= bed.end || chrom != bed.chr) None
      else {
        try {
          Some(VariableStep(chrom, span,
            lines.dropWhile(_._1 < bed.start).takeWhile(_._1 < bed.end))
          )
        }
        catch{
          case e:java.util.NoSuchElementException =>
            println(end + " " + bed.start + " " + start + " " + bed.end + chrom + " " + bed.chr)
            e.printStackTrace()
            None
        }
      }
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
        case _ => throw new UnsupportedOperationException
      }

    def interSection(bed: BedLine): Option[FixedStep] = {
      throw new UnsupportedOperationException
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

  def fromSource(s: Source, maxsize: Int = 2048, sep: String = DefaultSep) =
    new WigIterator with GenBlockIterator[WigUnit]{

    protected val lines = s.getLines()

    protected var nextunit: WigUnit = null

    protected var nextOne: Option[WigUnit] = None

    protected def gen(): Option[WigUnit] = nextunit match {
      case VariableStep(chrom, span, _) =>
        val buf = new ArrayBuffer[(Long, Double)]()
        for (line <- lines; if line.nonEmpty && !line.startsWith("#"); p = line.split(sep)) {
          p(0) match {
            case "fixedStep" =>
              nextunit = FixedStep(line)
              if(buf.nonEmpty) return Some(VariableStep(chrom, span, buf.toArray))
            case "variableStep" =>
              nextunit = VariableStep(line)
              if(buf.nonEmpty) return Some(VariableStep(chrom, span, buf.toArray))
            case _ =>
              buf += Tuple2(p(0).toLong, p(1).toDouble)
              if(buf.length >= maxsize) return Some(VariableStep(chrom, span, buf.toArray))
          }
        }
        if (buf.nonEmpty) Some(VariableStep(chrom, span, buf.toArray))
        else None
      case FixedStep(chrom, start, step, span, _) =>
        throw new UnsupportedOperationException
      case _ =>
        val line = lines.find(line => line.startsWith("fixed") || line.startsWith("variable"))
        if(line.isEmpty) sys.error("Input file may not be biformat.maf.wig format.")
        nextunit = line.get.split(sep)(0) match {
          case "fixedStep" => FixedStep(line.get)
          case "variableStep" => VariableStep(line.get)
        }
        gen()
    }
  }
}
