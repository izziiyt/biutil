package biformat

import biformat.BedIterator.BedLine
import biformat.BlockIterator.{MergedIterator, GenBlockIterator}
import biformat.WigIterator.{VariableStep, FixedStep, WigUnit}
import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, ArrayBuffer}
import scala.io.Source

abstract class WigIterator extends BlockIterator[WigUnit]{

  /**
    * non safe opperation
    *
    * @param x one WigUnit
    * @param y the other WigUnit
    * @return assemble of x and y
    */
  def append(x: WigUnit, y: WigUnit): WigUnit = WigUnit.append(x,y)

  def merged(_maxSize: Int, _its: BlockIterator[WigUnit] = this) = new WigIterator with MergedIterator[WigUnit]{
    val maxSize = _maxSize
    val its = _its
  }

  def merged(_maxSize: Int) = merged(_maxSize,this)

  def filterWithBed(bit: Iterator[BedLine]): WigIterator = filterWithBed(bit, this)
  protected def filterWithBed(bit: Iterator[BedLine], wit: WigIterator): WigIterator =
    new WigIterator with GenBlockIterator[WigUnit]{

    protected var bedBuf: Option[BedLine] = if (bit.hasNext) Some(bit.next()) else None

    protected var wigBuf: Option[WigUnit] = if (wit.hasNext) Some(wit.next()) else None

    protected def gen(): Option[WigUnit] = {
      @tailrec
      def f(wigop: Option[WigUnit], bedop: Option[BedLine]): (Option[WigUnit], Option[WigUnit], Option[BedLine]) = {
        def nextb = if (bit.hasNext) Some(bit.next()) else None
        def nextw = if (wit.hasNext) Some(wit.next()) else None
        (wigop, bedop) match {
          case (Some(wig), Some(bed)) =>
            if (wig.chr != bed.chr) f(wigop, nextb)
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
    def span: Int
    def lines: Array[T]
    def length = lines.length * span
    def marginalize(wing: Int): WigUnit
    def +(that: WigUnit): WigUnit
    def interSection(bed: BedLine): Option[WigUnit]
    def toVariableStep: VariableStep
  }

  object WigUnit {
    def append(x: WigUnit, y: WigUnit) = x + y
  }

  /**
    * @param chr chromosome name
    * @param span span paramter
    * @param lines all (Int, Double) pairs
    */

  case class VariableStep(chr: String, span: Int, lines: Array[(Int, Double)]) extends WigUnit {
    type T = (Int, Double)

    def start = lines.head._1

    def toVariableStep = this

    def end = lines.last._1 + span

    def marginalize(wing: Int) = {
      require(span == 1)
      @tailrec
      def f(xs: List[(Int, Double)],
            ys: ListBuffer[(Int, Double)],
            zs: List[(Int, Double)]): List[(Int, Double)] = {
        def lwlim = ys.head._1
        def uplim = lwlim + (2 * wing)
        if (ys.last._1 == uplim) f(xs, ys.tail, (lwlim + wing, ys.foldLeft(0.0) { (n, x) => n + x._2 }) :: zs)
        else if (xs.isEmpty) zs.reverse
        else if (xs.head._1 <= uplim) f(xs.tail, ys :+ xs.head, zs)
        else f(xs.tail, ListBuffer(xs.head), zs)
      }
      val tmp = f(lines.tail.toList, ListBuffer(lines.head), Nil)
      VariableStep(chr, span, tmp.toArray)
    }

    override def appendableWith(that: Block): Boolean = {
      that match {
        case VariableStep(_chr, _span, _) => chr == _chr && span == _span
        case _ => false
      }
    }

    def +(that: WigUnit): WigUnit = {
      that match {
        case VariableStep(_, _, xs) => VariableStep(chr, span, lines ++ xs)
        case _ => throw new UnsupportedOperationException
      }
    }

    def interSection(bed: BedLine): Option[VariableStep] = {
      if (end <= bed.start || start >= bed.end || chr != bed.chr) None
      else {
        try {
          Some(VariableStep(chr, span, lines.dropWhile(_._1 < bed.start).takeWhile(_._1 < bed.end)))
        }
        catch{
          case e:java.util.NoSuchElementException =>
            e.printStackTrace()
            None
        }
      }
    }

  }

  object VariableStep {
    def apply(line: String, sep: String = DefaultSep): VariableStep = {
      val p = line.split(sep)
      val chr = p(1).split("=").last
      val span = if(p.length == 3) p(2).split("=").last.toInt else 1
      VariableStep(chr, span, null)
    }
  }

  /**
    * Variable-Step type WigUnit
    *
    * @param chr chromosome name
    * @param start 0-origin start index
    * @param step step parameter
    * @param span span parameter
    * @param lines all Doubles
    */

  case class FixedStep(chr: String, start: Int, step: Int, span: Int, lines: Array[Double]) extends WigUnit {
    type T = Double

    def end = start + length

    def marginalize(wing: Int): FixedStep = {
      require(step == 1 && span == 1)
      @tailrec
      def f(headi: Int, lasti: Int, sum: Double,
            result: ArrayBuffer[Double] = new ArrayBuffer[Double]()): ArrayBuffer[Double] = {
        if (lasti >= lines.length) result :+ sum
        else f(headi + 1, lasti + 1, sum - lines(headi) + lines(lasti), result :+ sum)
      }
      val init = (0 to 2 * wing).map(lines(_)).sum
      val tmp = f(0, 2 * wing + 1, init).toArray

      FixedStep(chr,start + wing, 1, 1, tmp)
    }

    def appendableWith(that: Block): Boolean =
      that match {
        case FixedStep(thatch,thats,_,_,_) => end == thats && chr == thatch
        case _ => false
      }

    def +(that: WigUnit): WigUnit =
      that match {
        case FixedStep(_,_,_,_,ys) => FixedStep(chr, start, step, span, lines ++ ys)
        case _ => throw new UnsupportedOperationException
      }

    def interSection(bed: BedLine): Option[FixedStep] =
      throw new UnsupportedOperationException


    def toVariableStep: VariableStep =
      new VariableStep(chr, span, lines.zipWithIndex.map{case (x, i) => (i + start,x)})
  }

  object FixedStep {
    def apply(line: String, sep: String = DefaultSep): FixedStep = {
      val p = line.split(sep)
      val chr = p(1).split("=").last
      val start = p(2).split("=").last.toInt
      val step = p(3).split("=").last.toInt
      val span = if(p.length == 3) p(2).split("=").last.toInt else 1
      FixedStep(chr, start, step, span, null)
    }
  }

  def fromSource(s: Source, maxsize: Int = 2048, sep: String = DefaultSep) =
    new WigIterator with GenBlockIterator[WigUnit]{

    protected val lines = s.getLines()

    protected var nextunit: WigUnit = null

    protected def gen(): Option[WigUnit] = {
      nextunit match {
        case VariableStep(chr, span, _) =>
          val buf = new ArrayBuffer[(Int, Double)]()
          for (line <- lines; if line.nonEmpty && !line.startsWith("#"); p = line.split(sep)) {
            p(0) match {
              case "fixedStep" =>
                nextunit = FixedStep(line)
                if(buf.nonEmpty) return Some(VariableStep(chr, span, buf.toArray))
              case "variableStep" =>
                nextunit = VariableStep(line)
                if(buf.nonEmpty) return Some(VariableStep(chr, span, buf.toArray))
              case _ =>
                buf += Tuple2(p(0).toInt, p(1).toDouble)
                if(buf.length >= maxsize) return Some(VariableStep(chr, span, buf.toArray))
            }
          }
          if (buf.nonEmpty) Some(VariableStep(chr, span, buf.toArray))
          else None
        case FixedStep(chr, start, step, span, _) =>
          val buf = new ArrayBuffer[Double]()
          for (line <- lines; if line.nonEmpty && !line.startsWith("#"); p = line.split(sep)) {
            p(0) match {
              case "fixedStep" =>
                nextunit = FixedStep(line)
                if(buf.nonEmpty) return Some(FixedStep(chr, start, step, span, buf.toArray))
              case "variableStep" =>
                nextunit = VariableStep(line)
                if(buf.nonEmpty) return Some(FixedStep(chr, start, step, span, buf.toArray))
              case _ =>
                buf += p(0).toDouble
                if(buf.length >= maxsize) return Some(FixedStep(chr, start, step, span, buf.toArray))
            }
          }
          if (buf.nonEmpty) Some(FixedStep(chr, start, step, span, buf.toArray))
          else None
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
}
