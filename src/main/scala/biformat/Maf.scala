package biformat

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import alignment.Base
import scala.io.Source
/**
  * .maf format manager object.
  * */
object Maf {
  @deprecated
  def readMaf(mf: String, per: Int = 512): Array[List[Array[Base]]] = {
    val it = MafIterator.fromMSA(Source.fromFile(mf), "hg19")
    val totalunit = it.reduceLeft { (n, u) => n + u }
    val bases = totalunit.seqs
    val tmp = div(bases, per)
    tmp
  }

  /*def convert(mf: String, spmf: String, per: Int, species: Int): Unit = {
    val it = MafIterator.fromMSA(mf).iterator
    val w = new PrintWriter(spmf)
    var tmp = it.next()
    while (it.hasNext) {
      tmp = tmp + it.next()
      while (tmp.length > per) {
        val (x, y) = tmp.sliceAt(per)
        w.println(x.seqs.map(z => z.mkString("")).reduce(_ + "," + _))
        tmp = y
      }
    }
    if (tmp.length > 0) w.println(tmp.seqs.map(z => z.mkString("")).reduce(_ + "," + _))
    w.close()
  }*/

  def div(seqs: List[Array[Base]], size: Int): Array[List[Array[Base]]] = {
    @tailrec
    def f(xs: List[Array[Base]], ys: List[List[Array[Base]]], index: Int): Array[List[Array[Base]]] = {
      if (xs.head.isEmpty) ys.reverse.toArray
      else {
        val (target, reserve) = xs.map { x => x.splitAt(index) }.unzip
        f(reserve, target :: ys, index)
      }
    }
    f(seqs, Nil, size)
  }

  /**
    * a component appeared in .maf-fomat files.
    * iterably managed by MafIterator [[MafIterator]]
    * */
  case class MafUnit(lines: Map[String, MafLine], target: String) extends Block {
    val length = lines.values.head.length

    require(lines.values.forall(_.length == length))

    def start = lines(target).start
    def end = lines(target).end

    //def start(target: String) = lines(target).start

    /**
      * Please check whether appendable before use.
      * */
    def +(that: MafUnit): MafUnit = {
      val newlines = scala.collection.mutable.Map.empty[String, MafLine]
      that.lines.iterator.foreach{
        case (name, value)  =>
          this.lines.get(name) match {
            case None =>
              newlines += name -> MafLine(name, value.subname, value.start, value.num, value.strand,
                Array.fill(this.length)(Base.N) ++ value.seq)
            case _ =>
              newlines += name -> (this.lines(name) + value)
          }
      }
      this.lines.iterator.foreach{
        case (name, value)  =>
          that.lines.get(name) match {
            case None =>
              newlines += name -> MafLine(name, value.subname, value.start, value.num, value.strand,
                value.seq ++ Array.fill(that.length)(Base.N))
            case _ =>
              Unit
          }
      }
      MafUnit(newlines.toMap, target)
    }
    /**
      * [[Block.appendableWith]]
      * */
    def appendableWith(that: Block): Boolean = that match {
      case MafUnit(xlines, xtarget) =>
        val a = lines(target)
        val b = xlines(target)
        a.subname == b.subname && a.end == b.start && a.strand == b.strand
      case _ => false
    }
    /**
      * Works as like Seq.sliceAt
      * */
    /*def sliceAt(n: Int): Pair[MafUnit,MafUnit] = {
      val (a, b) = lines.iterator.map {
        case (x, y) =>
          val tmp = y.splitAt(n)
          Pair(x -> tmp._1, x -> tmp._2)
      }.toList.unzip
      (MafUnit(a.toMap, target), MafUnit(b.toMap, target))
    }*/
    @deprecated
    def seqs: List[Array[Base]] = lines.valuesIterator.map(_.seq).toList
  }

  object MafUnit {
    /**
      * @constructor
      * */
    def apply(lines: Iterable[MafLine], target: String): MafUnit = MafUnit(lines.map(x => x.name -> x).toMap, target)
  }

  final class MafIterator protected (val its: Iterator[MafUnit], target: String) extends BlockIterator[MafUnit] {

    def merge(maxSize: Int = 10000) = new MafIterator(mergedIterator(maxSize), target)

    def append(x: MafUnit, y: MafUnit): MafUnit = x + y
  }

  case class MafLine(name: String, subname: String, start: Long, num: Long, strand: String, seq: Array[Base]) {
    def length = seq.length
    def end = start + num
    def +(that: MafLine) = MafLine(name, subname, start, num + that.num, strand, seq ++ that.seq)
    /*def splitAt(n: Int): Pair[MafLine, MafLine] = {
      require(n < length)
      val (x, y) = seq.splitAt(n)
      (MafLine(name, subname, start, start + n, strand, x), MafLine(name, subname, start + n, end, strand, y))
    }*/
  }

  object MafLine {
    /**
      * @constructor
      * */
    def fromString(xs: Seq[String]) = {
      val names = xs(1).split('.')
      MafLine(names(0), if (names.length == 2) names(1) else "", xs(2).toLong, xs(3).toLong, xs(4), xs(6).toCharArray.map(Base.fromChar))
    }
  }

  object MafIterator {
    /**
      * @constructor
      * We recomend you to get Source by biformat.bigSource because ordinary maf-format files are big.
      * */
    def fromMSA(s : Source, target: String, sep: String = """\p{javaWhitespace}+""") =
      new MafIterator (new Iterator[MafUnit] {

        val lines = s.getLines()

        var i = 0

        protected var nextOne: Option[MafUnit] = nexti()

        def hasNext = nextOne.isDefined

        def next(): MafUnit = {
          if (!hasNext) sys.error("Nothing in next.")
          else {
            val tmp = nextOne.get
            nextOne = nexti()
            tmp
          }
        }
        def nexti(): Option[MafUnit] = {
          val buf = new ListBuffer[MafLine]()
          if (s.isEmpty) return None
          for (line <- lines; if line != "" && !line.startsWith("#"); p = line.split(sep)) {
            p(0) match {
              case "s" =>
                buf += MafLine.fromString(p)
              case "a" if buf.nonEmpty =>
                i = i + 1
                return Some(MafUnit(buf.toList, target))
              case _ => Unit
            }
          }
          if (buf.nonEmpty) Some(MafUnit(buf.toList, target))
          else {
            s.close()
            None
          }
        }
      }
        , target)
  }

}