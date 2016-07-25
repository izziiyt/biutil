package biformat

import scala.annotation.tailrec
/**
  * Bioinformatics format-managing interface extends Iterator.
  */
trait BlockIterator[T <: Block] extends Iterator[T] {
  import BlockIterator._

  /**
    * appends adjacent [[Block]] as long as [[Block.length]] is uncer maxSize
    *
    * @param maxSize
    * @param _its
    * @return shortened BlockIterator
    */
  //protected def merged(maxSize: Int, _its: BlockIterator[T]): MergedIterator[T]
  //protected def filtered(_its: BlockIterator[T]): MergedIterator[T]
  def merged(maxSize: Int): MergedIterator[T]

  protected def append(x: T, y: T): T

}

object BlockIterator {

  trait GenBlockIterator[T <: Block] extends BlockIterator[T]{
    protected var nextOne: Option[T] = None

    def next(): T = {
      if(!hasNext) throw new NoSuchElementException
      val tmp = nextOne.get
      nextOne = None
      tmp
    }

    def hasNext: Boolean = nextOne.isDefined || {
      nextOne = gen()
      nextOne.isDefined
    }

    protected def gen(): Option[T]
  }

  trait MergedIterator[T <: Block] extends GenBlockIterator[T]{

    def maxSize: Int

    def its: BlockIterator[T]

    protected var buf: Option[T] = None

    protected def gen(): Option[T] = {
      for(unit <- its) {
        buf match {
          case Some(x) if x.appendableWith(unit) && x.length + unit.length < maxSize =>
            buf = Some(append(x,unit))
          case Some(_) =>
            val tmp = buf
            buf = Some(unit)
            return tmp
          case None =>
            buf = Some(unit)
        }
      }
      buf match {
        case Some(_) =>
          val tmp = buf
          buf = None
          tmp
        case None => None
      }
    }
  }

  trait FilteredBlockIterator[T1 <: Block,T2 <: Block] extends GenBlockIterator[T1]{
    println("hoge")
    val wit: BlockIterator[T1]
    val bit: BlockIterator[T2]
    println("huga")
    if(bit.isEmpty) println("lkajsdh")
    if(wit.isEmpty) println("lkajsdh")
    protected var wigBuf: Option[T1] = if (wit.hasNext) Some(wit.next()) else None
    protected var bedBuf: Option[T2] = if (bit.hasNext) Some(bit.next()) else None
    println("yes")
    protected def gen(): Option[T1] = {
      @tailrec
      def f(wigop: Option[T1], bedop: Option[T2]): (Option[T1], Option[T1], Option[T2]) = {
        def nextb() = if (bit.hasNext) Some(bit.next()) else None
        def nextw() = if (wit.hasNext) Some(wit.next()) else None
        (wigop, bedop) match {
          case (Some(wig), Some(bed)) =>
            if (wig.chr != bed.chr) f(wigop, nextb())
            else wig.interSection(bed) match {
              case None =>
                if (wig.end <= bed.start) f(nextw(), bedop) else f(wigop, nextb())
              case tmp =>
                if (bed.end < wig.end) (tmp, wigop, nextb()) else (tmp, nextw(), bedop)
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

}

/**
  * Module class to be managed by [[biformat.BlockIterator]].
  */
trait Block extends Ordered[Block]{
  /** 0 origin index*/
  def start: Int
  /** 0 origin index*/
  def end: Int
  /** chromosome name*/
  def chr: String
  def length: Int
  def appendableWith(that: Block): Boolean
  override def compare(that: Block): Int = {
    def chr2int(str: String): Int = {
      val suffix = str.diff("chr")
      try suffix.toInt catch {case _:Exception => suffix.head.toInt + 99}
    }
    val tmp1 = chr2int(this.chr) - chr2int(that.chr)
    lazy val tmp2 = this.start - that.start
    lazy val tmp3 = this.end - that.end
    if(tmp1 == 0) {
      if(tmp2 == 0) tmp3
      else tmp2
    }
    else tmp1
  }

  def interSection[T <: Block](y: T): Option[this.type]

  def hasIntersection(that: Block): Boolean =
    this.chr == that.chr && this.start < that.end && this.end > that.start
}