package biformat

/**
  * Bioinformatics format-managing interface.
  */

trait BlockIterator[T <: Block] extends Iterator[T] {
  import BlockIterator._
  /**
    * Create Iterator[Block]
    * concatenates adjacent Blocks if they are appnedableWith
    * [[Block.appendableWith]]
    * */
  protected def merged(maxSize: Int, _its: BlockIterator[T]): MergedIterator[T]
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
}

/**
  * Managed by BlockwiseIterator.[[biformat.BlockIterator]]
  */
trait Block extends {
  /** 0 origin index*/
  def start: Int
  /** 0 origin index*/
  def end: Int
  def chr: String
  def length: Int
  def appendableWith(that: Block): Boolean
}
