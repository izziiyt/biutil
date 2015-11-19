package biformat

/**
  * Bioinformatics format UI.
  * [[Iterable]] [[TraversableOnce]]
  */

trait BlockIterator[T <: Block] extends Iterable[T] with TraversableOnce[T]{

  def its: Iterator[T]

  def iterator = its

  def merge: BlockIterator[T]

  protected def mergedIterator: Iterator[T] = new Iterator[T] {

    protected var buf: Option[T] = None

    protected var nextOne: Option[T] = gen()
    def next(): T = {
      if (!hasNext) sys.error("Nothing in next.")
      else {
        val tmp = nextOne.get
        nextOne = gen()
        tmp
      }
    }
    def hasNext: Boolean = nextOne.isDefined

    protected def gen(): Option[T] = {
      for (unit <- its) {
        buf match {
          case Some(x) if x.appendableWith(unit) =>
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

  protected def append(x: T, y: T): T
}
/**
  * Managed by BlockwiseIterator.[[biformat.BlockIterator]]
  */

trait Block extends {
  /** 0 origin index*/
  def start: Long
  /** 0 origin index*/
  def end: Long
  def appendableWith(that: Block): Boolean
}
