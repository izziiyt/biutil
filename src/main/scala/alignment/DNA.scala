package alignment

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ArrayBuffer,Builder}
import scala.collection.IndexedSeqLike

final class DNA private (val groups: Array[Long],val length:Int)
  extends IndexedSeq[Base] with IndexedSeqLike[Base,DNA]{
  import DNA._

  override protected[this] def newBuilder: Builder[Base,DNA] = DNA.newBuilder

  def apply(idx:Int): Base = {
    if(idx < 0 || length <= idx)
      throw new IndexOutOfBoundsException
    Base.fromInt((groups(idx / K) >> (idx % K * S) & M).toInt)
  }

  override def foreach[U](f:Base => U): Unit = {
    var i = 0
    var b:Long = 0
    while(i < length){
      b = if(i % K == 0) groups(i / K) else b >>> S
      f(Base.fromInt((b & M).toInt))
      i += 1
    }
  }
}

object DNA{

  private val S = Base.binLength

  private val K: Int = 64 / S //21

  private val M: Long = (1 << S) - 1 // 00..0111

  def fromSeq(buf: Seq[Base]): DNA = {
    val groups = new Array[Long]((buf.length - 1) / K + 1)
    for(i <- buf.indices)
      groups(i/K) |= Base.toInt(buf(i)) << (i % K * S)
    new DNA(groups,buf.length)
  }

  def fromString(s: String): DNA = DNA.fromSeq(s.map(Base fromChar))

  def apply(bases: Base*): DNA = fromSeq(bases)

  def newBuilder: Builder[Base,DNA] = new ArrayBuffer[Base] mapResult fromSeq
  implicit def canBuildFrom = new CanBuildFrom[DNA,Base,DNA]{
    def apply(): Builder[Base,DNA] = newBuilder
    def apply(from:DNA) = newBuilder
  }
}

