package alignment

abstract class AminoAcid{

  def toChar:Char = toString.head
}

object AminoAcid{

  val binSize = 5

  case object A extends AminoAcid
  case object R extends AminoAcid
  case object N extends AminoAcid
  case object D extends AminoAcid
  case object C extends AminoAcid
  case object Q extends AminoAcid
  case object E extends AminoAcid
  case object G extends AminoAcid
  case object H extends AminoAcid
  case object I extends AminoAcid
  case object L extends AminoAcid
  case object K extends AminoAcid
  case object M extends AminoAcid
  case object F extends AminoAcid
  case object P extends AminoAcid
  case object S extends AminoAcid
  case object T extends AminoAcid
  case object W extends AminoAcid
  case object Y extends AminoAcid
  case object V extends AminoAcid
  case object Z extends AminoAcid
  case object X extends AminoAcid {
    override def equals(other: Any) = false
  }

  override def equals(other:Any) = other match {
    case X => false
    case AminoAcid => this.equals(other)
    case _ => false
  }


  private val values = Array(A,R,N,D,C,Q,E,G,H,I,L,K,M,F,P,S,T,W,Y,V,Z,X)

  private val acids:Map[Char,AminoAcid] = values.flatMap(x => List(x.toChar -> x,x.toChar.toLower -> x)).toMap

  def fromChar(c: Char):AminoAcid = acids.get(c) match {
    case Some(x) => x
    case None => X
  }
}