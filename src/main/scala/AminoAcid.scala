abstract class AminoAcid

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
  case object X extends AminoAcid

  private val values = Array(A,R,N,D,C,Q,E,G,H,I,L,K,M,F,P,S,T,W,Y,V,Z,X)

  private val acids: Map[String,AminoAcid] = values.map{case x => x.toString -> x}.toMap

  def toChar = toString.toCharArray.head

  def fromChar(c:Char) = fromString(c.toString)

  def fromString(s: String): AminoAcid = acids.get(s) match {
    case Some(x) => x
    case None => X
  }
}