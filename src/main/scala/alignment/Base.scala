package alignment

abstract class Base{

  def toChar:Char = toString.head
  def toInt:Int = Base.toInt(this)
}

object Base{

  val binLength:Int = 3

  case object A extends Base
  case object C extends Base
  case object G extends Base
  case object T extends Base
  case object N extends Base

  val fromInt:Int => Base = Array(A,T,C,G,N)
  val toInt:Base => Int = Map(A -> 0,T -> 1,C -> 2, G -> 3,N -> 4)

  val fromChar:Char => Base = {
    case 'a' | 'A' => A
    case 't' | 'T' => T
    case 'c' | 'C' => C
    case 'g' | 'G' => G
    case _ => N
  }
}

