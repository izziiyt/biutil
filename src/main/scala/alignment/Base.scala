package alignment

trait Base{
  def toChar:Char = toString.head
  def toInt:Int = Base.toInt(this)
  def isN: Boolean = false
}

object Base{

  val binLength:Int = 3

  case object A extends Base
  case object C extends Base
  case object G extends Base
  case object T extends Base
  case object N extends Base{
    override def isN = true
  }

  val fromInt:Int => Base = Array(A,C,G,T,N)
  val toInt:Base => Int = Map(A -> 0,C -> 1,G -> 2, T -> 3,N -> 4)

  val fromChar:Char => Base = {
    case 'a' | 'A' => A
    case 't' | 'T' => T
    case 'c' | 'C' => C
    case 'g' | 'G' => G
    case _ => N
  }
}

