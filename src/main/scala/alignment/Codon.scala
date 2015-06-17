package alignment

case class Codon(first:Base,second:Base,third:Base)

object Codon{

  def apply(s:String):Codon = {
    require(s.length == 3)
    new Codon(Base fromChar s(0),Base fromChar s(1),Base fromChar s(2))
  }

}