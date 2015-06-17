package alignment

import java.io.FileReader
import alignment.Base._
import scala.collection._
import scala.util.parsing.combinator.JavaTokenParsers

case class CodonTable(is4Fold:Map[Codon,Boolean],transcript:Map[Codon,AminoAcid])

object CodonTable extends CodonTableParser{

  protected val bases = Array(A,C,G,T)

  def fromFile(inf:String):CodonTable = {
    val c2a = parseAll(blocks,new FileReader(inf)).get
    val is4Fold = mutable.Map[Codon,Boolean]()
    for(b1 <- bases;b2 <- bases;cdns = addOnes(b1,b2)){
      val aas:Array[AminoAcid] = cdns.map(c2a(_))
      val f = if(aas.forall(_ == aas(0))) true else false
      cdns.foreach(is4Fold += _ -> f)
    }
    new CodonTable(is4Fold,c2a)
  }

  protected def addOnes(f:Base,s:Base):Array[Codon] = bases.map(Codon(f,s,_))
}

class CodonTableParser extends JavaTokenParsers {

  def block:Parser[Block] = cdn~aa<~""".{3}""".r ^^ {case x~y => Block(x,y)}

  def cdn:Parser[Codon] = """[ATUCG]{3}""".r ^^ {Codon(_)}

  def aa:Parser[AminoAcid] = """.""".r ^^ {case x => AminoAcid.fromChar(x.head)}

  def blocks:Parser[Map[Codon,AminoAcid]] = rep(block) ^^ {_.map{case Block(x,y) => x -> y}.toMap}

  case class Block(cdn:Codon,amnacd:AminoAcid)
}