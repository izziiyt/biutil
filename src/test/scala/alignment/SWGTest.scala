package alignment

import alignment.AminoAcid._
import org.scalatest.FunSuite

class SWGTest extends FunSuite {

  test("practical"){
    val x = SWG(-6,-1,-6,9)
    val p1 = Array[AminoAcid](G,T,S,S,R)
    val p2 = Array[AminoAcid](T,S,R,R)
    println(x(p1,p2,"test"))
  }
}
