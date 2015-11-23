import biformat.WigIterator
import org.scalatest.FunSuite

/**
  * Created by yuto on 15/11/16.
  */
class WigTest extends FunSuite {

  test("marginalize"){
    val wing = 2
    val fi = "src/test/resources/biformat/sample.wig"
    val its = WigIterator.fromFile(fi).merge().iterator
    val x1 = Seq(4.2005078813259245E-5,4.0051565772730884E-5,4.001023323807591E-5,4.161713531369951E-5,4.001293308494154E-5).sum
    val x2 = Seq(4.0051565772730884E-5,4.001023323807591E-5,4.161713531369951E-5,4.001293308494154E-5,8.973172165137729E-6).sum

    println(its.next().marginalize(wing).lines.mkString(","))
    println("                   ||?")
    println(Array(x1, x2).mkString(","))

    for(it <- its) {
      assert(it.length - wing * 2 == it.marginalize(2).lines.length)
    }
  }
  test("merger"){
    val fi = "src/test/resources/biformat/sample.wig"

  }
}
