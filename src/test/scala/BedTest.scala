import biformat.BedIterator
import org.scalatest.FunSuite

/**
  * Created by yuto on 15/12/09.
  */
class BedTest extends FunSuite{
  test("read bed"){
    val bedit = BedIterator.fromSource(biformat.bigSource("src/test/resources/biformat/sample.bed"))
    val xs = List ((70,80), (81, 88), (91, 95), (97, 100), (101, 170), (200, 300))
    bedit.toList zip xs foreach{case (a,b) => assert(a.start == b._1 && a.end == b._2)}
  }
}
