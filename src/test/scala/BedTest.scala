import biformat.BedIterator
import org.scalatest.FunSuite

/**
  * Created by yuto on 15/12/09.
  */
class BedTest extends FunSuite{
  test("read bed"){
    val bedit = BedIterator.fromSource(biformat.bigSource("src/test/resources/biformat/sample.bed"))
    bedit.foreach(x => println(x.start + " " + x.end))
  }
}
