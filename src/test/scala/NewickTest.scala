import java.io.File

import biformat.{Newick, NewickParsers}
import org.scalatest.FunSuite
/**
  * Created by yuto on 16/04/29.
  */
class NewickTest extends FunSuite{
  val sample = new File("src/test/resources/biformat/sample.nh")
  test("toList") {
    val tree = NewickParsers.fromFile(sample)
    val branches = tree.toList.collect {
      case x: Newick.Child => x.branch
    }

    assert(branches == List(0.3, 0.2, 0.1, 0.7, 0.2, 0.4))
    val leaves = tree.toList.collect {
      case Newick.Leaf(_, x) => x
    }

    assert(leaves == List("alpha", "beta", "gamma", "delta"))

    val dfll = tree.toDFMLL.toList.collect {
      case x: Newick.Child => x.branch
    }

    assert(dfll == List(0.0, 1.2, 1.1, 1.0, 0.3, 0.9))
  }
}
