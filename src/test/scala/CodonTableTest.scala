import org.scalatest.FunSuite
import alignment._
/**
 * Created by yuto on 15/07/13.
 */
class CodonTableTest extends FunSuite {
  test("Codontable"){
    CodonTable.fromFile("src/test/resources/codon.table.txt")
  }
}
