import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import scala.io.{BufferedSource, Source}

package object biformat {
  def bigSource(f: String): Source = new BufferedSource(
    if (f.endsWith(".gz")) new GZIPInputStream(new FileInputStream(f), 1024 * 1024)
    else new FileInputStream(f)
    , 1024 * 1024)

}
