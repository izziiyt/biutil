import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import scala.io.{BufferedSource, Source}

/**
  * Provides classes for dealing with bioinfomatics formatted data.
  *
  * ==Overview==
  * You don't need to concern about heap size.
  * [[biformat.WigIterator]] and [[biformat.MafIterator]] can manage
  * large data as once-traversable iterator.
  * Also you can use these iterators by functional methods.
  */
package object biformat {
  def bigSource(f: String): Source = new BufferedSource(
    if (f.endsWith(".gz")) new GZIPInputStream(new FileInputStream(f), 1024 * 1024)
    else new FileInputStream(f)
    , 1024 * 1024)

}
