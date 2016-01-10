import java.io.{File, FileInputStream}
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
  def bigSource(f: File): Source = new BufferedSource(
    if (f.getName.endsWith(".gz")) new GZIPInputStream(new FileInputStream(f), 512 * 512)
    else new FileInputStream(f)
    , 512 * 512)

  def bigSource(f: String): Source = bigSource(new File(f))
}
