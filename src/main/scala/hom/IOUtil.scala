package hom

trait WithFileWriter {
  import java.io._
  import java.nio.charset.Charset
  var charset: Charset = Charset.defaultCharset
  
  class Exceptions(primary: Throwable, secondary: Throwable*) extends Exception(primary) {
    def all = secondary  
  }

  protected final def withWriter(f: File)(g: Writer => Unit) {
    val w: Writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f), charset))
    var problem: Exception = null
    try {
      g(w)
    } catch {
      case e: Exception => problem = e
    } finally {
      try {
        w.close()
      } catch {
        case e: java.io.IOException => problem = if (problem == null) e else new Exceptions(problem, e)
      }
    }
    if (problem != null) throw problem
  }
}
