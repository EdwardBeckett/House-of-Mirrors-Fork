
package hom

import java.io.File
import java.net.URI

import scala.util.Properties
import scala.xml.{Elem, XML}

object Resource {

  val homDir: File = new File(new File(Properties.userHome), ".hom")
  val usrDir: File = new File(Properties.userDir)

  private val SchemeFile = "file"
  private val SchemeJar = "jar"
  private val JarSeparator = "!/"
  private val JarBang = "!"

  /**
   * Go find a game resource file.
   * First check the name as a resource on the classpath.
   * Then check ${user.dir}. Then check ${user.home}/.hom.
   */
  def apply(r: String): Resource = {
    require(r != null)
    val where: java.net.URL = getClass.getClassLoader.getResource(r);
    if (where != null) {
      forURI(where.toURI)
    } else {
      forFilesystemPath(r)
    }
  }

  /**
   * Resolves a resource string against a base resource.
   * In particular, if base is the game pack resource,
   * constructs a game level resource.
   * But an arbitrary resource can be returned, such as "http://yahoo.com".
   */
  def apply(base: Resource, r: String): Resource = {
    if (base.uri.getScheme == SchemeJar) {
      forJarRelative(base, r)
    } else if (base.uri.isOpaque) {
      throw new IllegalArgumentException("Base URI must not be opaque for resolve")
    } else {
      forURI(base.uri.resolve(r))
    }
  }

  def apply(f: File): Resource = new FileResource(f)

  private def forURI(uri: URI): Resource = {
    if (uri.getScheme == "file") {
      new FileResource(uri)
    } else {
      new Resource(uri)
    }
  }

  private def forFilesystemPath(r: String): Resource = {
    val userDirProbe = new File(usrDir, r)
    if (userDirProbe.exists) {
      new FileResource(userDirProbe)
    } else {
      val homeDirProbe = new File(homDir, r)
      if (homeDirProbe.exists) {
        new FileResource(homeDirProbe)
      } else {
        throw new IllegalArgumentException("No resource: " + r)
      }
    }
  }

  private def forJarRelative(base: Resource, entry: String): Resource = {
    // resolve doesn't process opaque URIs like jar:file://foo.jar!/some/entry, so decompose it first
    val j = base.uri.getSchemeSpecificPart; // file://foo.jar!/some/entry
    val (jar, baseEntry) =
      if (j.contains(JarSeparator)) {
        if (j.indexOf(JarSeparator) == 0) {
          throw new IllegalArgumentException("Badly formed jar URI: " + base.uri)
        }
        val parts = j.split(JarBang) // split on ! to preserve /
        (parts(0), parts(1))
      } else {
        (j, "/")
      }
    val baseEntryURI = new URI("entry", null, baseEntry, null)
    val entryURI = baseEntryURI.resolve(entry)
    new Resource(new URI(base.uri.getScheme, jar + JarBang + entryURI.getPath(), null))
  }

  private class FileResource(val file: File) extends Resource(file.toURI) {
    def this(u: URI) = this(new File(u))
    override val isFile = true
    override val asFile: File = this.file
    override def loadXML(): Elem = XML.loadFile(this.file)
  }
}

sealed class Resource private (val uri: URI) {
  val isFile = false
  def asFile: File = throw new UnsupportedOperationException
  def loadXML(): Elem = XML.load(this.uri.toURL)
  override def toString = this.uri.toString
}
