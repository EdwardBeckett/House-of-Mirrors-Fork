package hom

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.Seq

class ResourcesTest extends AssertionsForJUnit {

  /**
   * This file is on the classpath, but in the filesystem.
   * (Sourced from src/test/resources.)
   */
  @Test def findsFileResource() {
    val f = "rowhouses/trial.homp"
    val r = Resource(f)
    assert(r.isFile)
  }

  /**
   * scalatest-1.2.jar has images/bluedot.gif,
   * so we use that for this test.
   * For this test to work, ScalaTest must not
   * be inherited from a source project, i.e.,
   * ScalaTest must be packaged in a jar.
   */
  @Test def findsJarResource() {
    val f = "images/bluedot.gif"
    val r = Resource(f)
    assert(!r.isFile)
  }

  /**
   * This file is not on any classpath.
   * The user dir must be the project dir (as in Eclipse).
   * (Sourced from src/test/other.)
   */
  @Test def findsFileResourceFromUserDir() {
    val f = "src/test/other/empty.sample"
    val r = Resource(f)
    assert(r.isFile)
  }

  /**
   * This file is not on any classpath,
   * but must exist at ${user.home}/.hom/emptySample
   * That must be set up for the user running the test.
   */
  @Test def findsFileResourceFromHomeDir() {
    val f = "empty.sample"
    val r = Resource(f)
    assert(r.isFile)
  }

  @Test def findsBaseRelativeResource() {
    val f = "rowhouses/trial.homp"
    val r = Resource(f)
    val g = Resource(r, "trial-welcome.hom")
    assert(g.isFile)
    assert(g.uri.toString.endsWith("trial-welcome.hom"))
  }

  @Test def findsBaseRelativeResourceFromJar() {
    val blue = "images/bluedot.gif"
    val base = Resource(blue)
    val red = Resource(base, "reddot.gif")
    assert(!red.isFile)
    assert(red.uri.toString.startsWith("jar:file:"))
    assert(red.uri.toString.endsWith("!/images/reddot.gif"))
  }
}
