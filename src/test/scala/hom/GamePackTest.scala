package hom

import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit

import scala.collection.Seq

import junit.SeqUnitHelper

class GamePackTest extends AssertionsForJUnit with SeqUnitHelper {

  @Test def packEntryFromXML() {
    val xs = <level id="3" file="somefile.hom"><unlock id="4"></unlock><unlock id="5"/></level>
    val p: PackEntry = PackEntry.fromXML(xs)
    assertNotNull(p)
    expect(3)(p.id)
    expect("somefile.hom")(p.file)
    coequals(List(4,5)) {
      p.unlock
    }
  }

  @Test def loadsPackDefinitionFile() {
    val f = "rowhouses/rowhouse.homp"
    val sut = GamePack(f);
    assert(sut.numLevels === 20)
  }
}
