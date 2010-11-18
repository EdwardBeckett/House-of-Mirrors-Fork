package hom

import java.io.{File,FileWriter}
import scala.xml._

object GamePack {

  def apply(s: String): GamePack = fromXML(Resource(s))

  def apply(f: File): GamePack = fromXML(Resource(f))

  def fromXML(r: Resource): GamePack = {
    val g = fromXML(r.loadXML)
    g.packResource = Some(r);
    return g
  }

  def fromXML(pack: Elem): GamePack = {
    val unlockedLevels = (pack \\ "unlocked" \\ "ulevel").toList.map(x=>x.attribute("id").get.text.toInt).toSet
    val packEntries = (pack \ "level").toList.map(n => PackEntry.fromXML(n))
    new GamePack(unlockedLevels, packEntries)
  }

  def toXML(pack: GamePack): Elem = pack.toXML

}

/**
 * Encapsulates the game definition.
 * The game definition is a set of game level files
 * and the set of accessible or unlocked levels.
 */
class GamePack(var unlockedLevels: Set[Int], private var packEntries: List[PackEntry]) {

  private var packResource: Option[Resource] = None
  
  /** The number of levels defined in this pack */
  val numLevels: Int = this.packEntries.length

  /** The maximum level number defined in this pack (may differ from numLevels). */
  val maxLevel: Int = this.packEntries.foldLeft(-1) { (m,p) => m.max(p.id) }

  def packEntryForLevel(n: Int): PackEntry = this.packEntries.find(p => p.id == n).get
  def isLevelDefined(n: Int): Boolean = this.packEntries.find(p => p.id == n).isDefined
  

  def isUnlocked(n: Int): Boolean = this.unlockedLevels.contains(n)

  /**
   * Unlock the given level n.
   * If n was newly unlocked, results in true.
   */
  def unlock(n: Int): Boolean = {
    if (this.unlockedLevels.contains(n)) false
    else {
      this.unlockedLevels += n
      true
    }
  }

  /**
   * Unlock levels after completing level n.
   * If any levels were newly unlocked, results in true.
   */
  def unlockAll(n: Int): Boolean = {
    require(isLevelDefined(n))
    var r = false;
    val p = packEntryForLevel(n)
    for (i <- p.unlock) {
      if (this.unlock(i)) r = true
    }
    r
  }

  /** Result is resource for level N. Throws if no such level, or pack was not loaded from a resource. */
  def levelResource(n: Int): Resource = Resource(this.packResource.get, packEntryForLevel(n).file)

  /**
   * If pack was loaded from a directory, return the (optional) directory.
   */
  def packDir: Option[File] = {
    if (this.packResource.isDefined && this.packResource.get.isFile) {
      val f = this.packResource.get.asFile
      if (f.getParentFile != null) Some(f.getParentFile) else None
    } else { None }
  }

  def toXML(): Elem = {
    <HoMPack>
      <unlocked> {unlockedLevels.toList.sortWith(_ < _).map(u => <ulevel id={u.toString}/>)} </unlocked>
      { for (p <- this.packEntries) yield { PackEntry.toXML(p) } }
    </HoMPack>
  }

  /*
   *

<HoMPack>
    <unlocked>
        <ulevel id="0"></ulevel>
    </unlocked>
    <level id="0" file="welcome.hom">
        <unlock id="1"></unlock>
    </level>
    <level id="1" file="level_1.hom">
        <unlock id="2"></unlock>
    </level>
   */
}

object PackEntry {
  def fromXML(n: Node): PackEntry = {
    val id = (n \ "@id").text.toInt
    val file = (n \ "@file").text.trim
    val unlockable: List[Int] = (n \\ "unlock").toList.map(x => x.attribute("id").get.text.toInt)
    new PackEntry(id, file, unlockable)
  }

  def toXML(p: PackEntry): Elem = {
    <level id={p.id.toString} file={p.file}>
      { p.unlock.map(i => <unlock id={i.toString}/>) }
    </level>
  }
}

class PackEntry(val id: Int, val file: String, val unlock: List[Int])

