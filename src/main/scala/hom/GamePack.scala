package hom

import java.io.{File,FileWriter}
import scala.xml._

object GamePack {

  def apply(s: String): GamePack = fromXML(Resource(s))

  def apply(f: File): GamePack = fromXML(Resource(f))

  def fromXML(r: Resource): GamePack = {
    fromXML(r, r.loadXML)
  }

  def fromXML(r: Resource, pack: Elem): GamePack = {
    val unlockedLevels = (pack \\ "unlocked" \\ "ulevel").toList.map(x=>x.attribute("id").get.text.toInt).toSet
    val packEntries = (pack \ "level").toList.map(n => PackEntry.fromXML(n))
    new GamePack(unlockedLevels, packEntries, Some(r))
  }

  def toXML(pack: GamePack): Elem = pack.toXML
}

/**
 * The game definition is a set of game level files
 * and the set of accessible or unlocked levels.
 */
case class GamePack(val unlockedLevels: Set[Int], val packEntries: List[PackEntry], packResource: Option[Resource] = None) {

  /** The number of levels defined in this pack */
  val numLevels: Int = this.packEntries.length

  /** The maximum level number defined in this pack (may differ from numLevels). */
  val maxLevel: Int = this.packEntries.foldLeft(-1) { (m,p) => m.max(p.id) }

  def packEntryForLevel(n: Int): PackEntry = this.packEntries.find(p => p.id == n).get
  def isLevelDefined(n: Int): Boolean = this.packEntries.find(_.id == n).isDefined
  
  def isUnlocked(n: Int): Boolean = this.unlockedLevels.contains(n)

  /**
   * Unlock the given level n.
   * If n was newly unlocked, results in true.
   */
  def unlock(n: Int): Option[GamePack] = {
    require(isLevelDefined(n))
    if (this.unlockedLevels.contains(n)) None
    else Some(copy(unlockedLevels = unlockedLevels + n))
  }

  /**
   * Unlock levels after completing level n.
   * If any levels were newly unlocked, result is Some[GamePack].
   */
  def unlockAll(n: Int): Option[GamePack] = {
    require(isLevelDefined(n))
    val todo = packEntryForLevel(n).unlock.toSet
    if (todo subsetOf unlockedLevels) {
      None // nothing to do
    } else {
      Some(copy(unlockedLevels = unlockedLevels union todo))
    }
  }

  /** Result is resource for level N. Throws if no such level, or pack was not loaded from a resource. */
  def levelResource(n: Int): Resource = Resource(this.packResource.get, packEntryForLevel(n).file)

  /** If pack was loaded from a file, return the parent directory. */
  def packDir: Option[File] = packResource filter { r => r.isFile && r.asFile.getParentFile != null } map { _.asFile.getParentFile }

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

/** A game pack entry describes where to load a level from, and which levels are unlocked on completion. */
case class PackEntry(id: Int, file: String, unlock: List[Int])

