
package hom.notes

import org.puremvc.java.interfaces.INotification

/**
 * An enumeration of notification types emitted by this application.
 */
object HouseNotifications extends Enumeration {

  type HouseNotificationType = Value

  // infrastructure and game
  val Startup, LoadGamePack, Trace, Status, BrokenMirror = Value

  // command requests
  val OpenFile, SaveFile, SaveGame, LoadLevel, Help = Value

  // model results
  val PackLoaded, LevelLoaded, LevelUpdate = Value

  def findName(n: String): Option[HouseNotificationType] = HouseNotifications.values.find(_.toString == n)

  def forNotification(n: INotification): Value = withName(n.getName)

  def asNames(c: List[HouseNotificationType]): Array[String] = c.map(n => n.toString).toArray
}

/** LoadLevel type: load N, advance to next, reset current */
object LevelCommands extends Enumeration {
  type LevelCommandType = Value

  val Load, Next, Reset = Value
}

/** BrokenMirror type. */
object MessageTypes extends Enumeration {
  val Error, Info = Value
}
