
package hom.notes

import org.puremvc.java.interfaces.INotification

import HouseNotifications._

sealed abstract class HouseNotification[B](val notificationType: HouseNotificationType, val body: B, val subtype: String = null) extends INotification {
  require(notificationType != null)
  def getName: String = notificationType.toString
  def getBody: AnyRef = body.asInstanceOf[AnyRef]
  final def setBody(b: AnyRef) { throw new UnsupportedOperationException }
  def getType: String = subtype
  final def setType(t: String) { throw new UnsupportedOperationException }
  override def toString = "HouseNotification(" + notificationType + ", " + body + ")"
}

final case class LoadGamePackNotification(b: Tuple2[AnyRef,Int]) extends HouseNotification(LoadGamePack, b) {
  require(b._1 != null)
}

final case class TraceNotification(state: hom.GameState) extends HouseNotification(Trace, state) {
  require(state != null)
}
