package hom

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import scala.xml._

import Directions._
import XMLable._

class XMLTest extends AssertionsForJUnit {

  @Test
  def unpicklesWormholeExplicitly() {
    val n: Node = <WormHole x2="5" y2="7" x1="3" y1="2"></WormHole>
    val w: WormHole = node2WormholeFactory(n).fromXML
    expect(Point(3,2)) {
      w.position
    }
  }
  @Test
  def unpicklesWormholeImplicitly() {
    val n: Node = <WormHole x2="5" y2="7" x1="3" y1="2"></WormHole>
    val u: UnXMLable[FixedWormHole] = n
    val w: WormHole = u.fromXML
    expect(Point(3,2)) {
      w.position
    }
    expect(classOf[FixedWormHole]) {
      w.getClass
    }
  }
  @Test
  def unpicklesMoveableWormholeImplicitly() {
    val n: Node = <WormHole x2="5" y2="7" x1="3" y1="2"></WormHole>
    val u: UnXMLable[MoveableWormHole] = n
    val w: WormHole = u.fromXML
    expect(Point(3,2)) {
      w.position
    }
    expect(Point(5,7)) {
      w.other
    }
    expect(classOf[MoveableWormHole]) {
      w.getClass
    }
  }
  @Test
  def unpicklesMoveableConduitImplicitly() {
    val n: Node = <Conduit x="5" y="7" direction="Southwest"></Conduit>
    val u: UnXMLable[MoveableConduit] = n
    val c: Conduit = u.fromXML
    expect(Point(5,7)) {
      c.position
    }
    expect(Southwest) {
      c.direction
    }
    expect(classOf[MoveableConduit]) {
      c.getClass
    }
  }
}

