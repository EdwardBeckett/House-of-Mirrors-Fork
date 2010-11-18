
package hom

import scala.xml._
import LineColors._

/**
 * Objects which can supply their external XML representation.
 */
trait XMLable {
  def toXML: Node
}

/**
 * An unmarshaller for type A.
 */
trait UnXMLable[A] {
  def fromXML: A
}

/**
 * Various implicits to convert domain objects to XMLable,
 * and a Node to an UnXMLable that is a factory for the unmarshalled object.
 */
object XMLable {

  // utility methods

  private def getNum(node: Node, attr: String) = node.attribute(attr).get.text.toInt
  private def getPosition(node: Node) = Point(getNum(node, "x"), getNum(node, "y"))
  private def getDirection(node: Node) = Directions.forName(node.attribute("direction").get.text)
  private def getColor(node : Node) = LineColors.withNameIgnoreCase(node.attribute("color").get.text)
  private def getDetectorWavelength(node: Node): LineColor =
    //List(Red, Green, Blue).foldLeft(Black) { (a,b) => if (isNeeded(node, b)) { a + b } else a }
    (Black /: List(Red, Green, Blue)) { (r,v) => if (isNeeded(node, v)) { r + v } else r }
  private def isNeeded(node: Node, c: LineColor) = node.attribute(c.toString.toLowerCase).get.text match {
    case "required" => true
    case "met" => throw new IllegalStateException(c + " requirement 'met' is not supported.")
    case "unwanted" => false
  }

  implicit def anyGate2XMLable(g: Gate): XMLable = g match {
    case x: WormHole => wormhole2XMLable(x)
    case x: Blocker => blocker2XMLable(x)
    case x: Conduit => conduit2XMLable(x)
    case x: Source => source2XMLable(x)
    case x: Detector => detector2XMLable(x)
    case x: Prism => prism2XMLable(x)
    case x: SilveredSurface => mirror2XMLable(x)
  }

  implicit def wormhole2XMLable(g: WormHole): XMLable = new XMLable {
    override def toXML = <WormHole
        x1={g.position.x.toString} y1={g.position.y.toString}
        x2={g.twin.position.x.toString} y2={g.twin.position.y.toString}/>
  }
  implicit def node2WormholeFactory(n: Node): UnXMLable[FixedWormHole] = new UnXMLable[FixedWormHole] {
    def fromXML: FixedWormHole = new FixedWormHole(Point(getNum(n,"x1"), getNum(n,"y1")), Point(getNum(n,"x2"), getNum(n,"y2")))
  }
  implicit def node2MoveableWormholeFactory(n: Node): UnXMLable[MoveableWormHole] = new UnXMLable[MoveableWormHole] {
    def fromXML: MoveableWormHole = new MoveableWormHole(Point(getNum(n,"x1"), getNum(n,"y1")), Point(getNum(n,"x2"), getNum(n,"y2")))
  }

  implicit def blocker2XMLable(g: Blocker): XMLable = new XMLable {
    override def toXML = <Blocker x={g.position.x.toString} y={g.position.y.toString} />
  }
  implicit def node2BlockerFactory(n: Node): UnXMLable[Blocker] = new UnXMLable[Blocker] {
    def fromXML: Blocker = new Blocker(Point(getNum(n,"x"), getNum(n,"y")))
  }
  implicit def node2MoveableBlockerFactory(n: Node): UnXMLable[MoveableBlocker] = new UnXMLable[MoveableBlocker] {
    def fromXML: MoveableBlocker = new MoveableBlocker(Point(getNum(n,"x"), getNum(n,"y")))
  }

  implicit def conduit2XMLable(g: Conduit): XMLable = new OrientedGateXMLable(g)
  implicit def node2ConduitFactory(n: Node): UnXMLable[Conduit] = new OrientedGateUnXMLable[Conduit](n)
  implicit def node2MoveableConduitFactory(n: Node): UnXMLable[MoveableConduit] = new OrientedMoveableGateUnXMLable[MoveableConduit](n)

  implicit def source2XMLable(g: Source): XMLable = new XMLable {
    override def toXML = <Source x={g.position.x.toString} y={g.position.y.toString} direction={g.direction.toString} color={g.color.toString} />
  }
  implicit def node2Source(n: Node): UnXMLable[Source] = new UnXMLable[Source] {
    def fromXML: Source = new Source(getPosition(n), getDirection(n), getColor(n))
  }
  implicit def node2MoveableSource(n: Node): UnXMLable[MoveableSource] = new UnXMLable[MoveableSource] {
    def fromXML: MoveableSource = new MoveableSource(getPosition(n), getDirection(n), getColor(n))
  }

  implicit def detector2XMLable(g: Detector): XMLable = new XMLable {
    override def toXML = <Detector x={g.position.x.toString} y={g.position.y.toString} red={need(g.wavelength.r)} green={need(g.wavelength.g)} blue={need(g.wavelength.b)} />

    // color is either required or unwanted
    private def need(b: Boolean) = if (b) "required" else "unwanted"
  }
  implicit def node2Detector(node: Node): UnXMLable[Detector] = new UnXMLable[Detector] {
    def fromXML: Detector = new Detector(getPosition(node), getDetectorWavelength(node))
  }
  implicit def node2MoveableDetector(node: Node): UnXMLable[MoveableDetector] = new UnXMLable[MoveableDetector] {
    def fromXML: MoveableDetector = new MoveableDetector(getPosition(node), getDetectorWavelength(node))
  }

  implicit def prism2XMLable(g: Prism): XMLable = new OrientedGateXMLable(g)
  implicit def node2PrismFactory(n: Node): UnXMLable[Prism] = new OrientedGateUnXMLable[Prism](n)
  implicit def node2MoveablePrismFactory(n: Node): UnXMLable[MoveablePrism] = new OrientedMoveableGateUnXMLable[MoveablePrism](n)

  // any subclass of SilveredSurface, i.e. mirrors
  implicit def mirror2XMLable(g: SilveredSurface): XMLable = new OrientedGateXMLable(g)
  implicit def node2MirrorFactory(n: Node): UnXMLable[Mirror] = new OrientedGateUnXMLable[Mirror](n)
  implicit def node2MoveableMirrorFactory(n: Node): UnXMLable[MoveableMirror] = new OrientedMoveableGateUnXMLable[MoveableMirror](n)
  implicit def node2PartialMirrorFactory(n: Node): UnXMLable[PartialMirror] = new OrientedGateUnXMLable[PartialMirror](n)
  implicit def node2MoveablePartialMirrorFactory(n: Node): UnXMLable[MoveablePartialMirror] = new OrientedMoveableGateUnXMLable[MoveablePartialMirror](n)
  implicit def node2CrossMirrorFactory(n: Node): UnXMLable[CrossMirror] = new OrientedGateUnXMLable[CrossMirror](n)
  implicit def node2MoveableCrossMirrorFactory(n: Node): UnXMLable[MoveableCrossMirror] = new OrientedMoveableGateUnXMLable[MoveableCrossMirror](n)

  private class OrientedGateUnXMLable[A <: Gate with Oriented](n: Node) extends UnXMLable[A] {
    override def fromXML: A = node2OrientedGate(n)
  }

  private def node2OrientedGate[A <: Gate with Oriented](n: Node): A = {
    val p = getPosition(n)
    val d = getDirection(n)
    val g: Gate = n.label match {
      case "Conduit" => new Conduit(p, d)
      case "Prism" => new Prism(p, d)
      case "Mirror" => new Mirror(p, d)
      case "PartialMirror" => new PartialMirror(p, d)
      case "CrossMirror" => new CrossMirror(p, d)
    }
    return g.asInstanceOf[A]
  }

  private class OrientedMoveableGateUnXMLable[A <: Gate with Oriented with Moveable](n: Node) extends UnXMLable[A] {
    override def fromXML: A = node2OrientedMoveableGate(n)
  }

  private def node2OrientedMoveableGate[A <: Gate with Oriented with Moveable](n: Node): A = {
    val p = getPosition(n)
    val d = getDirection(n)
    val g: Gate = n.label match {
      case "Conduit" => new MoveableConduit(p, d)
      case "Prism" => new MoveablePrism(p, d)
      case "Mirror" => new MoveableMirror(p, d)
      case "PartialMirror" => new MoveablePartialMirror(p, d)
      case "CrossMirror" => new MoveableCrossMirror(p, d)
    }
    return g.asInstanceOf[A]
  }

  /** Deduce the tag from the simple class name, and supply x,y and direction attributes */
  private class OrientedGateXMLable(g: Gate with Oriented) extends XMLable {
    def gateName: String = {
      val candidate = g.getClass.getSimpleName
      val mov = "Moveable"
      if (candidate.startsWith(mov)) {
        candidate.substring(mov.length)
      } else {
        candidate
      }
    }
    //< {gateName} x={g.position.x.toString} y={g.position.y.toString} direction={g.direction.toString} />
    // Simulate an XML literal by building the attributes in reverse textual order.
    // This is for the purpose of PrettierPrinter output, but attribute order should be determined by the formatter anyway.
    def toXML = {
        /* append doesn't preserve attribute order
    	val m: MetaData =
          new UnprefixedAttribute("direction", g.direction.toString, Null) append
          new UnprefixedAttribute("y", g.position.y.toString, Null) append
          new UnprefixedAttribute("x", g.position.x.toString, Null)
         */

        val attrs = List(("x", g.position.x.toString), ("y", g.position.y.toString), ("direction", g.direction.toString))
        //val m = (attrs.reverse :\ Null.asInstanceOf[MetaData]) ((a,tail) => new UnprefixedAttribute(a._1, a._2, tail))
        val m = (Null.asInstanceOf[MetaData] /: attrs) ((tail,a) => new UnprefixedAttribute(a._1, a._2, tail))
    	Elem(null, gateName, m, TopScope)
    }
  }

  implicit def node2BoundFactory(node: Node): UnXMLable[Bound] = new UnXMLable[Bound] {
    override def fromXML = Bound(0, 0, getNum(node, "width"), getNum(node, "height"))
  }
}
