package hom

import Directions._
import LineColors._

/** An Oriented Gate has a mutable direction. */
trait Oriented {
  var direction: Direction
}

/** A Turnable Gate can be turned clockwise or counter-clockwise in 45 degree increments, mutating the gate direction. */
trait Turnable {
  this: Gate with Oriented =>
  def turnCW = { this.direction = this.direction.right45; this }
  def turnCCW = { this.direction = this.direction.left45; this }
}

/**
 * Mutates the position of this gate.
 */
trait Moveable {
  this: Gate =>
  def moveUp = { this.position = position.moveUp; this }
  def moveDown = { this.position = position.moveDown; this }
  def moveLeft = { this.position = position.moveLeft; this }
  def moveRight = { this.position = position.moveRight; this }
  def moveTo(dst: Point) = { this.position = dst; this }
}

/**
 * A gate that refuses to be moved.
 */
trait Fixed {
  this: Gate =>
  override def position_=(p: Point): Nothing = throw new UnsupportedOperationException
}

/**
 * A gate is the base class for all objects in the light box.
 * Gates act upon incident rays, and in turn may be acted upon by them.
 * A gate has a mutable position.
 */
sealed abstract class Gate(p: Point) {
  private var where = p
  def position: Point = this.where
  def position_=(other: Point) { this.where = other }
  /**
   * For the given incoming ray, generate the outgoing rays
   * and a possibly altered gate.
   */
  def act(inRay: Ray) : (List[Ray], Gate) = (Nil, this)
}

/**
 * Incident rays emerge at the twin wormhole with the same direction and color.
 */
class WormHole(p: Point, val other: Point) extends Gate(p) {
  var twin: WormHole = _
  override def act(inRay: Ray) = (Ray(twin.position, inRay.direction, inRay.color) :: Nil, this)
}
class FixedWormHole(p: Point, p2: Point) extends WormHole(p, p2) with Fixed
class MoveableWormHole(p: Point, p2: Point) extends WormHole(p, p2) with Moveable

/**
 * Blocks all rays.
 */
class Blocker(p: Point) extends Gate(p) {
  override def act(inRay: Ray): (List[Ray], Gate) = (Nil, this)
}
class MoveableBlocker(p: Point) extends Blocker(p) with Moveable

/**
 * Transmits only rays parallel to this orientation; blocks all others.
 */
class Conduit(p: Point, var direction: Direction) extends Gate(p) with Oriented {
  override def act(inRay: Ray) = {
    if (inRay.direction == direction || inRay.direction == direction.reverse)
      (Ray(position, inRay.direction, inRay.color) :: Nil, this)
    else (Nil, this)
  }
}
class MoveableConduit(p: Point, d: Direction) extends Conduit(p, d) with Moveable with Turnable

/**
 * Blocks incoming rays, but produces rays in the direction it is pointed.
 */
class Source(p: Point, var direction: Direction, var color: LineColor) extends Gate(p) with Oriented {
  override def act(inRay : Ray) = (Nil, this)
  /** A list of rays emitted by this source. */
  def emit(): List[Ray] = Ray(this.position,this.direction,this.color) :: Nil
}
class MoveableSource(p: Point, d: Direction, c: LineColor) extends Source(p,d,c) with Moveable with Turnable

/**
 * All detectors must be lit up to complete a level.
 * Detectors transmit all rays but respond only to certain colors.
 * A detector is on when its absorption of colors equals its target wavelength.
 */
class Detector(p: Point, val wavelength: LineColor, val absorption: LineColor = Black) extends Gate(p) {
  def isOn: Boolean = this.wavelength == this.absorption
  override def act(inRay: Ray) =
    (Ray(this.position, inRay.direction, inRay.color) :: Nil, new Detector(this.position, wavelength, absorption + inRay.color))
  override def toString = "B(" + position + ":" + isOn + ")"
}
class MoveableDetector(p: Point, w: LineColor) extends Detector(p,w) with Moveable

/**
 * A prism splits (redirects the components of) incoming light.
 * Red is passed through at right angles to the prism;
 * green is deflected 45 degrees (incident at 45 or 90 degrees);
 * blue is deflected 90 degrees (incident at right angles)
 * or split to both right angles (incident on the axis of the prism).
 */
class Prism(p: Point, var direction: Direction) extends Gate(p) with Oriented {
  private def splitRedRay(inRay: Ray) = inRay.direction angle direction.reverse match {
    case 90 => Ray(position, inRay.direction, Red) :: Nil 
    case 270 => Ray(position, inRay.direction, Red) :: Nil 
    case _ => Nil 
  }

  private def splitGreenRay(inRay: Ray) = inRay.direction angle direction.reverse match {
    case 45 => Ray(position, inRay.direction.right45, Green) :: Nil 
    case 90 => Ray(position, inRay.direction.right45, Green) :: Nil 
    case 270 => Ray(position, inRay.direction.left45, Green) :: Nil 
    case 315 => Ray(position, inRay.direction.left45, Green) :: Nil 
    case _ => Nil 
  }

  private def splitBlueRay(inRay: Ray) = inRay.direction angle direction.reverse match {
    case 90 => Ray(position, inRay.direction.right90, Blue) :: Nil 
    case 270 => Ray(position, inRay.direction.left90, Blue) :: Nil 
    case 0 => Ray(position, inRay.direction.left90, Blue) :: Ray(position, inRay.direction.right90, Blue) :: Nil 
    case _ => Nil 
  }

  override def act(inRay: Ray) = {
    var result: List[Ray] = Nil
    inRay.color.components.foreach(c => c match {
      case Red => result = splitRedRay(inRay) ::: result
      case Green => result = splitGreenRay(inRay) ::: result
      case Blue => result = splitBlueRay(inRay) ::: result
    })
    (result, this)
  }

  override def toString = "Prism(" + position + ":" + direction + ")"
}
class MoveablePrism(p: Point, d: Direction) extends Prism(p,d) with Moveable with Turnable

/**
 * SilveredSurfaces have orientation and reflect rays.
 */
abstract class SilveredSurface(p: Point, var direction: Direction) extends Gate(p) with Oriented {
  override final def act(inRay: Ray) = (reflectRay(inRay), this)
  protected def reflectRay(inRay: Ray): List[Ray]
}

/**
 * A mirror reflects rays back or at right angles.
 * It has a mirrored surface and a blocking surface, so that it only reflects in the direction it is facing.
 */
class Mirror(p: Point, d: Direction) extends SilveredSurface(p,d) {
  override protected def reflectRay(inRay : Ray) = inRay.direction angle direction.reverse match {
    case 0 => Ray(position, inRay.direction.reverse, inRay.color) :: Nil
    case 45 => Ray(position, inRay.direction.right90, inRay.color) :: Nil 
    case 315 => Ray(position, inRay.direction.left90, inRay.color) :: Nil 
    case _ => Nil 
  }

  override def toString = "M(" + position + ":" + direction + ")"
}
class MoveableMirror(p: Point, d: Direction) extends Mirror(p,d) with Moveable with Turnable

/**
 * A partial mirror both reflects and transmits light incident on either surface.
 */
class PartialMirror(p: Point, d: Direction) extends SilveredSurface(p,d) {
  override protected def reflectRay(inRay : Ray) = inRay.direction angle direction.reverse match {
    case 0 => Ray(position, inRay.direction.reverse, inRay.color) :: Ray(position, inRay.direction, inRay.color) :: Nil
    case 180 => Ray(position, inRay.direction.reverse, inRay.color) :: Ray(position, inRay.direction, inRay.color) :: Nil
    case 45 => Ray(position, inRay.direction.right90, inRay.color) :: Ray(position, inRay.direction, inRay.color) :: Nil 
    case 135 => Ray(position, inRay.direction.left90, inRay.color) :: Ray(position, inRay.direction, inRay.color) :: Nil 
    case 225 => Ray(position, inRay.direction.right90, inRay.color) :: Ray(position, inRay.direction, inRay.color) :: Nil 
    case 315 =>  Ray(position, inRay.direction.left90, inRay.color) :: Ray(position, inRay.direction, inRay.color) :: Nil
    case _ => Nil 
  }

  override def toString = "PM(" + position + ":" + direction + ")"
}
class MoveablePartialMirror(p: Point, d: Direction) extends PartialMirror(p,d) with Moveable with Turnable

class CrossMirror(p: Point, d: Direction) extends SilveredSurface(p,d) {
  override def reflectRay(inRay : Ray) = inRay.direction angle direction.reverse match {
    case 315 => Ray(position, inRay.direction.left135, inRay.color) :: Nil
    case 270 => Ray(position, inRay.direction.left45, inRay.color) :: Nil 
    case 45 => Ray(position, inRay.direction.right45, inRay.color) :: Nil 
    case 0 => Ray(position, inRay.direction.right135, inRay.color) :: Nil 
    case _ => Nil 
  }

  override def toString = "M(" + position + ":" + direction + ")"
}
class MoveableCrossMirror(p: Point, d: Direction) extends CrossMirror(p,d) with Moveable with Turnable

