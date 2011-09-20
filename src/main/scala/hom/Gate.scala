package hom

import Directions._
import LineColors._

/** A gate is the base trait for all objects in the light box.  */
sealed trait Gate {
  /** The position of this gate. */
  val position: Point

  /** Does this gate contain the point? */
  def contains(p: Point): Boolean = (position == p)

  /** Validate this gate, in the given context. */
  def validate(gates: Seq[Gate]): Gate = this

  /**
   * Gates act upon incident rays, and in turn may be acted upon by them.
   * For the given incident ray, generate the outgoing rays and a possibly altered gate.
   */
  def act(in: Ray): (List[Ray], Gate)
}

/** A gate that blocks rays and is non-reactive to light. */
trait Opaque {
  this: Gate =>
  override def act(in: Ray): (List[Ray], Gate) = (Nil, this)
}

/** An Oriented Gate has a direction. */
trait Oriented {
  val direction: Direction
}

/** Marker mixin for gates that can be rotated in increments of 45 degrees. */
trait Turnable {
  this: Oriented =>
  def turnCW = reorient(direction.right45)
  def turnCCW = reorient(direction.left45)
  protected def reorient(d: Direction): Gate
}

/** Marker mixin for gates that can be moved. */
trait Moveable {
  this: Gate =>
  //def moveUp = moveTo(position.moveUp)
  //def moveDown = moveTo(position.moveDown)
  //def moveLeft = moveTo(position.moveLeft)
  //def moveRight = moveTo(position.moveRight)
  def moveTo(dst: Point, src: Point = position) = repositionTo(dst, src)
  protected def repositionTo(dst: Point, src: Point): Gate = reposition(dst)
  protected def reposition(dst: Point): Gate
}

/** Formerly used to subvert assignment to position. */
trait Fixed {
  this: Gate =>
}

trait Colored {
  val color: LineColor
}

/** Incident rays emerge at the twin wormhole with the same direction and color.  */
abstract class WormHole extends Gate { primary =>
  lazy val twin: WormHole = makeTwin
  private def makeTwin: WormHole = if (otherIsFixed) {
      new FixedWormHole(other, position, this.isInstanceOf[Fixed]) {
        override lazy val twin = primary
      }
    } else {
      new MoveableWormHole(other, position, this.isInstanceOf[Fixed]) {
        override lazy val twin = primary
      }
    }
  /** A wormhole contains both endpoints.
  override def contains(p: Point): Boolean = super.contains(p) || (other == p)
  */
  /** After a move, if we are detached from our twin, return our twin's twin to replace us. */
  override def validate(gates: Seq[Gate]): Gate = {
    println("Validate "+ this)
    val found = gates find (g => g.isInstanceOf[WormHole] && g.asInstanceOf[WormHole].twin.position == position)
    if (found.isDefined) {
      println("Found companion wormhole at " + found.get)
      val candidate = found.get.asInstanceOf[WormHole]
      candidate.twin // either this or new twin created when other end was moved
    } else {
      println("Validating "+ this)
      this  // this end was moved and other end is detached
    }
  }
  val other: Point
  val otherIsFixed: Boolean
  override def act(in: Ray) = (Ray(twin.position, in.direction, in.color) :: Nil, this)
}
/** If `otherIsFixed` is not specified, a fixed twin will be created. */
case class FixedWormHole(position: Point, other: Point, otherIsFixed: Boolean = true) extends WormHole with Fixed
/** If `otherIsFixed` is not specified, a moveable twin will be created. */
case class MoveableWormHole(position: Point, other: Point, otherIsFixed: Boolean = false) extends WormHole with Moveable {
  override def reposition(p: Point): WormHole = copy(position = p)
}

/** Blocks all rays. */
abstract class Blocker extends Gate with Opaque
case class FixedBlocker(position: Point) extends Blocker
case class MoveableBlocker(position: Point) extends Blocker with Moveable {
  override protected def reposition(to: Point): MoveableBlocker = copy(position = to)
}

/** Transmits only rays parallel to this orientation; blocks all others.  */
abstract class Conduit extends Gate with Oriented with Opaque {
  override def act(in: Ray) = if (in.direction ?|| direction) (Ray(position, in.direction, in.color) :: Nil, this) else super.act(in)
}
case class FixedConduit(position: Point, direction: Direction) extends Conduit with Fixed
case class MoveableConduit(position: Point, direction: Direction) extends Conduit with Moveable with Turnable {
  override protected def reposition(p: Point): MoveableConduit = copy(position = p)
  override protected def reorient(d: Direction): MoveableConduit = copy(direction = d)
}

/** Blocks incoming rays, but produces rays in the direction it is pointed. */
abstract class Source extends Gate with Oriented with Colored with Opaque {
  /** A list of rays emitted by this source. */
  def emit(): List[Ray] = Ray(position, direction, color) :: Nil
}
case class FixedSource(position: Point, direction: Direction, color: LineColor) extends Source
case class MoveableSource(position: Point, direction: Direction, color: LineColor) extends Source with Moveable with Turnable {
  override protected def reposition(p: Point): MoveableSource = copy(position = p)
  override protected def reorient(d: Direction): MoveableSource = copy(direction = d)
}

/**
 * All detectors must be lit up to complete a level.
 * Detectors transmit all rays but respond only to certain colors.
 * A detector is on when its absorption of colors equals its target wavelength.
 */
abstract class Detector extends Gate {
  val wavelength: LineColor
  val absorption: LineColor
  def isOn: Boolean = wavelength == absorption
  override def toString = "B(" + position + ":" + isOn + ")"
  override def act(in: Ray) = (Ray(position, in.direction, in.color) :: Nil, absorb(in.color))
  protected def absorb(color: LineColor): Detector
}
case class FixedDetector(position: Point, wavelength: LineColor, absorption: LineColor = Black) extends Detector {
  override protected def absorb(color: LineColor) = copy(absorption = absorption + color)
}
case class MoveableDetector(position: Point, wavelength: LineColor, absorption: LineColor = Black) extends Detector with Moveable {
  override def reposition(p: Point): MoveableDetector = copy(position = p)
  override protected def absorb(color: LineColor) = copy(absorption = absorption + color)
}

/**
 * A prism splits (redirects the components of) incoming light.
 * Red is passed through at right angles to the prism;
 * green is deflected 45 degrees (incident at 45 or 90 degrees);
 * blue is deflected 90 degrees (incident at right angles)
 * or split to both right angles (incident on the axis of the prism).
 */
abstract class Prism extends Gate with Oriented {
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
case class FixedPrism(position: Point, direction: Direction) extends Prism
case class MoveablePrism(position: Point, direction: Direction) extends Prism with Moveable with Turnable {
  override protected def reposition(p: Point): MoveablePrism = copy(position = p)
  override protected def reorient(d: Direction): MoveablePrism = copy(direction = d)
}

/** SilveredSurfaces have orientation and reflect rays.  */
abstract class SilveredSurface extends Gate with Oriented {
  override final def act(in: Ray) = (reflectRay(in), this)
  protected def reflectRay(inRay: Ray): List[Ray]
}

/**
 * A mirror reflects rays back or at right angles.
 * It has a mirrored surface and a blocking surface, so that it only reflects in the direction it is facing.
 */
abstract class Mirror extends SilveredSurface {
  override protected def reflectRay(in: Ray) = in.direction angle direction.reverse match {
    case 0 => Ray(position, in.direction.reverse, in.color) :: Nil
    case 45 => Ray(position, in.direction.right90, in.color) :: Nil 
    case 315 => Ray(position, in.direction.left90, in.color) :: Nil 
    case _ => Nil 
  }

  override def toString = "M(" + position + ":" + direction + ")"
}
case class FixedMirror(position: Point, direction: Direction) extends Mirror with Fixed
case class MoveableMirror(position: Point, direction: Direction) extends Mirror with Moveable with Turnable {
  override protected def reposition(p: Point): MoveableMirror = copy(position = p)
  override protected def reorient(d: Direction): MoveableMirror = copy(direction = d)
}

/**
 * A partial mirror both reflects and transmits light incident on either surface.
 */
abstract class PartialMirror extends SilveredSurface {
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
case class FixedPartialMirror(position: Point, direction: Direction) extends PartialMirror with Fixed
case class MoveablePartialMirror(position: Point, direction: Direction) extends PartialMirror with Moveable with Turnable {
  override protected def reposition(p: Point): MoveablePartialMirror = copy(position = p)
  override protected def reorient(d: Direction): MoveablePartialMirror = copy(direction = d)
}

abstract class CrossMirror extends SilveredSurface {
  override def reflectRay(inRay : Ray) = inRay.direction angle direction.reverse match {
    case 315 => Ray(position, inRay.direction.left135, inRay.color) :: Nil
    case 270 => Ray(position, inRay.direction.left45, inRay.color) :: Nil 
    case 45 => Ray(position, inRay.direction.right45, inRay.color) :: Nil 
    case 0 => Ray(position, inRay.direction.right135, inRay.color) :: Nil 
    case _ => Nil 
  }

  override def toString = "M(" + position + ":" + direction + ")"
}
case class FixedCrossMirror(position: Point, direction: Direction) extends CrossMirror with Fixed
case class MoveableCrossMirror(position: Point, direction: Direction) extends CrossMirror with Moveable with Turnable {
  override protected def reposition(p: Point): MoveableCrossMirror = copy(position = p)
  override protected def reorient(d: Direction): MoveableCrossMirror = copy(direction = d)
}

