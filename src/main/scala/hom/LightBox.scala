package hom

import java.awt.AWTEvent
import java.awt.event._
import java.awt.image.{BufferedImage}
import java.awt.{Graphics2D, Graphics, Color, BasicStroke, Dimension, Composite, RenderingHints, CompositeContext, GridBagConstraints}

import scala.swing._
import scala.swing.event._

import Directions._
import LineColors._

/** UI configuration and constants. */
object LightBox {
  val DefaultBoxSide = 600
}

/**
 * Draws the House of Mirrors.
 * The LightBox is just a list of rays (line segments) and gates (various objects).
 * The UI emits requests to move and rotate gates.
 */
class LightBox extends Panel {

  import LightBox._

  val bgColor = new Color(5, 5, 5)
  val gridColor = new Color(25, 25, 25)

  val widePixels    = 4
  val wideStroke    = new BasicStroke(4)
  val thinPixels    = 1
  val thinStroke    = new BasicStroke(1)
  val normStroke    = new BasicStroke(2)
  val mediumStroke  = new BasicStroke(3)
  val thickStroke   = new BasicStroke(6)

  this.preferredSize = new java.awt.Dimension(DefaultBoxSide, DefaultBoxSide)

  /** How big is our box, on component resize? */
  var boxside: Int = _

  /** How big is a scaled grid box in the horizontal? */
  var hscale: Int = _
  /** In the vertical? */
  var vscale: Int = _

  import java.awt.event.{ComponentAdapter, ComponentEvent}
  this.peer.addComponentListener(
    new ComponentAdapter {
      override def componentResized(e: ComponentEvent) {
        if (e.getID == ComponentEvent.COMPONENT_RESIZED && e.getComponent == LightBox.this.peer) {
          calculateScale()
        }
      }
    }
  )

  private def calculateScale() {
    val d: java.awt.Dimension = this.size
    this.boxside = if (d.width < d.height) d.width else d.height
    this.hscale = this.boxside / this.gridBounds.width
    this.vscale = this.boxside / this.gridBounds.height
  }

  val selectionStroke = new BasicStroke(1, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 0.0f, (2f :: 5f :: Nil).toArray, 0.0f)
  val dragStroke = new BasicStroke(1, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 0.0f, (1f :: 5f :: Nil).toArray, 0.0f)

  var isEmptyGesture = false
  var isDragging = false
  var dragStart: Point = _
  var hoverPoint: Point = _

  var gridBounds = new Bound(Point(0,0), Point(15,15))
  var gates: List[Gate] = Nil
  var segments: List[Segment] = Nil
  /** The currently selected gate. */
  var currentSelection: Option[Gate] = None

  var completed: Boolean = false

  var renderables: List[Renderable] = Nil

  listenTo(mouse.clicks, mouse.moves, mouse.wheel, keys)

  reactions += {
    case KeyPressed(_, Key.N, _, _) => highlightNextMoveableGate()
    case KeyPressed(_, Key.P, _, _) => highlightPreviousMoveableGate()
    case e: MousePressed => startDrag(e)
    case e: MouseDragged => doDrag(e)
    case e: MouseReleased => endDrag(e)
    case e: MouseWheelMoved => wheeling(e)
    case _ => null // println ("Unreacted event")
  }

  def occupied(p: Point): Boolean = this.gates.find(g => g.position == p).isDefined

  def occupiedByMoveable(p: Point): Boolean = this.gates.find(g => g.position == p && g.isInstanceOf[Moveable]).isDefined

  private def inBounds(p: Point): Boolean = this.gridBounds.contains(p)

  private def scaledPoint(x: Double, y: Double): Point = {
    Point((x / hscale).toInt, (y / vscale).toInt)
  }

  private def startDrag(e: MousePressed) {
    val clickPoint = scaledPoint(e.point.getX, e.point.getY)
    val g = optionalGateForPosition(clickPoint)
    if (g.isDefined) {
      g.get match {
        case m: Moveable =>
          this.dragStart = clickPoint
          this.hoverPoint = this.dragStart
          this.isDragging = true
          repaint()
        case _ =>
          // clicking a fixed gate does nothing
      }
    } else {
      // clicking an empty spot means change level
      this.dragStart = clickPoint
      this.isEmptyGesture = true
    }
  }
  private def doDrag(e: MouseDragged) {
    if (this.isDragging) {
      val newHoverPoint = scaledPoint(e.point.getX, e.point.getY)
      if (newHoverPoint != this.hoverPoint) {
        this.hoverPoint = newHoverPoint
        repaint()
      }
    }
  }
  private def endDrag(e: MouseReleased) {
    import java.awt.event.MouseEvent
    val releasePoint = scaledPoint(e.point.getX, e.point.getY)
    if (this.isDragging) {
      if (this.dragStart == releasePoint) {
          // This was just a click
          if (this.currentSelection.isDefined && releasePoint == this.currentSelection.get.position) {
            // Clicking current selection rotates it
            //assert (this.currentSelection.get.isInstanceOf[Moveable])
            //publish(ClickEvent(releasePoint, if (e.peer.getButton == MouseEvent.BUTTON1) 0 else 1))
            publish(ClickEvent(releasePoint, MouseButtons.forEvent(e)))
          } else {
            // Clicking another (moveable) gate selects it
            val g = optionalGateForPosition(releasePoint)
            select(g)
            //assert (this.currentSelection.get.isInstanceOf[Moveable])
          }
      } else {
          // This was a drag :)
          if (inBounds(releasePoint) && !occupied(releasePoint)) {
            selectAt(this.dragStart) // move selection to start, it will move on next update
            publish(DragEvent(this.dragStart, releasePoint))
          }
      }
      this.isDragging = false
      repaint()
    } else if (this.isEmptyGesture) {
      this.isEmptyGesture = false
      if (this.dragStart == releasePoint) {
        //publish(ClickEvent(releasePoint, if (e.peer.getButton == MouseEvent.BUTTON1) 0 else 1))
        publish(ClickEvent(releasePoint, MouseButtons.forEvent(e)))
      }
    }
  }
  private def wheeling(e: MouseWheelMoved) {
    import java.awt.event.MouseEvent._
    import java.awt.event.MouseWheelEvent._
    if (this.isDragging) return
    val clickPoint = scaledPoint(e.point.getX, e.point.getY)
    val g = optionalGateForPosition(clickPoint)
    if (g.isDefined) {
      g.get match {
        case m: Turnable =>
          val w = e.peer.asInstanceOf[MouseWheelEvent]
          val (rotation: Int, scale: Int) = w.getScrollType match {
            case WHEEL_UNIT_SCROLL => (w.getWheelRotation, w.getScrollAmount)
            case WHEEL_BLOCK_SCROLL => (w.getWheelRotation, 1)
          }
          //val direction = if (rotation < 0) 0 else 1
          import MouseButtons._
          val direction = if (rotation < 0) Left else Right
          val clicks = if (rotation < 0) -rotation else rotation
          for (i <- 0 until clicks) publish(ClickEvent(clickPoint, direction))
        case _ =>
          // scrolling over a fixed gate does nothing
      }
    }
  }

  private def optionalGateForPosition(where: Point): Option[Gate] = {
    this.gates.find(g => g.position == where)
  }

  /** The gate at the given position. Blows up if no such gate. */
  private def gateForPosition(where: Point): Gate = {
    //this.gates.find(g => g.position == where).get
    optionalGateForPosition(where).get
  }

  /** The next moveable gate after the given gate, defaults to the first moveable gate. */
  private def nextMoveableGate(from: Option[Gate]): Option[Gate] = {
    var first: Option[Gate] = None
    var found = false
    // as a side effect, predicate saves the first moveable gate
    def isNextMoveable(g: Gate): Boolean = {
      g match {
        case x: Moveable =>
          if (found || !from.isDefined) {
            true // the first moveable after found, or the very first if no from
          } else if (from.isDefined && from.get == g) {
            found = true // found from gate
            if (!first.isDefined) first = Some(x) // from is also first
            false // we want the next one
          } else if (!first.isDefined) {
            first = Some(x) // the first moveable
            false // continue looking for gate
          } else {
            // we've seen first moveable already, but not from
            false // continue looking for gate
          }
        case _ => false
      }
    }
    val result: Option[Gate] = this.gates.find(isNextMoveable)
    if (!result.isDefined) first else result
  }

  /** The previous moveable gate before the given gate, defaults to the last moveable gate. */
  private def previousMoveableGate(from: Option[Gate]): Option[Gate] = {
    var prev: Option[Gate] = None
    // as a side effect, predicate saves the previous (or last) moveable gate
    def isFrom(g: Gate): Boolean = {
      g match {
        case x: Moveable =>
          if (prev.isDefined && from.isDefined && from.get == g) {
            true // we're done
          } else {
            prev = Some(x)
            false
          }
        case _ => false
      }
    }
    this.gates.find(isFrom)
    return prev
  }

  private def highlightNextMoveableGate() {
    currentSelection = nextMoveableGate(currentSelection)
    repaint()
  }

  private def highlightPreviousMoveableGate() {
    currentSelection = previousMoveableGate(currentSelection)
    repaint()
  }

  /**
   * If a gate like g is at point p, make it the current selection.
   * Used to make the current selection follow a gate that may have moved
   * or changed identity after an update.
   */
  def selectAt(g: Gate, p: Point) {
    // ignore g for now
    selectAt(p)
    /*
    this.currentSelection = this.gates.find(x => x.position == p) match {
      case Some(m) => Some(m)
      case None => None
    }
    */
  }

  def selectAt(p: Point) {
    this.currentSelection = this.gates.find(x => x.position == p)
  }

  def select(g: Option[Gate]) {
    this.currentSelection = g
  }

  var offscreen = new BufferedImage(DefaultBoxSide, DefaultBoxSide, BufferedImage.TYPE_INT_RGB)

  override def paintComponent(g: Graphics2D) {
    val d = size
    if (d.width > offscreen.getWidth || d.height > offscreen.getHeight) {
      this.offscreen = new BufferedImage(d.width, d.height, BufferedImage.TYPE_INT_RGB)
    }
    paintHouse(this.offscreen.getGraphics.asInstanceOf[Graphics2D])
    g.drawImage(offscreen, 0, 0, d.width, d.height, this.peer)
  }

  /**
   * Render the board.
   */
  final private def paintHouse(g: Graphics2D) {

    // Fill with Bg
    val completedBgColor = new Color(30, 0, 0)
    if (this.completed) g.setColor(completedBgColor) else g.setColor(bgColor)
    g.fillRect(0,0, this.boxside, this.boxside)

    g.setClip(0,0,this.gridBounds.width * hscale - 1,this.gridBounds.height * vscale - 1)


    // Draw the Grid
    //Grid.render(g)

    // Draw those saber rays
    /*
    val oldComposite = g.getComposite
    g.setComposite(AddComposite)
    g.setStroke(thinStroke)
    this.segments.foreach (line => {
      g.setColor(line.color.color)
      g.drawLine (hscale/2 + line.start.x * hscale,
                  vscale/2 + line.start.y * vscale,
                  hscale/2 + line.end.x * hscale,
                  // Math.min(hscale/2 + line.end.x * hscale, d.width - (d.width % hscale)),
                  vscale/2 + line.end.y * vscale)
    })
    g.setComposite(oldComposite)
    */

    // Draw the gates. Notice that we draw them on-top-of the saber rays
    //g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    for (r <- this.renderables) r.render(g)

    // Draw the selection, but not if it's the start of a drag
    if (this.currentSelection.isDefined && !(this.isDragging && this.currentSelection.get.position == this.dragStart)) {
      val selectionColor = new Color(200, 200, 200)
      g.setStroke(selectionStroke)
      g.setColor(selectionColor)
      g.drawRect(currentSelection.get.position.x * hscale, currentSelection.get.position.y * vscale, hscale, vscale)
    }

    // Draw the drag-start and hover cells
    if (this.isDragging) {
      val dragColor = new Color(100, 100, 150)
      val dragLineColor = new Color(50, 50, 50)
      g.setStroke(dragStroke)
      g.setColor(dragColor)
      g.drawRect(dragStart.x * hscale, dragStart.y * vscale, hscale, vscale)

      if (this.hoverPoint != this.dragStart) {
        g.setStroke(selectionStroke)
        g.drawRect(hoverPoint.x * hscale, hoverPoint.y * vscale, hscale, vscale)

        g.setColor (dragLineColor)
        // Draw an arrow to show what is moving where
        g.drawLine (hscale/2 + dragStart.x * hscale,
                    vscale/2 + dragStart.y * vscale,
                    hscale/2 + hoverPoint.x * hscale,
                    vscale/2 + hoverPoint.y * vscale)
      }
    }
  }

  /**
   * Apparently, the time parameter is not used anyway?
   */
  def repaint(tm: Long) {
    repaint()
  }

  /**
   * Update objects to display.
   */
  def setTrace(segments: List[Segment], gates: List[Gate]) {
    this.segments = segments
    this.gates = gates

    // establish what to render on paint
    import scala.collection.mutable.{Builder, ListBuffer}
    // could view todo as either Buffer and use Buffer.toList, or Builder and use Builder.result
    val todo: Builder[Renderable, List[Renderable]] = ListBuffer(Grid)
    todo += new RayRenderer(segments)
    todo += new RenderingHint()
    gates.foreach (gate => {
      todo += widgetForGate(gate)
    })
    this.renderables = todo.result()
  }

  def setGridBounds(b: Bound) {
    this.gridBounds = b
    calculateScale()
  }

  private def widgetForGate(gate: Gate): Renderable = {
    gate match {
      case x: Mirror => new MirrorWidget(x)
      case x: PartialMirror => new PartialMirrorWidget(x)
      case x: CrossMirror => new CrossMirrorWidget(x)
      case x: Detector => new DetectorWidget(x)
      case x: Source => new SourceWidget(x)
      case x: Blocker => new BlockerWidget(x)
      case x: Prism => new PrismWidget(x)
      case x: Conduit => new ConduitWidget(x)
      case x: WormHole => new WormHoleWidget(x)
    }
  }

  /** Any object that is rendered. */
  trait Renderable {
    def render(g: Graphics2D)
  }

  /** Various renderable objects. */
  abstract class Widget(val position: Point) extends Renderable {

    final protected def drawLine(g: Graphics2D, p1: Point, p2: Point) = g.drawLine(p1.x, p1.y, p2.x, p2.y)

  }

  abstract class GateWidget(val gate: Gate) extends Widget(gate.position) {

    /** Scaled position in the current context. */
    def scaledPosition = Point(hscale/2 + this.position.x * hscale, vscale/2 + this.position.y * vscale)

    def isMoveable = this.gate.isInstanceOf[Moveable]

    override def render(g: Graphics2D) {
      if (!isMoveable) {
        // Draw some bolts around it. For now, just an oval will do
        g.setColor(gridColor)
        g.setStroke(thinStroke)
        g.drawOval(scaledPosition.x - hscale/2, scaledPosition.y - hscale/2, hscale, vscale)
      }
    }
  }

  object Grid extends Renderable {
    val boundColor = new Color(10, 10, 50)
    override def render(g: Graphics2D) {
      g.setColor(LightBox.this.gridColor)
      g.setStroke(LightBox.this.thinStroke)
      val h = LightBox.this.hscale
      val v = LightBox.this.vscale
      val side = LightBox.this.boxside
      0.until(LightBox.this.gridBounds.width).foreach(i => { g.drawLine(i * h, 0, i * h, side) })
      0.until(LightBox.this.gridBounds.height).foreach(i => { g.drawLine(0, i * v, side, i * v) })

      // Draw the boundary
      g.setStroke(LightBox.this.thickStroke)
      g.setColor(this.boundColor)
      g.drawRect(LightBox.this.gridBounds.x * h,
              LightBox.this.gridBounds.y * v,
              LightBox.this.gridBounds.width * h,
              LightBox.this.gridBounds.height * v)
    }
  }

  class RayRenderer(segments: List[Segment]) extends Renderable {
    override def render(g: Graphics2D) {
      val oldComposite = g.getComposite
      try {
        g.setComposite(AddComposite)
        g.setStroke(thinStroke)
        this.segments.foreach(line => {
          g.setColor(line.color.color)
          g.drawLine(hscale/2 + line.start.x * hscale,
                      vscale/2 + line.start.y * vscale,
                      hscale/2 + line.end.x * hscale,
                      // Math.min(hscale/2 + line.end.x * hscale, d.width - (d.width % hscale)),
                      vscale/2 + line.end.y * vscale)
        })
      } finally {
        g.setComposite(oldComposite)
      }
    }
  }

  class RenderingHint extends Renderable {
    override def render(g: Graphics2D) {
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    }
  }

  class MirrorWidget(m: Mirror) extends GateWidget(m) {
    val mirrorColor = new Color(120, 120, 140)
    val mirrorBackColor = new Color(40, 60, 60)
    override def render(g: Graphics2D) {
      super.render(g)
      val faceDirection = m.direction.left90
      val p = scaledPosition

      g.setColor(mirrorColor)
      g.setStroke(mediumStroke)
      drawLine(g, p.nextPoint(faceDirection, hscale/3, vscale/3), p.nextPoint(faceDirection.reverse, hscale/3, vscale/3))

      // Draw the back plane
      g.setColor(mirrorBackColor)
      g.setStroke(thinStroke)
      val backP = p.nextPoint(m.direction.reverse, hscale/6, vscale/6)
      drawLine(g, backP.nextPoint(faceDirection, hscale/3, vscale/3), backP.nextPoint(faceDirection.reverse, hscale/3, vscale/3))
      val backP2 = p.nextPoint(m.direction.reverse, hscale/6 - 2, vscale/6 - 2)
      drawLine(g, backP2.nextPoint(faceDirection, hscale/3, vscale/3), backP2.nextPoint(faceDirection.reverse, hscale/3, vscale/3))
    }
  }

  class PartialMirrorWidget(m: PartialMirror) extends GateWidget(m) {
    val partMirrorColor = new Color(100, 120, 140)
    val partMirrorBorderColor = new Color(150, 150, 150)
    override def render(g: Graphics2D) {
      super.render(g)
      val faceDirection = m.direction.left90
      val p = scaledPosition

      val point1 = p.nextPoint(faceDirection, hscale/3, vscale/3)
      val point2 = p.nextPoint(faceDirection.reverse, hscale/3, vscale/3)

      g.setColor(partMirrorColor)
      g.setStroke(wideStroke)
      drawLine(g, point1, point2)

      val point1edge = p.nextPoint(faceDirection, hscale/3 + widePixels/2, vscale/3 + widePixels/2)
      val point2edge = p.nextPoint(faceDirection.reverse, hscale/3 + widePixels/2, vscale/3 + widePixels/2)
      g.setColor(partMirrorBorderColor)
      g.setStroke(normStroke)
      drawLine(g, point1edge.nextPoint(faceDirection.right90, widePixels, widePixels),
                  point1edge.nextPoint(faceDirection.left90, widePixels, widePixels))
      drawLine(g, point2edge.nextPoint(faceDirection.right90, widePixels, widePixels),
                  point2edge.nextPoint(faceDirection.left90, widePixels, widePixels))
    }
  }

  class CrossMirrorWidget(m: CrossMirror) extends GateWidget(m) {
    val crossMirrorColor = new Color(90, 90, 140)
    val mirrorBackColor = new Color(40, 60, 60)
    val handleShape = {
      val path = new java.awt.geom.Path2D.Float
      path.moveTo(0.4, 0)
      path.lineTo(0.6, 0)
      path.lineTo(0.6, 0.5)
      path.lineTo(-0.6, 0.5)
      path.lineTo(-0.6, 0)
      path.lineTo(-0.4, 0)
      path.lineTo(-0.4, 0.3)
      path.lineTo(0.4, 0.3)
      path.lineTo(0.4, 0)
      path
    }
    override def render(g: Graphics2D) {
      super.render(g)
      val p = scaledPosition

      val angle = (East.angle(m.direction) + 22.5).toRadians
      val complement = ((90 - East.angle(m.direction)) - 22.5).toRadians
      val hlen = (hscale * scala.math.sin(angle)).toInt / 3
      val vlen = (hscale * scala.math.cos(angle)).toInt / 3

      // Draw the back plane
      g.setColor(mirrorBackColor)
      val scaleTransform = java.awt.geom.AffineTransform.getScaleInstance(hscale/2,vscale/2)
      val transTransform = java.awt.geom.AffineTransform.getTranslateInstance(p.x,p.y)
      val rotTransform   = java.awt.geom.AffineTransform.getRotateInstance(complement)
      val modShape = scaleTransform.createTransformedShape(handleShape)
      val modShape2 = rotTransform.createTransformedShape(modShape)
      val modShape3 = transTransform.createTransformedShape(modShape2)
      g.fill(modShape3)
/*
      g.setStroke(thinStroke)
      val backP = p.nextPoint(m.direction.reverse, hscale/6, vscale/6)
      drawLine(g, Point(backP.x + hlen, backP.y + vlen), Point (backP.x - hlen, backP.y - vlen))
      val backP2 = p.nextPoint(m.direction.reverse, hscale/6 - 2, vscale/6 - 2)
      drawLine(g, Point(backP2.x + hlen, backP2.y + vlen), Point (backP2.x - hlen, backP2.y - vlen))
*/

      g.setColor(crossMirrorColor)
      g.setStroke(mediumStroke)

      drawLine(g, Point(p.x + hlen, p.y + vlen), Point (p.x - hlen, p.y - vlen))
    }
  }

  class DetectorWidget(d: Detector) extends GateWidget(d) {
    override def render(g: Graphics2D) {
      super.render(g)
      val p = scaledPosition
      val detectorColor = if (d.wavelength != Black) d.wavelength.color else bgColor
      g.setStroke(thickStroke)
      if (d.isOn) {
        g.setColor(detectorColor)
        g.drawOval(p.x - hscale/4, p.y - vscale/4, hscale/2, vscale/2)
        g.fillOval(p.x - hscale/4, p.y - vscale/4, hscale/2, vscale/2)
      } else {
        g.setColor(detectorColor.darker.darker)
        g.drawOval(p.x - hscale/4, p.y - vscale/4, hscale/2, vscale/2)
      }

      g.setStroke(normStroke)
      g.setColor(gridColor)
      g.drawOval(p.x - hscale/4 - 1, p.y - vscale/4 - 1, hscale/2 + 2, vscale/2 + 2)
      drawLine(g, p.nextPoint(Northeast, hscale/4, vscale/4), p.nextPoint(Southwest, hscale/4, vscale/4))
      drawLine(g, p.nextPoint(Southeast, hscale/4, vscale/4), p.nextPoint(Northwest, hscale/4, vscale/4))
    }
  }

  class SourceWidget(s: Source) extends GateWidget(s) {
    val sourceColor = new Color(150, 150, 150)
    override def render(g: Graphics2D) {
      super.render(g)
      val p = scaledPosition

      val centerOval = p.nextPoint(s.direction.reverse, hscale/4, vscale/4)
      val leftOfCenter = centerOval.nextPoint(s.direction.left90, hscale/8, vscale/8)
      val rightOfCenter = centerOval.nextPoint(s.direction.right90, hscale/8, vscale/8)

      g.setColor(sourceColor)
      g.setStroke(thinStroke)
      g.fillOval(centerOval.x - hscale/8, centerOval.y - vscale/8, hscale/4, vscale/4)
      drawLine(g, leftOfCenter, leftOfCenter.nextPoint(s.direction, hscale/2, vscale/2))
      drawLine(g, rightOfCenter, rightOfCenter.nextPoint(s.direction, hscale/2, vscale/2))

      g.setColor(s.color.color)
      g.fillOval(centerOval.x - hscale/10, centerOval.y - vscale/10, hscale/5, vscale/5)
    }
  }

  class BlockerWidget(b: Blocker) extends GateWidget(b) {
    val blockerColor = new Color(42, 42, 42)
    val blockerLightColor = new Color(62, 62, 62)
    val blockerDarkColor = new Color(12, 12, 12)
    override def render(g: Graphics2D) {
      super.render(g)
      val p = scaledPosition
      g.setColor(blockerColor)
      val topLeft     = Point(p.x - hscale/2 + thinPixels, p.y - vscale/2 + thinPixels)
      val topRight    = Point(p.x + hscale/2 - thinPixels, p.y - vscale/2 + thinPixels)
      val bottomRight = Point(p.x + hscale/2 - thinPixels, p.y + vscale/2 - thinPixels)
      val bottomLeft  = Point(p.x - hscale/2 + thinPixels, p.y + vscale/2 - thinPixels)

      g.fillRect(topLeft.x - thinPixels, topLeft.y - thinPixels, hscale, vscale)

      g.setStroke(thinStroke)
      g.setColor(blockerLightColor)
      drawLine(g, topLeft, topRight)
      drawLine(g, topLeft, bottomLeft)

      g.setStroke(thinStroke)
      g.setColor(blockerDarkColor)
      drawLine(g, topRight, bottomRight)
      drawLine(g, bottomLeft, bottomRight)
    }
  }

  class PrismWidget(z: Prism) extends GateWidget(z) {
    val mirrorColor = new Color(120, 120, 140)
    val mirrorBackColor = new Color(40, 60, 60)
    override def render(g: Graphics2D) {
      super.render(g)
      val p = scaledPosition
      val faceDirection = z.direction.left90
      val topPoint = p.nextPoint(z.direction.reverse, hscale/4, vscale/4)
      val bottomPoint = p.nextPoint(z.direction, hscale/4, vscale/4)
      val bottomLeftPoint = bottomPoint.nextPoint(faceDirection, hscale/3, vscale/3)
      val bottomRightPoint = bottomPoint.nextPoint(faceDirection.reverse, hscale/3, vscale/3)

      g.setColor(mirrorColor)
      g.setStroke(mediumStroke)
      drawLine(g, bottomLeftPoint, bottomRightPoint)
      g.setStroke(thinStroke)
      drawLine(g, bottomLeftPoint, topPoint)
      drawLine(g, bottomRightPoint, topPoint)
    }
  }

  class ConduitWidget(c: Conduit) extends GateWidget(c) {
    val conduitColor = new Color(100, 100, 100)
    override def render(g: Graphics2D) {
      super.render(g)
      val p = scaledPosition
      val sideDirection = c.direction.left90
      // Draw the sides
      g.setColor(conduitColor)
      g.setStroke(thinStroke)
      val backP = p.nextPoint(sideDirection, hscale/8, vscale/8)
      drawLine(g, backP.nextPoint(c.direction, hscale/3, vscale/3), backP.nextPoint(c.direction.reverse, hscale/3, vscale/3))
      val backP2 = p.nextPoint(sideDirection.reverse, hscale/8, vscale/8)
      drawLine(g, backP2.nextPoint(c.direction, hscale/3, vscale/3), backP2.nextPoint(c.direction.reverse, hscale/3, vscale/3))
    }
  }

  class WormHoleWidget(w: WormHole) extends GateWidget(w) {
    val wormHoleColor = new Color(28, 57, 13)
    val wormHoleInnerColor = new Color(42, 21, 21)
    val wormHoleOuterColor = new Color(150, 150, 150)
    override def render(g: Graphics2D) {
      super.render(g)
      val p = scaledPosition
      g.setColor(wormHoleOuterColor)
      g.setStroke(normStroke)
      g.drawOval(p.x - (3*hscale)/8, p.y - (3*vscale)/8, 3*hscale/4, 3*vscale/4)

      g.setColor(wormHoleColor)
      g.setStroke(normStroke)
      g.drawOval(p.x - hscale/4, p.y - vscale/4, hscale/2, vscale/2)

      g.setColor(wormHoleInnerColor)
      g.setStroke(mediumStroke)
      g.drawOval(p.x - hscale/8, p.y - vscale/8, hscale/4, vscale/4)

      // g.fillRect(p.x - 3*hscale/8, p.y - 3*vscale/8, 3*hscale/4, 3*vscale/4)
      // g.fillOval(p.x - hscale/4, p.y - vscale/4, hscale/2, vscale/2)
    }
  }
}

case class DragEvent(from: Point, to: Point) extends Event

case class ClickEvent(where: Point, button: MouseButtons.MouseButton) extends Event

object MouseButtons extends Enumeration {
  type MouseButton = Value
  val Left, Right, Middle = Value
  import java.awt.event.MouseEvent._
  def forEvent(e: MouseButtonEvent): MouseButton = e.peer.getButton match {
    case BUTTON1 => Left
    case BUTTON2 => Middle
    case BUTTON3 => Right
    case x => Left //No good can come of this
  }
}

import java.awt.{CompositeContext, RenderingHints}
import java.awt.image.{ColorModel, ComponentColorModel, Raster, WritableRaster, IndexColorModel, PackedColorModel}

object AddComposite extends java.awt.Composite {
  import scala.math.{min,max}
  class AddCompositePackedContext extends CompositeContext {
    def compose(src: Raster, dst: Raster, w: WritableRaster) = {
      val minX = max(src.getMinX, dst.getMinX)
      val maxX = minX + min(src.getWidth, dst.getWidth)
      val minY = max(src.getMinY, dst.getMinY)
      val maxY = minY + min(src.getHeight, dst.getHeight)
      for (i <- minX until maxX; j <- minY until maxY; band <- 0 until min(src.getNumBands, dst.getNumBands)) {
        val value = min(src.getSample(i,j,band) + dst.getSample(i,j,band), 255)
        w.setSample(i, j, band, value)
      }
    }
    def dispose = {
      //empty
    }
  }

  def createContext (cm: ColorModel, cm2: ColorModel, rh: RenderingHints): CompositeContext = new AddCompositePackedContext
}
