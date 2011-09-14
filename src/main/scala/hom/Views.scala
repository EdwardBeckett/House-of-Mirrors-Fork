
package hom

import swing._
import swing.event._

import react.Observing

trait ViewComponents {
  this: ControllerComponents with ModelComponents =>
  val lightBox: LightBox
  val statusBar: Label
  val statusIcon: Label
  val frame: Frame

  /**
   * Handles update notifications from the application
   * and user input from the LightBox.
   */
  class LightBoxMediator(private val ui: LightBox) extends Reactor with Observing {

    /** Attempt to track our selection across updates: the point is where the gate should end up. */
    private var selectionContinuity: (Option[Gate], Option[Point]) = (None, None)

    val helpText = "<html><h1>Keyboard shortcuts</h1><br/><br/><dl>" + 
      "<dt>n (p)</dt><dd>Select next (previous) gate<br/><br/></dd>" +
      "<dt>SPACE (ENTER)</dt><dd>Rotate the selected element Counter-clockwise (Clockwise)<br/><br/></dd>" +
      "<dt>UP,DOWN,LEFT,RIGHT</dt><dd>Move the selected element<br/><br/></dd>" +
      "<dt>PAGE_UP (PAGE_DOWN)</dt><dd>Go to next (previous) level<br/><br/></dd>" +
      "</dl></html>"

    listenTo(ui, ui.keys, ui.mouse.clicks)

    reactions += {
      case KeyPressed(_, Key.Q, _, _) => sys.exit()
      case KeyPressed(_, Key.Space, _, _) => rotateSelectedGate()
      case KeyPressed(_, Key.Enter, _, _) => rotateOtherwiseSelectedGate()
      case KeyPressed(_, Key.Up, _, _) => moveUp()
      case KeyPressed(_, Key.Down, _, _) => moveDown()
      case KeyPressed(_, Key.Left, _, _) => moveLeft()
      case KeyPressed(_, Key.Right, _, _) => moveRight()
      case KeyPressed(_, Key.PageUp, _, _) => previousLevel()
      case KeyPressed(_, Key.PageDown, _, _) => nextLevel()
      case DragEvent(from, to) => handleDrag(from, to)
      case ClickEvent(where, button) => handleClick(where, button)
      //case x => println("Unreacted event " + x)
    }

    observe(controller.modelEvents) { e => e match {
        case LevelLoaded(v) => onLevelLoaded(v)
        case TraceResult(s) => onTrace(s)
        case Help => Dialog.showMessage(ui, helpText)
        case unknown => println("Lightbox mediator ignored: "+ unknown)
      }
      true
    }

    /**
     * When a level is loaded, select the first moveable gate.
     */
    private def onLevelLoaded(g: GameLevel) {
      this.ui.currentSelection = if (g.moveable_gates.length > 0) Some(g.moveable_gates(0)) else None
    }

    private def onTrace(state: GameState) {
      ui.setGridBounds(model.level.bounds)
      ui.setTrace(state.segments, state.gates)
      ui.completed = state.status.score == state.status.total
      if (this.selectionContinuity._1.isDefined) {
        ui.selectAt(this.selectionContinuity._1.get, this.selectionContinuity._2.get)
        this.selectionContinuity = (None, None)
      }
      if (!this.ui.currentSelection.isDefined) {
        this.ui.select(state.gates.find(x => x.isInstanceOf[Moveable]))
      }
      ui.repaint()
    }

    /**
     * Anticipate that after the next update (trace),
     * the current selection should be at p and be similar
     * to g.
     */
    private def continuity(g: Option[Gate], p: Point) {
      this.selectionContinuity = (g, Some(p))
    }

    /** CounterClockwise */
    private def rotateSelectedGate() {
      if (this.ui.currentSelection.isDefined) {
        val p: Point = this.ui.currentSelection.get.position
        if (model.level.turnCCW(p)) {
          continuity(this.ui.currentSelection, p);
          controller ! LevelUpdate
        }
      }
    }
    /** Otherwise means Clockwise */
    private def rotateOtherwiseSelectedGate() {
      if (this.ui.currentSelection.isDefined) {
        val p: Point = this.ui.currentSelection.get.position
        if (model.level.turnCW(p)) {
          continuity(this.ui.currentSelection, p);
          controller ! LevelUpdate
        }
      }
    }
    private def moveUp() {
      if (this.ui.currentSelection.isDefined) {
        val p: Point = this.ui.currentSelection.get.position
        if (model.level.moveUp(p)) {
          continuity(this.ui.currentSelection, p.moveUp);
          controller ! LevelUpdate
        }
      }
    }
    private def moveDown() {
      if (this.ui.currentSelection.isDefined) {
        val p: Point = this.ui.currentSelection.get.position
        if (model.level.moveDown(p)) {
          continuity(this.ui.currentSelection, p.moveDown);
          controller ! LevelUpdate
        }
      }
    }
    private def moveLeft() {
      if (this.ui.currentSelection.isDefined) {
        val p: Point = this.ui.currentSelection.get.position
        if (model.level.moveLeft(p)) {
          continuity(this.ui.currentSelection, p.moveLeft);
          controller ! LevelUpdate
        }
      }
    }
    private def moveRight() {
      if (this.ui.currentSelection.isDefined) {
        val p: Point = this.ui.currentSelection.get.position
        if (model.level.moveRight(p)) {
          continuity(this.ui.currentSelection, p.moveRight);
          controller ! LevelUpdate
        }
      }
    }

    private def previousLevel() {
      controller ! PreviousLevel
    }

    /**
     * User request to advance to next level.
     * The level must be unlocked.
     * The current level need not be in a completed state.
     */
    private def nextLevel() {
      controller ! NextLevel
    }

    private def handleDrag(from: Point, to: Point) {
      if (model.level.moveTo(from, to)) {
        continuity(this.ui.currentSelection, to)
        controller ! LevelUpdate
      }
    }

    /**
     * Mouse click means rotate gate or next/previous level.
     */
    import MouseButtons._
    private def handleClick(where: Point, button: MouseButton) {
      if (button == Left) {
        if (this.ui.occupied(where)) {
          //todo don't mutate
          if (model.level.turnCCW(where)) {
            continuity(this.ui.currentSelection, where);
            controller ! LevelUpdate
          }
        } else {
          nextLevel()
        }
      } else if (button == Right) {
        if (this.ui.occupied(where)) {
          if (model.level.turnCW(where)) {
            continuity(this.ui.currentSelection, where);
            controller ! LevelUpdate
          }
        } else {
          previousLevel()
        }
      }
    }
  }

  class StatusBarMediator(statusBar: Label) extends Observing {

    observe(controller.modelEvents) { e => e match {
        case TraceResult(s) => onStatusUpdate(s.status)
        case ErrorMessage(m) => message(null, "Yikes!", m)
        case unknown => println("StatusBar mediator ignored: "+ unknown)
      }
      true
    }

    import scala.xml._
    private def onStatusUpdate(s: StatusUpdate) {
      message(XML.loadString("<p>"+s.description+"</p>"), "Progress", s.score + " of " + s.total + (if (s.isComplete) " [Completed!]" else ""))
    }

    private def message(title: Elem, header: String, s: String) {
      val t = if (title == null) new Text("") else title
      val xhtml = <html>{ t } <br/><br/><hr/><font size="+1">{ header }</font><br/> { s } </html>
      statusBar.text = Xhtml.toXhtml(xhtml)
      statusBar.repaint
    }
  }

  class StatusIconMediator(statusIcon: Label) extends Observing {
    observe(controller.modelEvents) { e => e match {
        case TraceResult(s) => statusIcon.enabled = s.status.isComplete
        case unknown => println("StatusIcon mediator ignored: "+ unknown)
      }
      true
    }
  }
}

