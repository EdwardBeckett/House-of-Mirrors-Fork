
package hom

import org.puremvc.java.interfaces.INotification
import org.puremvc.java.patterns.mediator.Mediator

import scala.swing.{Reactor, Dialog}
import scala.swing.event._

import notes.HouseNotifications._

/**
 * Handles update notifications from the application
 * and user input from the LightBox.
 */
abstract class LightBoxMediator (c: Object) extends Mediator("ui-mediator", c) with Reactor {
  require(c.isInstanceOf[LightBox])

  val levelProxy: GameLevelProxy
  val packProxy: GamePackProxy

  override val listNotificationInterests = asNames(List(LevelLoaded, Trace, Help))

  private def ui: LightBox = getViewComponent.asInstanceOf[LightBox]

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
    case ClickEvent(where, op) => handleClick(where, op)
    //case x => println("Unreacted event " + x)
  }

  override def handleNotification(n: INotification) {
    notes.HouseNotifications findName n.getName match {
      case Some(LevelLoaded) => onLevelLoaded(n.getBody.asInstanceOf[GameLevel])
      case Some(Trace) => onTrace(n.getBody.asInstanceOf[GameState])
      case Some(Help) => Dialog.showMessage(ui, helpText)
      case None => // it's not one of our notifications
      case _ => // didn't ask for anything else
    }
  }

  /**
   * When a level is loaded, select the first moveable gate.
   */
  private def onLevelLoaded(g: GameLevel) {
    this.ui.currentSelection = if (g.moveable_gates.length > 0) Some(g.moveable_gates(0)) else None
  }

  private def onTrace(state: GameState) {
    ui.setGridBounds(this.levelProxy.gameLevel.bounds)
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
      if (this.levelProxy.gameLevel.turnCCW(p)) {
        continuity(this.ui.currentSelection, p);
        sendNotification(LevelUpdate.toString, null, null)
      }
    }
  }
  /** Otherwise means Clockwise */
  private def rotateOtherwiseSelectedGate() {
    if (this.ui.currentSelection.isDefined) {
      val p: Point = this.ui.currentSelection.get.position
      if (this.levelProxy.gameLevel.turnCW(p)) {
        continuity(this.ui.currentSelection, p);
        sendNotification(LevelUpdate.toString, null, null)
      }
    }
  }
  private def moveUp() {
    if (this.ui.currentSelection.isDefined) {
      val p: Point = this.ui.currentSelection.get.position
      if (this.levelProxy.gameLevel.moveUp(p)) {
        continuity(this.ui.currentSelection, p.moveUp);
        sendNotification(LevelUpdate.toString, null, null)
      }
    }
  }
  private def moveDown() {
    if (this.ui.currentSelection.isDefined) {
      val p: Point = this.ui.currentSelection.get.position
      if (this.levelProxy.gameLevel.moveDown(p)) {
        continuity(this.ui.currentSelection, p.moveDown);
        sendNotification(LevelUpdate.toString, null, null)
      }
    }
  }
  private def moveLeft() {
    if (this.ui.currentSelection.isDefined) {
      val p: Point = this.ui.currentSelection.get.position
      if (this.levelProxy.gameLevel.moveLeft(p)) {
        continuity(this.ui.currentSelection, p.moveLeft);
        sendNotification(LevelUpdate.toString, null, null)
      }
    }
  }
  private def moveRight() {
    if (this.ui.currentSelection.isDefined) {
      val p: Point = this.ui.currentSelection.get.position
      if (this.levelProxy.gameLevel.moveRight(p)) {
        continuity(this.ui.currentSelection, p.moveRight);
        sendNotification(LevelUpdate.toString, null, null)
      }
    }
  }

  private def previousLevel() {
    val n: Int =  this.levelProxy.gameLevel.level - 1
    if (n >= 0) {
      sendNotification(LoadLevel.toString, n, null)
    }
  }

  /**
   * User request to advance to next level.
   * The level must be unlocked.
   * The current level need not be in a completed state.
   */
  private def nextLevel() {
    sendNotification(LoadLevel.toString, -1, null)
  }

  private def handleDrag(from: Point, to: Point) {
    if (this.levelProxy.gameLevel.moveTo(from, to)) {
      continuity(this.ui.currentSelection, to)
      sendNotification(LevelUpdate.toString, null, null)
    }
  }

  /**
   * Mouse click means rotate gate or next/previous level.
   */
  private def handleClick(where: Point, op: Int) {
    if (op == 0) {
      if (this.ui.occupied(where)) {
        if (this.levelProxy.gameLevel.turnCCW(where)) {
          continuity(this.ui.currentSelection, where);
          sendNotification(LevelUpdate.toString, null, null)
        }
      } else {
        previousLevel()
      }
    } else if (op == 1) {
      if (this.ui.occupied(where)) {
        if (this.levelProxy.gameLevel.turnCW(where)) {
          continuity(this.ui.currentSelection, where);
          sendNotification(LevelUpdate.toString, null, null)
        }
      } else {
        nextLevel()
      }
    }
  }
}
