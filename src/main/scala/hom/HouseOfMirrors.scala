package hom

/** Application-wide configuration and utility methods. */
object HouseApplication {

  def apperror(message: String): Nothing = throw new ApplicationException(message)

  val DefaultGamePack = "rowhouses/rowhouse.homp"

}

class ApplicationException(m: String) extends RuntimeException(m)

import java.io.File
import swing._

/** Application entry point initializes the application and loads the default game. */
object HouseOfMirrors extends SwingApplication with HouseComponents with GameIO {

  val controller = new GameController
  val loader = new GameLoader

  var model = new GameModel(null, null, new GameHistory)

  lazy val lightBox = ViewDefaults.lightBox
  lazy val statusBar = ViewDefaults.statusBar
  lazy val statusIcon = ViewDefaults.statusIcon

  // creating frame also evaluates other components
  lazy val frame = new MainFrame {
    title = "Mouse of Horrors"
    menuBar = new MenuBar {
      contents += (new Menu("File") {
        contents += (new MenuItem(Action("Open...") { openFile() }))
        contents += (new MenuItem(Action("Save...") { saveFile() }))
        contents += (new MenuItem(Action("Save Game") { saveGame() }))
        contents += (new MenuItem(Action("Quit") { quit() }))
      })
      contents += (new Menu("Game") {
        contents += (new MenuItem(Action("Reset") { controller ! ResetLevel }))
      })
      contents += (new MenuItem(Action("Help") { controller ! Help }))
    }
    contents = new BoxPanel (Orientation.Horizontal) {
      contents += lightBox
      contents += new BoxPanel(Orientation.Vertical) {
        contents += statusBar
        contents += statusIcon
      }
    }
  }

  override def startup(args: Array[String]) {

    construct()
    val m1 = new LightBoxMediator(lightBox)
    val m2 = new StatusBarMediator(statusBar)
    val m3 = new StatusIconMediator(statusIcon)

    controller.start()
    controller ! Start
  }

  override def shutdown() {
    controller ! Stop
    loader ! Stop
    frame.dispose()
    Thread.sleep(100)
  }

  /** Construct the UI. */
  private def construct() {
    if (frame.size == new java.awt.Dimension(0,0)) frame.pack()
    frame.centerOnScreen()
    frame.visible = true
    lightBox.requestFocus
  }
}

object ViewDefaults {
  def lightBox = new LightBox
  def statusBar = new Label("HoM") { preferredSize = new java.awt.Dimension(250, 100) }

  import javax.swing.ImageIcon
  import javax.swing.border.EmptyBorder
  def statusIcon = new Label() {icon = new ImageIcon(Resource("images/thumbsUp.png").uri.toURL); border = new EmptyBorder(20,10,10,10)}
}

import actors.Actor
import actors.Actor._

trait HouseComponents extends ControllerComponents with ModelComponents with ViewComponents

import scala.react.{ Events, EventSource }

// react infrastructure really wants to be run by one thread (not just single-threaded propagation)
object HouseEventSource {
  import java.util.concurrent._
  val x = Executors.newSingleThreadExecutor
  def shutdown() { x.shutdown() }
  def submit(s: () => Unit) { x.submit(new Runnable { override def run { s() }})}
}

class HouseEventSource[A <: HouseMessage] extends EventSource[A] {
  import HouseEventSource._
  def raise(m: A) {
    submit { () => emit(m) }
  }
}

trait ControllerComponents {
  this: ModelComponents =>
  val controller: GameController
  val loader: GameLoader

  class GameController extends Actor {

    private[ControllerComponents] val emitter = new HouseEventSource[HouseMessage]
    val modelEvents: Events[HouseMessage] = emitter

    override def act() {
      loop {
        react {
          case Start => println("Starting up"); loader ! LoadGamePackResource(HouseApplication.DefaultGamePack, Some(1))
          case Stop => println("Controller: Shutting down"); HouseEventSource.shutdown(); exit()
          case Help => emitter.raise(Help)
          case PackLoaded(p) => model = model.copy(pack = p, history = new GameHistory); model.debug(); loadLevel(0, true)
          case LevelLoaded(v) => model = model.copy(level = v); emitter.raise(LevelLoaded(v)); model.debug(); update()
          case NextLevel => loadLevel(model.level.level + 1, true)
          case PreviousLevel => if (model.level.level > 0) loadLevel(model.level.level - 1, true)
          case ResetLevel => loadLevel(model.level.level, false)
          case LevelUpdate => update()
          case unknown => println("Controller: Unhandled message: " + unknown)
        }
      }
    }

    def update() {
      val state: GameState = model.level.trace
      emitter.raise(TraceResult(state))
      if (state.status.isComplete) {
        if (model.pack.unlockAll(model.level.level)) { println("New levels unlocked.") }
        if (!model.history.hasGameLevel(model.level)) {
          model.history.putGameLevel(model.level)
        }
      }
    }

  /*
  // if level < 0, find the min unlocked level
  private def emitLoadLevel(level: Int) {
    val i = if (level < 0) {
        this.packProxy.gamePack.unlockedLevels.foldLeft(Integer.MAX_VALUE) { (x,y) => x.min(y) }
      } else level
  }
  */

    private def loadLevel(request: Int, withHistory: Boolean) {
      val which: Int = if (request >= 0) request else model.level.level + 1
      if (model.pack.isUnlocked(which)) {
        if (withHistory && model.history.hasGameLevel(which)) {
          loadLevelFromXml(model.history.history.get(which).get, which)
        } else {
          loader ! LoadGameLevelResource(model.pack.levelResource(which), which)
        }
      } else {
        emitter.raise(ErrorMessage("Your way to level " + which + " is blocked by a locked door!"))
      }
    }
    import scala.xml._
    private def loadLevelFromXml(xml: Elem, n: Int) {
      self ! LevelLoaded(GameLevel.fromXML(xml, n))
    }
  }

  class GameLoader extends Actor with SaveFileCommand with SaveGameCommand {
    start()
    override def act() {
      loop {
        react {
          case LoadGamePackResource(name, level) => reply(PackLoaded(GamePack(name)))
          case LoadGameLevelResource(resource, level) => reply(LevelLoaded(GameLevel(resource, level)))
          case OpenFile(f) => openFile(f)
          case SaveFile(f) => saveFile(f)
          case SaveGame(f) => saveGame(f)
          case Stop => println("Loader: Shutting down"); exit()
          case unknown => println("Loader: Unhandled message: " + unknown)
        }
      }
    }
    private def openFile(f: File) {
      import scala.xml._
      val OptFileExt = """.*?(\..*)?""".r
      val OptFileExt(x) = f.getName
      x match {
        case ".hom" => // loading level file not supported yet
        case ".homp" => controller ! PackLoaded(GamePack(f))     // load pack and start at lowest level
        case _ => // don't know what that is
      }
    }
  }

  /** Base class of commands that read/write *.hom[p] files. */
  trait FileCommand extends WithFileWriter {
    private val printWidth = 120
    private val printTab = 2

    protected final def prettyPrinter = new PrettierPrinter(printWidth, printTab)
  }

  /** Save the current game to a file. */
  trait SaveFileCommand extends FileCommand {
    def saveFile(f: File) {
      if (f.exists) {
        controller.emitter.raise(ErrorMessage("File exists: " + f.getAbsolutePath))
      } else {
        f.createNewFile
        val xml = model.level.toXML
        withWriter(f) {
          w => w.write(prettyPrinter.format(xml))
        }
      }
    }
  }

  /**
   * Save the game pack with history.
   * For pack file "foo.homp", solution file for level N is "foo_N.hom".
   */
  trait SaveGameCommand extends FileCommand {
    import scala.xml._
    import java.io._
    def saveGame(f: File) {
      def noext(s: String): String = if (s.lastIndexOf('.') > 0) s.substring(0, s.lastIndexOf('.')) else s
      def solutionFileName(simple: String, id: String): String = simple + "_" + id + ".hom"
      //def solutionFileName(id: String): String = noext(f.getName) + "_" + id + ".hom"
      if (f.exists) {
        controller.emitter.raise(ErrorMessage("File exists: " + f.getAbsolutePath))
        return;
      }
      val base = Resource(f)
      val namer = solutionFileName(noext(f.getName), _: String)
      writeSolutionFiles(base, namer)
      // update the game xml with our solution file names
      val packElem: Elem = updatePack(model.pack.toXML, namer)
      withWriter(f) {
        w => w.write(prettyPrinter.format(packElem))
      }
    }

    private def writeSolutionFiles(base: Resource, namer: (String) => String) {
      for ((level, xml) <- model.history.history) {
        val name = namer(level.toString)
        val r = Resource(base, name)
        withWriter(r.asFile) {
          w => w.write(prettyPrinter.format(xml))
        }
      }
    }

    // to each level <HoMPack><level id="5" file="..."><unlock id="6"/> add child <level ...><solution file="mygame-5.hom"/>
    private def updatePack(input: Elem, namer: (String) => String): Elem = {
      def id(n: Node): String = { val s = n \ "@id"; s(0).asInstanceOf[Text].data }
      def hasHistoryFor(n: Node): Boolean = {
        def hasHistoryFor(i: Int) = model.history.hasGameLevel(i)
        require(n.label == "level")
        hasHistoryFor(Integer.parseInt(id(n)))
      }
      def addChild(n: Node, e: Node) = Elem(n.prefix, n.label, n.attributes, n.scope, (n.child ++ e):_*)
      def addSolution(n: Node, f: String) = addChild(n, Elem(null, "solution", new UnprefixedAttribute("file", f, Null), TopScope))
      def updateLevels(ch: Seq[Node]): Seq[Node] = {
        for (c <- ch) yield c match {
          case x @ <level>{ _* }</level> if hasHistoryFor(x) => addSolution(x, namer(id(x)))
            //addChild(x, Elem(null, "solution", new UnprefixedAttribute("file", solutionFileName(simple, id(x)), Null), TopScope))
          case x => x  // <unlocked> or <level> that's not in our history (or junk whitespace text)
        }
      }
      // same as require(input.label == "HoMPack"); <HoMPack>{ updateLevels(input.child) }</HoMPack>
      input match {
        case <HoMPack>{ ch @ _* }</HoMPack> => <HoMPack>{ updateLevels(ch) }</HoMPack>
        //case x => throw new RuntimeException()
      }
    }
  }
}

trait ModelComponents {
  case class GameModel(pack: GamePack, level: GameLevel, history: GameHistory) {
    def debug() {
      if (level == null) println("No level")
      else println("Level "+ level.level)
    }
  }
  var model: GameModel
}

sealed trait HouseMessage

case object Start extends HouseMessage
case object Stop extends HouseMessage
case object Help extends HouseMessage
case class ErrorMessage(message: String) extends HouseMessage
case class LoadGamePackResource(name: String, level: Option[Int])
case class LoadGameLevelResource(resource: Resource, level: Int)
case class OpenFile(file: java.io.File)
case class SaveFile(file: java.io.File)
case class SaveGame(file: java.io.File)
case class PackLoaded(pack: GamePack) extends HouseMessage
case class LevelLoaded(level: GameLevel) extends HouseMessage
case class TraceResult(state: GameState) extends HouseMessage
case object PreviousLevel extends HouseMessage
case object NextLevel extends HouseMessage
case object ResetLevel extends HouseMessage
case object LevelUpdate extends HouseMessage

/**
 * A map of game levels externalized as XML.
 */
class GameHistory {
  import scala.xml.Elem
  var _history: Map[Int, Elem] = Map.empty
  def history: Map[Int, Elem] = _history
  def putGameLevel(g: GameLevel) { _history += (g.level -> g.toXML); }
  def hasGameLevel(g: GameLevel) = history.contains(g.level)
  def hasGameLevel(i: Int) = history.contains(i)
  def clear() { _history = Map.empty }
}

trait GameIO {
  this: ModelComponents with ControllerComponents =>
  import java.io.File
  def openFile() {
    val fc = new MyFileChooser(workingDir)
    if (fc.showOpenDialog(null) == FileChooser.Result.Approve) {
      loader ! OpenFile(fc.selectedFile)
    }
  }
  def saveFile() {
    val fc = new MyFileChooser(workingDir)
    if (fc.showSaveDialog(null) == FileChooser.Result.Approve) {
      loader ! SaveFile(fc.selectedFile)
    }
  }
  def saveGame() {
    val fc = new MyFileChooser(homDir)
    if (fc.showSaveDialog(null) == FileChooser.Result.Approve) {
      loader ! SaveGame(fc.selectedFile)
    }
  }
  private def workingDir: File = model.pack.packDir.getOrElse(new File(scala.util.Properties.userDir))
  private def homDir: File = new File(new File(scala.util.Properties.userHome), ".hom")

  import javax.swing.filechooser.FileNameExtensionFilter
  class MyFileChooser(dir: File) extends FileChooser(dir) {
    this.peer.addChoosableFileFilter(new FileNameExtensionFilter("House of Mirrors game levels (.hom)", "hom"))
    this.fileFilter = new FileNameExtensionFilter("House of Mirrors game packs (.homp)", "homp")
  }
}

