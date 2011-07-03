
package hom

import org.puremvc.scala.Facade
import org.puremvc.java.interfaces._
import org.puremvc.java.patterns.command._
import org.puremvc.java.patterns.mediator._
import org.puremvc.java.patterns.observer._
import org.puremvc.java.core.controller._
import org.puremvc.java.core.model._
import org.puremvc.java.core.view._

import com.google.inject._

/**
 * PureMVC Facade for the HoM application.
 */
class HouseFacade @Inject()(val model: IModel, val view: IView, val controller: IController) extends Facade(model, view, controller) {

  import notes.HouseNotifications._

  override def initializeController() {
    super.initializeController()
    this.controller.asInstanceOf[Controller].setFacade(this)
    this.controller.asInstanceOf[Controller].setObserverRegistry(this.view)
    registerCommand(Startup.toString, classOf[StartupCommand])
  }
  
  override def registerProxy(p: IProxy) {
    if (p.isInstanceOf[Notifier]) {
      p.asInstanceOf[Notifier].setFacade(this);
    }
    super.registerProxy(p);
  }

  override def registerMediator(m: IMediator) {
    if (m.isInstanceOf[Notifier]) {
      m.asInstanceOf[Notifier].setFacade(this);
    }
    super.registerMediator(m);
  }

  def sendNotification(n: HouseNotificationType) { sendNotification(n.toString, null, null) }

  def sendNotification(n: HouseNotificationType, body: AnyRef) { sendNotification(n.toString, body, null) }

  def sendNotification[T <: AnyRef](n: notes.HouseNotification[T]) { notifyObservers(n) }
}

import org.puremvc.java.patterns.observer.Notifier
trait RichNotifier {
  this: Notifier =>
  import notes.HouseNotification
  import notes.HouseNotifications.{HouseNotificationType, BrokenMirror}
  import notes.MessageTypes.Error
  def houseFacade: HouseFacade = getFacade.asInstanceOf[HouseFacade]
  def sendNotification[T <: AnyRef](n: HouseNotification[T]) { houseFacade.notifyObservers(n) }
  def sendNotification(n: HouseNotificationType, body: AnyRef = null, subtype: String = null) { sendNotification(n.toString, body, subtype) }
  def sendError(m: String) = sendNotification(BrokenMirror, m, Error.toString)
  implicit def toConvertibleNotification(n: INotification): ConvertibleNotification = {
    n match {
      case c: ConvertibleNotification => c
      case _ => new ConvertibleNotification(n)
    }
  }
  /**
   * Convenience wrapper.
   */
  class ConvertibleNotification(not: INotification) extends INotification {
    import notes.HouseNotifications._
    def asType: HouseNotificationType = {
      notes.HouseNotifications.forNotification(this)
    }
    def getName: String = not.getName
    def getBody: AnyRef = not.getBody
    def setBody(b: AnyRef) { not.setBody(b) }
    def getType: String = not.getType
    def setType(t: String) { not.setType(t) }
    override def toString = "Convertible " + not.toString
  }
}

/**
 * A conventional handler that delegates the assembly
 * of the rest of the PureMVC infrastructure.
 */
class StartupCommand @Inject()(cf: CommandFactory) extends MacroCommand(cf) {
  override def initializeMacroCommand() {
    addSubCommand(classOf[ControllerPrepCommand]);
    addSubCommand(classOf[ModelPrepCommand]);
    addSubCommand(classOf[ViewPrepCommand]);
  }
}

/**
 * Register command handlers on startup.
 */
class ControllerPrepCommand extends SimpleCommand {
  import notes.HouseNotifications._
  override def execute(n: INotification) {
    getFacade.registerCommand(LoadGamePack.toString, classOf[LoadGamePackCommand]);
    getFacade.registerCommand(PackLoaded.toString, classOf[LoadGamePackCommand]);
    getFacade.registerCommand(LoadLevel.toString, classOf[LoadLevelCommand]);
    getFacade.registerCommand(LevelLoaded.toString, classOf[TraceCommand]);
    getFacade.registerCommand(LevelUpdate.toString, classOf[TraceCommand]);
    getFacade.registerCommand(OpenFile.toString, classOf[OpenFileCommand]);
    getFacade.registerCommand(SaveFile.toString, classOf[SaveFileCommand]);
    getFacade.registerCommand(SaveGame.toString, classOf[SaveGameCommand]);
    getFacade.registerCommand(PackLoaded.toString, classOf[HistoryCommand]);
    getFacade.registerCommand(Trace.toString, classOf[HistoryCommand]);
  }
}

import java.io.{File, FileWriter}

/**
 * Initiate loading a game pack (of levels).
 * Level Zero is loaded implicitly, and a LevelLoaded
 * notification is emitted.
 */
class LoadGamePackCommand @Inject()(private val packProxy: GamePackProxy) extends SimpleCommand {
  //private lazy val packProxy = this.facade.retrieveProxy("GamePack").asInstanceOf[GamePackProxy]
  override def execute(n: INotification) {
    n match {
      case load: notes.LoadGamePackNotification =>
        load.body match {
          case (what: String, level: Int) => this.packProxy.loadPack(what); emitLoadLevel(level)
          case (what: File, level: Int) => this.packProxy.loadPack(what); emitLoadLevel(level)
          case _ => println("Bad load game")
        }
    }
  }
  // if level < 0, find the min unlocked level
  private def emitLoadLevel(level: Int) {
    val i = if (level < 0) {
        this.packProxy.gamePack.unlockedLevels.foldLeft(Integer.MAX_VALUE) { (x,y) => x.min(y) }
      } else level
    import notes.HouseNotifications._
    sendNotification(LoadLevel.toString, i, null)
  }
}

/**
 * Load a game level.
 * The requested level must be unlocked.
 * The current level need not be in a completed state.
 */
class LoadLevelCommand @Inject()(private val pack: GamePackProxy,
                                 private val level: GameLevelProxy,
                                 private val history: HistoryProxy) extends SimpleCommand {
  import notes.HouseNotifications._
  import notes.LevelCommands._
  override def execute(n: INotification) {
    require(n.getName == LoadLevel.toString)
    //require(n.getType != null)
    val subtype = if (n.getType != null) notes.LevelCommands.withName(n.getType) else Load
    subtype match {
      case Load =>
        val request = n.getBody.asInstanceOf[Int]
        loadLevel(request, true)
      case Next =>
        if (!this.level.isLoaded) HouseApplication.apperror("No level")
        loadLevel(this.level.gameLevel.level + 1, true)
      case Reset =>
        if (!this.level.isLoaded) HouseApplication.apperror("No level")
        loadLevel(this.level.gameLevel.level, false)
    }
  }
  private def loadLevel(request: Int, withHistory: Boolean) {
    val which: Int = if (request >= 0) request else this.level.gameLevel.level + 1
    if (this.pack.gamePack.isUnlocked(which)) {
      if (withHistory && this.history.hasGameLevel(which)) {
        this.level.loadLevel(this.history.history.get(which).get, which)
      } else {
        this.level.loadLevel(this.pack.gamePack.levelResource(which), which)
      }
    } else {
      sendNotification(BrokenMirror.toString, "Your way to level " + which + " is blocked by a locked door!", null)
    }
  }
}

/**
 * Invoked when a level is loaded or updated, to generate the "trace" of the current level.
 * Emits Trace and Status notifications for the UI, and handles unlocking levels when the current
 * level is completed.
 */
class TraceCommand @Inject()(pack: GamePackProxy, level: GameLevelProxy) extends SimpleCommand with RichNotifier {
  override def execute(n: INotification) {
    import notes.HouseNotifications._
    val state: GameState = this.level.trace
    sendNotification(Trace, state)
    sendNotification(Status, state.status)
    if (state.status.isComplete) {
      if (this.pack.gamePack.unlockAll(this.level.gameLevel.level)) { /*println("New levels unlocked.")*/ }
    }
  }
}

/**
 * When a Trace event shows that a level is complete, save the game history.
 * When a new game pack is loaded, game history is cleared.
 * Retains only the first completion of a level; there may be multiple solutions to a level.
 */
class HistoryCommand @Inject()(private val history: HistoryProxy, private val level: GameLevelProxy) extends SimpleCommand with RichNotifier {
  import notes.HouseNotifications._
  override def execute(n: INotification) = n.asType match {
    case PackLoaded => history.clear
    case Trace => onTrace(n.getBody.asInstanceOf[GameState])
  }
  private def onTrace(state: GameState) {
    if (state.status.isComplete && !history.hasGameLevel(level.gameLevel)) {
      history.putGameLevel(level.gameLevel)
    }
  }
}

/**
 * Base class of commands that read/write *.hom[p] files.
 */
abstract class FileCommand extends SimpleCommand with RichNotifier with WithFileWriter {
  import java.io._
  import java.nio.charset.Charset
  import scala.xml._
  private val printWidth = 120
  private val printTab = 2
  override final def execute(n: INotification) {
    val file = n.getBody.asInstanceOf[File]
    executeBody(file)
  }
  protected def executeBody(f: File)

  protected final def prettyPrinter = new PrettierPrinter(printWidth, printTab)
}

class OpenFileCommand extends FileCommand {
  import scala.xml._
  private val OptFileExt = """.*?(\..*)?""".r
  override def executeBody(f: File) {
    val OptFileExt(x) = f.getName
    x match {
      case ".hom" => // loading level file not supported yet
      case ".homp" => sendNotification(notes.LoadGamePackNotification(f, -1)) // load pack and start at lowest level
      case _ => // don't know what that is
    }
  }
}

/** Save the current game to a file. */
class SaveFileCommand @Inject()(level: GameLevelProxy) extends FileCommand {
  override def executeBody(f: File) {
    if (f.exists) {
      sendNotification(notes.HouseNotifications.BrokenMirror.toString, "File exists: " + f.getAbsolutePath, notes.MessageTypes.Error.toString)
    } else {
      f.createNewFile
      val xml = level.toXML
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
class SaveGameCommand @Inject()(private val pack: GamePackProxy, private val history: HistoryProxy) extends FileCommand {
  import scala.xml._
  import java.io._
  override def executeBody(f: File) {
    def noext(s: String): String = if (s.lastIndexOf('.') > 0) s.substring(0, s.lastIndexOf('.')) else s
    def solutionFileName(simple: String, id: String): String = simple + "_" + id + ".hom"
    //def solutionFileName(id: String): String = noext(f.getName) + "_" + id + ".hom"
    if (f.exists) {
      sendError("File exists: " + f.getAbsolutePath)
      return;
    }
    val base = Resource(f)
    val namer = solutionFileName(noext(f.getName), _: String)
    writeSolutionFiles(base, namer)
    // update the game xml with our solution file names
    val packElem: Elem = updatePack(pack.toXML, namer)
    withWriter(f) {
      w => w.write(prettyPrinter.format(packElem))
    }
  }

  private def writeSolutionFiles(base: Resource, namer: (String) => String) {
    for ((level, xml) <- this.history.history) {
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
      def hasHistoryFor(i: Int) = history.hasGameLevel(i)
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

/**
 * Register model components on startup.
 */
class ModelPrepCommand extends SimpleCommand {
  @Inject private var injector: Injector = _
  override def execute(n: INotification) {

    /*
    this.facade.registerProxy(new GameLevelProxy)
    this.facade.registerProxy(new HistoryProxy)
    this.facade.registerProxy(new GamePackProxy)
    */

    getFacade.registerProxy(injector.getInstance(classOf[GameLevelProxy]))
    getFacade.registerProxy(injector.getInstance(classOf[GamePackProxy]))
    getFacade.registerProxy(injector.getInstance(classOf[HistoryProxy]))
  }
}

import org.puremvc.java.patterns.proxy.Proxy

@Singleton
class GamePackProxy extends Proxy("GamePack", null) {
  import notes.HouseNotifications._
  setData(new GamePack(Set.empty, Nil))
  def gamePack = getData.asInstanceOf[GamePack]
  def loadPack(resource: String) {
    setData(GamePack(resource))
    sendNotification(PackLoaded.toString, resource, null)
  }
  def loadPack(file: File) {
    setData(GamePack(file))
    sendNotification(PackLoaded.toString, file, null)
  }
  import scala.xml.Elem
  def toXML: Elem = gamePack.toXML
}

@Singleton
class GameLevelProxy extends Proxy("GameLevel", null) {
  import notes.HouseNotifications._
  import scala.xml.Elem
  def gameLevel: GameLevel = getData.asInstanceOf[GameLevel]
  def isLoaded = getData != null
  def loadLevel(resource: Resource, which: Int) {
    if (gameLevel != null) {
      gameLevel.dispose()
    }
    //println("Loading level " + which + " from " + resource)
    setData(GameLevel(resource, which))
    sendNotification(LevelLoaded.toString, gameLevel, null)
  }
  def loadLevel(xml: Elem, which: Int) {
    if (gameLevel != null) {
      gameLevel.dispose()
    }
    //println("Loading history " + which )
    setData(GameLevel.fromXML(xml, which))
    sendNotification(LevelLoaded.toString, gameLevel, null)
  }
  def trace: GameState = this.gameLevel.trace
  def toXML: Elem = GameLevel.toXML(gameLevel)
}

/**
 * A map of game levels externalized as XML.
 */
@Singleton
class HistoryProxy extends Proxy("History", Map.empty) {
  import scala.xml.Elem
  def history: Map[Int, Elem] = getData.asInstanceOf[Map[Int, Elem]]
  def putGameLevel(g: GameLevel) { setData(history + (g.level -> g.toXML)); /*println(new PrettierPrinter().format(g.toXML))*/ }
  def hasGameLevel(g: GameLevel) = history.contains(g.level)
  def hasGameLevel(i: Int) = history.contains(i)
  def clear() { setData(Map.empty) }
}

import scala.swing.{Dimension, FileChooser, Frame, MainFrame, MenuBar, Menu, MenuItem, Action, Dialog, BoxPanel, Orientation, Swing}

trait GameIO {
  this: Notifier with RichNotifier =>
  import notes.HouseNotifications._
  val pack: GamePackProxy
  def openFile() {
    val fc = new MyFileChooser(workingDir)
    if (fc.showOpenDialog(null) == FileChooser.Result.Approve) {
      sendNotification(OpenFile.toString, fc.selectedFile, null)
    }
  }
  def saveFile() {
    val fc = new MyFileChooser(workingDir)
    if (fc.showSaveDialog(null) == FileChooser.Result.Approve) {
      sendNotification(SaveFile.toString, fc.selectedFile, null)
    }
  }
  def saveGame() {
    val fc = new MyFileChooser(homDir)
    if (fc.showSaveDialog(null) == FileChooser.Result.Approve) {
      sendNotification(SaveGame, fc.selectedFile)
    }
  }
  private def workingDir: File = {
    if (this.pack.gamePack.packDir.isDefined) this.pack.gamePack.packDir.get
    else new File(scala.util.Properties.userDir)
  }
  private def homDir: File = new File(new File(scala.util.Properties.userHome), ".hom")

  import javax.swing.filechooser.FileNameExtensionFilter
  class MyFileChooser(dir: File) extends FileChooser(dir) {
    this.peer.addChoosableFileFilter(new FileNameExtensionFilter("House of Mirrors game levels (.hom)", "hom"))
    this.fileFilter = new FileNameExtensionFilter("House of Mirrors game packs (.homp)", "homp")
  }
}

trait LightBoxComponent {
  lazy val lightBox = new LightBox
}

trait StatusBarComponent {
  import scala.swing.Label
  val statusBar = new Label("HoM") {preferredSize = new java.awt.Dimension(250, 100)}
}

trait StatusIconComponent {
  import javax.swing.ImageIcon
  import scala.swing.Label
  import javax.swing.border.EmptyBorder
  lazy val statusIcon = new Label() {icon = new ImageIcon(Resource("images/thumbsUp.png").uri.toURL); border = new EmptyBorder(20,10,10,10)}
}

trait MainFrameComponent {
  this: LightBoxComponent with StatusBarComponent with StatusIconComponent with GameIO with Notifier =>
  import notes.HouseNotifications._
  lazy val frame = new MainFrame {
    title = "House of Mirrors"
    menuBar = new MenuBar {
      contents += (new Menu("File") {
        contents += (new MenuItem(Action("Open...") { openFile() }))
        contents += (new MenuItem(Action("Save...") { saveFile() }))
        contents += (new MenuItem(Action("Save Game") { saveGame() }))
        contents += (new MenuItem(Action("Quit") { sys.exit() }))
      })
      contents += (new Menu("Game") {
        contents += (new MenuItem(Action("Reset") { resetLevel() }))
      })
      contents += (new MenuItem(Action("Help") { sendNotification(Help.toString, null, null) }))
    }
    contents = new BoxPanel (Orientation.Horizontal) {
      contents += lightBox
      contents += new BoxPanel(Orientation.Vertical) {
        contents += statusBar
        contents += statusIcon
      }
    }
  }
  def resetLevel() {
    sendNotification(LoadLevel.toString, null, notes.LevelCommands.Reset.toString)
  }
}

class LightBoxMediatorComponent(c: Object) extends LightBoxMediator(c) {
  lazy val levelProxy = getFacade.retrieveProxy("GameLevel").asInstanceOf[GameLevelProxy]
  lazy val packProxy = getFacade.retrieveProxy("GamePack").asInstanceOf[GamePackProxy]
}

/**
 * Register mediators for view components.
 */
class ViewPrepCommand @Inject()(val pack: GamePackProxy) extends SimpleCommand
  with RichNotifier with MainFrameComponent with LightBoxComponent with StatusBarComponent with StatusIconComponent with GameIO {

  override def execute(n: INotification) {
    //Swing.onEDTWait(construct)
    // should already execute on EDT, by SwingStartup
    construct()
  }

  /** Construct the UI and register mediator components. */
  private def construct(): Unit = {
    if (frame.size == new Dimension(0,0)) frame.pack()
    center(frame)
    frame.visible = true
    lightBox.requestFocus

    getFacade.registerMediator(new ApplicationMediator(frame))
    getFacade.registerMediator(new LightBoxMediatorComponent(lightBox))
    getFacade.registerMediator(new StatusMediator(statusBar))
    getFacade.registerMediator(new StatusIconMediator(statusIcon))
  }

  private def center(f: Frame) {
    import java.awt.{Dimension,Toolkit}
    val d: Dimension = Toolkit.getDefaultToolkit.getScreenSize
    val xMargin = (d.width - f.size.width)/2
    val yMargin = (d.height - f.size.height)/2
    f.peer.setLocation(xMargin, yMargin)
  }
}

class ApplicationMediator (c: Object) extends Mediator("application-mediator", c) {
  import scala.swing.MainFrame
  require(c.isInstanceOf[MainFrame])

  private def app: MainFrame = getViewComponent.asInstanceOf[MainFrame]
}

class StatusMediator (c: Object) extends Mediator("status-mediator", c) {
  import notes.HouseNotifications._
  import scala.swing.Label
  require(c.isInstanceOf[Label])

  override val listNotificationInterests = asNames(List(Status, BrokenMirror))

  override def handleNotification(n: INotification) {
    notes.HouseNotifications findName n.getName match {
      case Some(Status) => onStatusUpdate(n.getBody.asInstanceOf[StatusUpdate])
      case Some(BrokenMirror) => message(null, "Yikes!", n.getBody.asInstanceOf[String])
      case None => // it's not one of our notifications
      case _ => // didn't ask for anything else
    }
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

  private def statusBar: Label = getViewComponent.asInstanceOf[Label]
}

class StatusIconMediator(c: Object) extends Mediator("status-icon-mediator", c) {
  import notes.HouseNotifications._
  import scala.swing.Label
  require(c.isInstanceOf[Label])

  override val listNotificationInterests = asNames(List(Status))

  override def handleNotification(n: INotification) {
    notes.HouseNotifications findName n.getName match {
      case Some(Status) => onStatusUpdate(n.getBody.asInstanceOf[StatusUpdate])
      case None => // it's not one of our notifications
      case _ => // didn't ask for anything else
    }
  }

  private def onStatusUpdate(s: StatusUpdate) {
    statusIcon.enabled = s.isComplete
  }

  private def statusIcon: Label = getViewComponent.asInstanceOf[Label]
}

class StatusUpdate(val description: String, val score: Int, val total: Int) {
  def isComplete: Boolean = score == total
}

