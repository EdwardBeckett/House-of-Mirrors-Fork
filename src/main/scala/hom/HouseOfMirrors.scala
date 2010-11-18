package hom

/** Application-wide configuration and utility methods. */
object HouseApplication {

  def apperror(message: String): Nothing = throw new ApplicationException(message)

}

class ApplicationException(m: String) extends RuntimeException(m)

import scala.swing.Swing
import org.puremvc.scala.{PureApplication,SwingStartup}

import org.puremvc.java.interfaces._
import org.puremvc.java.patterns.command._
import org.puremvc.java.patterns.facade._
import org.puremvc.java.patterns.mediator._
import org.puremvc.java.patterns.observer._
import org.puremvc.java.core.controller._
import org.puremvc.java.core.model._
import org.puremvc.java.core.view._

import com.google.inject._

/** Application entry point initializes the application and loads the default game. */
object HouseOfMirrors extends PureApplication with SwingStartup {

  class HouseController @Inject()(f: CommandFactory) extends Controller(f)

  class GuicedCommandFactory extends CommandFactory {
    //@Inject var injector: Injector = _
    override def create(k: Class[_ <: ICommand]): ICommand = {
      val command: ICommand = injector.getInstance(k)
      if (command.isInstanceOf[Notifier]) {
        command.asInstanceOf[Notifier].setFacade(facade);
      }
      return command;
    }
  }
  
  val module = new AbstractModule {
    override protected def configure() = {
    	bind(classOf[CommandFactory]).to(classOf[GuicedCommandFactory])
        bind(classOf[IModel]).to(classOf[Model])
        bind(classOf[IView]).to(classOf[View])
        bind(classOf[IController]).to(classOf[HouseController])
        bind(classOf[IFacade]).to(classOf[HouseFacade])
    }
  }
  val injector = Guice.createInjector(module)
  val facade = injector.getInstance(classOf[IFacade]).asInstanceOf[HouseFacade] //new HouseFacade(new Model(), new View(), new HouseController(new GuicedCommandFactory))

  private val DefaultGamePack = "rowhouses/rowhouse.homp"

  override def startup(args: Array[String]) {
	// sends a default Notification
	super.startup(args)
    val firstLevel = 
      if (args.length == 1) {
        try {
          args(0).toInt;
        } catch {
          case e: NumberFormatException => 0
        }
      } else {
        0
      }
    facade.sendNotification(notes.LoadGamePackNotification(DefaultGamePack, firstLevel))
  }
}
