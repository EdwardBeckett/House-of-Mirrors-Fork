package org.puremvc.scala

import org.puremvc.java.interfaces._

/**
 * A PureMVC application in Scala.
 * This type should be extensible for frameworks like
 * Guice and Spring.
 * 
 * @author apm
 *
 */
abstract class PureApplication {
	
	/** The canonical startup notification name. */
	val Startup = "Startup"
	
	/**
	 * The default entry point calls preinitialize and startup.
	 * @param args
	 */
	def main(args: Array[String]) {
		preinitialize()
		startup(args)
	}
	
	/**
	 * The default implementation does nothing.
	 */
	def preinitialize() {
		//empty
	}
	
	/**
	 * Subclasses must provide a Facade with
	 * this factory method.
	 * @return the facade singleton
	 */
	def facade: IFacade
	
	/**
	 * Subclasses may override this startup method.
	 * The default implementation invokes processArguments
	 * and emits the canonical "startup"
	 * notification using the facade.
	 * @param args
	 */
	def startup(args: Array[String]) {
		processArguments(args)
		facade.sendNotification(Startup, this, null)
	}
	
	def processArguments(args: Array[String]) {
		//empty
	}
	
	def shutdown() {
		exit()
	}
}