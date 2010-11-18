package org.puremvc.scala

import scala.swing.Swing

/**
 * Overrides PureApplication.main to run preinitialize
 * on the main thread and startup on the EDT.
 * 
 * @author A. P. Marki
 */
trait SwingStartup {
	this: PureApplication =>
	
	override def main(args: Array[String]) {
		preinitialize()
		Swing.onEDT {
			startup(args)
		}
	}

}