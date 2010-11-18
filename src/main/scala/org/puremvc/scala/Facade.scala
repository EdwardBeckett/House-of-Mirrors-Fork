package org.puremvc.scala

import org.puremvc.java.interfaces.IController
import org.puremvc.java.interfaces.IModel
import org.puremvc.java.interfaces.IView

/**
 *
 */
class Facade(m: IModel, v: IView, c: IController) extends org.puremvc.java.patterns.facade.Facade(m, v, c)

