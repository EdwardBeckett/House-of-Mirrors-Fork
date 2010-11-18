/*
   PureMVC Java Port by Donald Stinchfield <donald.stinchfield@puremvc.org>, et al.
   PureMVC - Copyright(c) 2006-08 Futurescale, Inc., Some rights reserved.
   Your reuse is governed by the Creative Commons Attribution 3.0 License
 */

package org.puremvc.java.patterns.command;

import org.puremvc.java.interfaces.ICommand;
import org.puremvc.java.patterns.observer.Notifier;

/**
 * A base <code>ICommand</code> implementation.
 * 
 * <P>
 * Your subclass should override the <code>execute</code> method where your
 * business logic will handle the <code>INotification</code>.
 * </P>
 * 
 * @see org.puremvc.java.core.controller.Controller Controller
 * @see org.puremvc.java.patterns.observer.Notification Notification
 * @see org.puremvc.java.patterns.command.MacroCommand MacroCommand
 */
public abstract class SimpleCommand extends Notifier implements ICommand {
	//empty
}
