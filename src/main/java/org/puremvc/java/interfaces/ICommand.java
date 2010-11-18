/*
   PureMVC Java Port by Donald Stinchfield <donald.stinchfield@puremvc.org>, et al.
   PureMVC - Copyright(c) 2006-08 Futurescale, Inc., Some rights reserved.
   Your reuse is governed by the Creative Commons Attribution 3.0 License
 */

package org.puremvc.java.interfaces;

/**
 * The interface definition for a PureMVC Command.
 * 
 * @see org.puremvc.java.interfaces INotification
 */
public interface ICommand {

	/**
	 * Fulfill the use-case initiated by the given <code>INotification</code>.
	 * 
	 * <P>
	 * In the Command Pattern, an application use-case typically begins with
	 * some user action, which results in an <code>INotification</code> being
	 * broadcast, which is handled by business logic in the <code>execute</code>
	 * method of an <code>ICommand</code>.
	 * </P>
	 * 
	 * @param notification
	 *            the <code>INotification</code> to handle.
	 */
	void execute(INotification notification);
}
