/*
   PureMVC Java Port by Donald Stinchfield <donald.stinchfield@puremvc.org>, et al.
   PureMVC - Copyright(c) 2006-08 Futurescale, Inc., Some rights reserved.
   Your reuse is governed by the Creative Commons Attribution 3.0 License
*/

package org.puremvc.java.interfaces;

/**
 * The publisher for a PureMVC application.
 */
public interface IObserverRegistry {

	/**
	 * Register an <code>IObserver</code> to be notified of
	 * <code>INotifications</code> with a given name.
	 * 
	 * @param noteName
	 *            the name of the <code>INotifications</code> to notify this
	 *            <code>IObserver</code> of
	 * @param observer
	 *            the <code>IObserver</code> to register
	 */
	void registerObserver(String noteName, IObserver observer);

	/**
	 * Notify the <code>IObservers</code> for a particular
	 * <code>INotification</code>.
	 * 
	 * <P>
	 * All previously attached <code>IObservers</code> for this
	 * <code>INotification</code>'s list are notified and are passed a
	 * reference to the <code>INotification</code> in the order in which they
	 * were registered.
	 * </P>
	 * 
	 * @param note
	 *            the <code>INotification</code> to notify
	 *            <code>IObservers</code> of.
	 */
	void notifyObservers(INotification note);
}
