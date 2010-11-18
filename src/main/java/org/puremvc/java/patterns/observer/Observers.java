/*
   PureMVC Java Port by Donald Stinchfield <donald.stinchfield@puremvc.org>, et al.
   PureMVC - Copyright(c) 2006-08 Futurescale, Inc., Some rights reserved.
   Your reuse is governed by the Creative Commons Attribution 3.0 License
 */

package org.puremvc.java.patterns.observer;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.puremvc.java.interfaces.INotification;
import org.puremvc.java.interfaces.IObserver;

/**
 * Helper class that contains all observers for a notification
 * 
 * @see org.puremvc.java.interfaces IObservers
 * @see org.puremvc.java.interfaces INotification
 */
public class Observers implements Iterable<IObserver> {
	private final List<IObserver> observers;

	private final String notificationName;

	/**
	 * Constructor
	 * 
	 * @param note
	 * @param observer
	 */
	public Observers(String note, IObserver observer) {
		if (note == null || observer == null) {
			throw new NullPointerException();
		}
        this.observers = new ArrayList<IObserver>();
        this.observers.add(observer);
        this.notificationName = note;
	}

	/**
	 * Add new observer
	 * 
	 * @param observer
	 */
	public void addObserver(IObserver observer) {
		if (observer == null) {
			throw new NullPointerException();
		}
        this.observers.add(observer);
	}

	/**
	 * Delete an observer
	 * 
	 * 
	 * @param observer
	 */
	public void deleteObserver(IObserver observer) {
		if (observer == null) {
			throw new NullPointerException();
		}
        this.observers.remove(observer);
	}
	
	public Iterator<IObserver> iterator() {
		return this.observers.iterator();
	}

	/**
	 * Notify all observers of the notification
	 * 
	 * @param note
	 */
	public void notifyObservers(INotification note) {
		if (note == null) {
			throw new NullPointerException();
		}
        for (IObserver o : this.observers) {
            o.notifyObserver(note);
        }
	}

	/**
	 * Return the notification
	 * 
	 * @return String
	 */
	public String getNotification() {
		return this.notificationName;
	}
}
