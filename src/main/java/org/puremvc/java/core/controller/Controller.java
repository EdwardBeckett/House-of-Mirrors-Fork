/*
   PureMVC Java Port by Donald Stinchfield <donald.stinchfield@puremvc.org>, et al.
   PureMVC - Copyright(c) 2006-08 Futurescale, Inc., Some rights reserved.
   Your reuse is governed by the Creative Commons Attribution 3.0 License
 */

package org.puremvc.java.core.controller;

import java.util.HashMap;
import java.util.Map;

import org.puremvc.java.interfaces.ICommand;
import org.puremvc.java.interfaces.IController;
import org.puremvc.java.interfaces.IFacade;
import org.puremvc.java.interfaces.IFunction;
import org.puremvc.java.interfaces.INotification;
import org.puremvc.java.interfaces.IObserverRegistry;
import org.puremvc.java.patterns.observer.Notifier;
import org.puremvc.java.patterns.observer.Observer;

/**
 * A Singleton <code>IController</code> implementation.
 * 
 * <P>
 * In PureMVC, the <code>Controller</code> class follows the 'Command and
 * Controller' strategy, and assumes these responsibilities:
 * <UL>
 * <LI>Remembering which <code>ICommand</code>s are intended to handle which
 * <code>INotifications</code>.</LI>
 * <LI>Registering itself as an <code>IObserver</code> with the
 * <code>View</code> for each <code>INotification</code> that it has an
 * <code>ICommand</code> mapping for.</LI>
 * <LI>Creating a new instance of the proper <code>ICommand</code> to handle a
 * given <code>INotification</code> when notified by the <code>View</code>.</LI>
 * <LI>Calling the <code>ICommand</code>'s <code>execute</code> method, passing
 * in the <code>INotification</code>.</LI>
 * </UL>
 * 
 * <P>
 * Your application must register <code>ICommands</code> with the Controller.
 * <P>
 * The simplest way is to subclass </code>Facade</code>, and use its
 * <code>initializeController</code> method to add your registrations.
 * 
 * @see org.puremvc.java.core.view.View View
 * @see org.puremvc.java.patterns.observer.Observer Observer
 * @see org.puremvc.java.patterns.observer.Notification Notification
 * @see org.puremvc.java.patterns.command.SimpleCommand SimpleCommand
 * @see org.puremvc.java.patterns.command.MacroCommand MacroCommand
 */
public class Controller implements IController {

    /**
     * Mapping of Notification names to Command Class references
     */
    private final Map<String, Class<? extends ICommand>> commandMap;

    /**
     * Local reference to View
     */
    protected IObserverRegistry observerRegistry;
    
    private IFacade facade;

    private final CommandFactory commandFactory;

    public Controller() {
        this.commandMap = new HashMap<String, Class<? extends ICommand>>();
        this.commandFactory = new DefaultCommandFactory();
        initializeController();
    }

    public Controller(CommandFactory commandFactory) {
        this.commandMap = new HashMap<String, Class<? extends ICommand>>();
        this.commandFactory = commandFactory;
        initializeController();
    }

    public void setObserverRegistry(IObserverRegistry observerRegistry) {
        this.observerRegistry = observerRegistry;
    }

    /**
     * Initialize the Singleton <code>Controller</code> instance.
     * 
     * <P>
     * Called automatically by the constructor.
     * </P>
     * 
     * <P>
     * Note that if you are using a subclass of <code>View</code> in your
     * application, you should <i>also</i> subclass <code>Controller</code> and
     * override the <code>initializeController</code> method in the following
     * way:
     * </P>
     * 
     * <listing> // ensure that the Controller is talking to my IView
     * implementation override public function initializeController( ) : void {
     * view = MyView.getInstance(); } </listing>
     * 
     */
    protected void initializeController() {
        //empty
    }

    /**
     * If an <code>ICommand</code> has previously been registered to handle a
     * the given <code>INotification</code>, then it is executed.
     * 
     * @param note
     *            an <code>INotification</code>
     */
    public void executeCommand(INotification note) {
        Class<? extends ICommand> klass = this.commandMap.get(note.getName());
        if (klass != null) {
            this.commandFactory.create(klass).execute(note);
        }
    }

    /**
     * Register a particular <code>ICommand</code> class as the handler for a
     * particular <code>INotification</code>.
     * 
     * <P>
     * If an <code>ICommand</code> has already been registered to handle
     * <code>INotification</code>s with this name, it is no longer used, the new
     * <code>ICommand</code> is used instead.
     * </P>
     * 
     * The Observer for the new ICommand is only created if this the first time
     * an ICommand has been regisered for this Notification name.
     * 
     * @param notificationName
     *            the name of the <code>INotification</code>
     * @param commandClassRef
     *            the <code>Class</code> of the <code>ICommand</code>
     */
    public Class<? extends ICommand> registerCommand(String notificationName, Class<? extends ICommand> commandClassRef) {
        Class<? extends ICommand> prev = this.commandMap.put(notificationName, commandClassRef);
        if (prev == null) {
            this.observerRegistry.registerObserver(notificationName, new Observer(
                new IFunction() {
                    public void onNotification(INotification notification) {
                        executeCommand(notification);
                    }
                }, this));
        }
        return prev;
    }

    /**
     * Remove a previously registered <code>ICommand</code> to
     * <code>INotification</code> mapping.
     * 
     * @param notificationName
     *            the name of the <code>INotification</code> to remove the
     *            <code>ICommand</code> mapping for
     */
    public Class<? extends ICommand> removeCommand(String notificationName) {
        return this.commandMap.remove(notificationName);
    }

    public IFacade getFacade() {
        return facade;
    }

    public void setFacade(IFacade facade) {
        this.facade = facade;
    }

    private class DefaultCommandFactory implements CommandFactory {
        public ICommand create(Class<? extends ICommand> klass) {
            Exception problem;
            try {
                ICommand command = klass.newInstance();
                if (command instanceof Notifier) {
                    ((Notifier)command).setFacade(getFacade());
                }
                return command;
            } catch (IllegalAccessException iae) {
                problem = iae;
            } catch (InstantiationException ie) {
                problem = ie;
            }
            throw new RuntimeException(problem);
        }
    }
}
