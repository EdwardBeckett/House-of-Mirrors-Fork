/*
   PureMVC Java Port by Donald Stinchfield <donald.stinchfield@puremvc.org>, et al.
   PureMVC - Copyright(c) 2006-08 Futurescale, Inc., Some rights reserved.
   Your reuse is governed by the Creative Commons Attribution 3.0 License
 */

package org.puremvc.java.patterns.facade;

import org.puremvc.java.interfaces.ICommand;
import org.puremvc.java.interfaces.IController;
import org.puremvc.java.interfaces.IFacade;
import org.puremvc.java.interfaces.IMediator;
import org.puremvc.java.interfaces.IModel;
import org.puremvc.java.interfaces.INotification;
import org.puremvc.java.interfaces.IProxy;
import org.puremvc.java.interfaces.IView;
import org.puremvc.java.patterns.observer.Notification;

import com.google.inject.*;

/**
 * A base Singleton <code>IFacade</code> implementation.
 * 
 * <P>
 * In PureMVC, the <code>Facade</code> class assumes these responsibilities:
 * <UL>
 * <LI>Initializing the <code>Model</code>, <code>View</code> and
 * <code>Controller</code> Singletons.</LI>
 * <LI>Providing all the methods defined by the <code>IModel, 
 * IView, & IController</code> interfaces.</LI>
 * <LI>Providing the ability to override the specific <code>Model</code>,
 * <code>View</code> and <code>Controller</code> Singletons created.</LI>
 * <LI>Providing a single point of contact to the application for registering
 * <code>Commands</code> and notifying <code>Observers</code></LI>
 * </UL>
 * 
 * @see org.puremvc.java.core.model.Model Model
 * @see org.puremvc.java.core.view.View View
 * @see org.puremvc.java.core.controller.Controller Controller
 * @see org.puremvc.java.patterns.observer.Notification Notification
 * @see org.puremvc.java.patterns.mediator.Mediator Mediator
 * @see org.puremvc.java.patterns.proxy.Proxy Proxy
 * @see org.puremvc.java.patterns.command.SimpleCommand SimpleCommand
 * @see org.puremvc.java.patterns.command.MacroCommand MacroCommand
 */
@Singleton
public class Facade implements IFacade {

    /**
     * Reference to the Controller
     */
    private final IController controller;

    /**
     * Reference to the Model
     */
    private final IModel model;

    /**
     * Reference to the View
     */
    private final IView view;

    /**
     */
    @Inject
    public Facade(IModel m, IView v, IController c) {
        this.model = m;
        this.view = v;
        this.controller = c;
        initializeFacade();
    }

    /**
     * Initialize the Singleton <code>Facade</code> instance.
     * 
     * <P>
     * Called automatically by the constructor. Override in your subclass to do
     * any subclass specific initializations. Be sure to call
     * <code>super.initializeFacade()</code>, though.
     * </P>
     */
    protected void initializeFacade() {
        initializeModel();
        initializeController();
        initializeView();
    }

    /**
     * Initialize the <code>Controller</code>.
     * 
     * <P>
     * Called by the <code>initializeFacade</code> method. Override this method
     * in your subclass of <code>Facade</code> if one or both of the following
     * are true:
     * <UL>
     * <LI>You wish to initialize a different <code>IController</code>.</LI>
     * <LI>You have <code>Commands</code> to register with the
     * <code>Controller</code> at startup.</code>.</LI>
     * </UL>
     * If you don't want to initialize a different <code>IController</code>,
     * call <code>super.initializeController()</code> at the beginning of your
     * method, then register <code>Command</code>s.
     * </P>
     */
    protected void initializeController() {
        //empty
    }

    /**
     * Initialize the <code>Model</code>.
     * 
     * <P>
     * Called by the <code>initializeFacade</code> method. Override this method
     * in your subclass of <code>Facade</code> if one or both of the following
     * are true:
     * <UL>
     * <LI>You wish to initialize a different <code>IModel</code>.</LI>
     * <LI>You have <code>Proxy</code>s to register with the Model that do not
     * retrieve a reference to the Facade at construction time.</code></LI>
     * </UL>
     * If you don't want to initialize a different <code>IModel</code>, call
     * <code>super.initializeModel()</code> at the beginning of your method,
     * then register <code>Proxy</code>s.
     * <P>
     * Note: This method is <i>rarely</i> overridden; in practice you are more
     * likely to use a <code>Command</code> to create and register
     * <code>Proxy</code>s with the <code>Model</code>, since <code>Proxy</code>
     * s with mutable data will likely need to send <code>INotification</code>s
     * and thus will likely want to fetch a reference to the <code>Facade</code>
     * during their construction.
     * </P>
     */
    protected void initializeModel() {
        //empty
    }

    /**
     * Initialize the <code>View</code>.
     * 
     * <P>
     * Called by the <code>initializeFacade</code> method. Override this method
     * in your subclass of <code>Facade</code> if one or both of the following
     * are true:
     * <UL>
     * <LI>You wish to initialize a different <code>IView</code>.</LI>
     * <LI>You have <code>Observers</code> to register with the
     * <code>View</code></LI>
     * </UL>
     * If you don't want to initialize a different <code>IView</code>, call
     * <code>super.initializeView()</code> at the beginning of your method, then
     * register <code>IMediator</code> instances.
     * <P>
     * Note: This method is <i>rarely</i> overridden; in practice you are more
     * likely to use a <code>Command</code> to create and register
     * <code>Mediator</code>s with the <code>View</code>, since
     * <code>IMediator</code> instances will need to send
     * <code>INotification</code>s and thus will likely want to fetch a
     * reference to the <code>Facade</code> during their construction.
     * </P>
     */
    protected void initializeView() {
        //empty
    }

    /**
     * Register an <code>ICommand</code> with the <code>Controller</code> by
     * Notification name.
     * 
     * @param noteName
     *            the name of the <code>INotification</code> to associate the
     *            <code>ICommand</code> with
     * @param commandClassRef
     *            a reference to the Class of the <code>ICommand</code>
     */
    public void registerCommand(String noteName, Class<? extends ICommand> commandClassRef) {
        this.controller.registerCommand(noteName, commandClassRef);
    }

    /**
     * Remove a previously registered <code>ICommand</code> to
     * <code>INotification</code> mapping from the Controller.
     * 
     * @param notificationName
     *            the name of the <code>INotification</code> to remove the
     *            <code>ICommand</code> mapping for
     */
    public Class<? extends ICommand> removeCommand(String notificationName) {
        return this.controller.removeCommand(notificationName);
    }

    /**
     * Register a <code>IMediator</code> with the <code>View</code>.
     * 
     * @param mediator
     *            the name to associate with this <code>IMediator</code>
     */
    public void registerMediator(IMediator mediator) {
        this.view.registerMediator(mediator);
    }

    /**
     * Register an <code>IProxy</code> with the <code>Model</code> by name.
     * 
     * @param proxy
     *            the name of the <code>IProxy</code> instance to be registered
     *            with the <code>Model</code>.
     */
    public void registerProxy(IProxy proxy) {
        this.model.registerProxy(proxy);
    }

    /**
     * Remove an <code>IMediator</code> from the <code>View</code>.
     * 
     * @param mediatorName
     *            name of the <code>IMediator</code> to be removed.
     * @return the <code>IMediator</code> that was removed from the
     *         <code>View</code>
     */
    public IMediator removeMediator(String mediatorName) {
        return this.view.removeMediator(mediatorName);
    }

    /**
     * Remove an <code>IProxy</code> from the <code>Model</code> by name.
     * 
     * @param proxyName
     *            the <code>IProxy</code> to remove from the <code>Model</code>.
     * @return the <code>IProxy</code> that was removed from the
     *         <code>Model</code>
     */
    public IProxy removeProxy(String proxyName) {
        return this.model.removeProxy(proxyName);
    }

    /**
     * Retrieve an <code>IMediator</code> from the <code>View</code>.
     * 
     * @param mediatorName
     * @return the <code>IMediator</code> previously registered with the given
     *         <code>mediatorName</code>.
     */
    public IMediator retrieveMediator(String mediatorName) {
        return this.view.retrieveMediator(mediatorName);
    }

    /**
     * Retrieve an <code>IProxy</code> from the <code>Model</code> by name.
     * 
     * @param proxyName
     *            the name of the proxy to be retrieved.
     * @return the <code>IProxy</code> instance previously registered with the
     *         given <code>proxyName</code>.
     */
    public IProxy retrieveProxy(String proxyName) {
        return this.model.retrieveProxy(proxyName);
    }

    /**
     * Create and send an <code>INotification</code>.
     * 
     * <P>
     * Keeps us from having to construct new notification instances in our
     * implementation code.
     * 
     * @param notificationName
     *            the name of the notiification to send
     * @param body
     *            the body of the notification (optional)
     * @param type
     *            the type of the notification (optional)
     */
    public void sendNotification(String notificationName, Object body, String type) {
        notifyObservers(new Notification(notificationName, body, type));
    }

    /**
     * Notify <code>Observer</code>s.
     * 
     * @param notification
     *            the <code>INotification</code> to have the <code>View</code>
     *            notify <code>Observers</code> of.
     */
    public void notifyObservers(INotification notification) {
        this.view.notifyObservers(notification);
    }

}
