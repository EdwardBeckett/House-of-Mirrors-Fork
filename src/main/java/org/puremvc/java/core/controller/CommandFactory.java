package org.puremvc.java.core.controller;

import org.puremvc.java.interfaces.ICommand;

/**
 * Factory for a (fully-configured) command handler.
 * Used by Controller and MacroCommand to create handlers.
 */
public interface CommandFactory {
    ICommand create(Class<? extends ICommand> klass);
}