
## Mouse of Horrors

A fork of House of Mirrors, as an exercise in Scala development.

Thanks to the author of [the original code](http://code.google.com/p/houseofmirrors/).

### Notes

Requires Scala 2.9 for trivial API (sys.exit).

A build file for SBT is included.

### Summary

1. The refactored code uses the PureMVC framework in Java, the primary virtue of which is to ensure
that UI events are converted to application events for processing.  "Evil" (Martin Odersky)
protected static data is removed from the framework code included here, with other minor changes
for usability and testability.

2. The UI components use the scala.swing idiom for registering event handlers.

3. Guice is used for wiring (again, as an exercise).

4. There are a few minor bug fixes (such as when dragging gates) and a few minor features (such as
rotating a gate with the scroll wheel and descending with a click).

5. There are only a few more comments.

