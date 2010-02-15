AwesomePrelude
==============

The AwesomePrelude is reimplementation of the Haskell prelude in which all data types have been replaced with type classes. Embedded Domain Specific Languages (DSLs) can implement these type classes to get access to the functions defined in terms of the type classes.  
For example: by implementing the `BoolC` type class, a DSL gets to use the `(&&)`, `(||)` and `not` functions.

Background information
----------------------

 * The blog post "[Deeply embedded DSLs in Haskell](http://tom.lokhorst.eu/2009/09/deeply-embedded-dsls)" from september 2009 explains some of the ideas behind the AwesomePrelude, however the implementation of the AwesomePrelude is radically different now.
 * At the february 2010 meeting of the Dutch Haskell User Group, Tom and Sebastiaan presented the current state of the project. See [the video recording](http://tom.lokhorst.eu/2010/02/awesomeprelude-presentation-video) of that presentation.

Authors
-------

 * Tom Lokhorst
 * Sebastiaan Visser

 Additional contributions by:

 * Chris Eidhof
