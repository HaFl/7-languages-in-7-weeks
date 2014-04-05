#!/usr/local/bin/io

/*
FIND

sample code: http://www.iolanguage.com/about/samplecode
io community: iolanguage.org
style guide: http://en.wikibooks.org/wiki/Io_Programming/Io_Style_Guide
*/


/*
ANSWER

strongly typed as 1 + "one" does not work
*/
(true and 0) println     // true
(true and "") println    // true
(true and nil) println   // false

Object slotNames println

/*
::= creates slot, creates setter (e.g. setBlah), assigns value (good if you
    want to expose an object's attribute)
:= creates slot, assigns value
= assigns value to slot if it exists, otherwise raises exception
*/


// DO
Stuff := Object clone
Stuff macro := method(name, perform(name))
oh_yeah := method("Oh yeah!" println)
Stuff macro("oh_yeah")
