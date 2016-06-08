Optic UI (v2)
================

[![Build Status](https://travis-ci.org/zrho/purescript-optic-ui.svg?branch=master)](https://travis-ci.org/zrho/purescript-optic-ui)
[![Maintainer: zrho](https://img.shields.io/badge/maintainer-zrho-lightgrey.svg)](http://github.com/zrho)

Optic UI is a Purescript library that allows to write single page web user
interfaces declaratively and concisely with the help of lenses and traversals:

UI components are defined in a profunctor that gives them access to a view
model, a state local to the component. Child components that access only a part
of the state of their parent can be embedded in a bigger component using lenses
and traversals that focus on the respective sub-states. Components provide a
handler function that - given a new state - triggers an update of the UI and
generate a view that is finally rendered using a virtual dom library.

Version 2
---------------------

This branch contains a work-in-progress implementation of version 2 of OpticUI.
The following things have changed since version 1:

 - OpticUI now no longer contains a data type for markup and the corresponding
   rendering functions; use `purescript-markup` and `purescript-markup-incdom`,
   or roll your own.
 - `UI` is an instance of `Wander` now.
 - Markup is now represented by functors implementing `Plus`. The type argument
   to the functor is the type of event raised by the markup. This removes the
   complexity arising from plumbing handlers through the `UI` type.
 - An additional functor implementing `Plus` can be used for subscriptions. This
   replaces and generalizes the signal system from OpticUI 1.

Examples
---------------------

For examples, have a look at the `examples` directory:

- [Todo Manager](examples/todo/src/Main.purs)
- [AJAX Example](examples/ajax/src/Main.purs)
