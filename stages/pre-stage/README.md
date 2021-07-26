# grasp-android
GRASP for Android

This is the second prototype of
a structural editor for Lisp - this one
is written in Java and works with touch 
screens on Android devices.

You can watch a demo
[here](https://youtu.be/BmZ39IfElzg)

The code was written entirely on my smartphone
and built using the BuildAPKs project from
Termux (this turned out to work much faster
and use much less resources than running 
Android Studio).

Editing with GRASP may seem inefficient,
but it's actually a first step towards
the idea of "editable interactions": I imagine
that eventually the user should be able
to specify specialized editing modes for
different sorts of data (e.g. trees, graphs,
3d meshes, shapes, sounds etc.) as well as 
a dedicated editor for Scheme code (GRASP 
stands for GRAphical Scheme Programming), 
or perhaps some typed variant of thereof.

The core idea is to deconstruct the (false
but common) belief that programming languages
must have syntax and be based on text, and
to build a system which allows to operate
directly on structures (just like file systems
allow to operate on directory trees and files
instead of raw sequences of bytes on a disk).
