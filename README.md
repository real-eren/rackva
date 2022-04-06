# interpreter

Interpreter for a C/Java-like language, written in Scheme/Racket

`interpreter.rkt` contains 
 - interpret
 - Mstate
 - Mvalue
 - Mboolean

`interpreter-extension.rkt` contains overloads for interpret, including simple-interpret

`conts.rkt` contains the continuation mapping

`state.rkt` contains the state implementation

`map.rkt` contains an implementation of a map (associative list)
