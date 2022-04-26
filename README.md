# interpreter

Interpreter for a Java-like language, written in Scheme/Racket

## Language Features:
Dynamic Typing
Stack Trace for user errors

### V1
- Int and Bool literals, expressions
- Basic Arithmetic and Logical operators
- Short-circuiting || and &&
- Variables
- If, While, Try, 

### V2
- V1 features
- functions
  - top-level and nested
  - pass-by-value and pass-by-reference parameters
  - w/ or w/out return value
  - overloading by # parameters

### V3
- V2 features
- classes
  - static members
  - instance members
  - user-defined constructors
  - polymorphism
  - abstract methods
- dot operator
  - Java (8) semantics

## Notable files

`interpreter.rkt` contains 
 - interpret
 - `Mstate`s
 - `Mvalue`s
 - `Mboolean`s
 - `Mname`s

`interpreter-extension.rkt` contains overloads for interpret

`conts.rkt` contains the continuation mapping

### state
`state.rkt` - the conglomerate representation of the program state
`var-table.rkt` - a `map` representing a table of var bindings
`function-table.rkt` - a table of function bindings
`class.rkt` - a `map` representing a class closure
`instance.rkt` - a `map` representing a 


### utils
`map.rkt` contains an implementation of a map (associative list)
`testing.rkt` contains helpers for creating concise unit tests


