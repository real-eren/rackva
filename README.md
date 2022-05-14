# Rava interpreter
name inspired by Jython

Interpreter for a Java-like language, written in Scheme/Racket.
Apart from variables and fields being boxed, within the intepreter, values are immutable and functions are referentially-transparent.

## Authors
v1.0.0 - v3.0.1: Duc Huy Nguyen, Eren Kahriman, Loc Nguyen

v3.0.1+ : Eren Kahriman 

## Language Features:
- Dynamic typing
- Stack trace for user errors

### V1
- Int and Bool literals, expressions
- Basic Arithmetic and Logical operators
- Short-circuiting || and &&
- Variables
- If, While, Try, block statements

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
    - constructor chaining
  - polymorphism
  - abstract methods
- dot operator
  - Java (8) semantics

