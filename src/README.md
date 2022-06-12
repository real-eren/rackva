
## Notable files

`interpreter.rkt` contains 
 - `interpret`
 - `Mstate`s
 - `Mvalue`s
 - `Mboolean`s
 - `Mname`s

`conts.rkt` contains the continuation mapping

`src-gen.rkt` converts AST -> source code

`user-errors.rkt` defines user-facing errors for the interpreter

### state
| file | desc |
| :-----:             | ---------------------------------- |
|`state.rkt`          | the conglomerate representation of the program state |
|`var-table.rkt`      | a `map` representing a table of var bindings |
|`function-table.rkt` | a table of function bindings |
|`class.rkt`          | a `map` representing a class closure |
|`instance.rkt`       | a `map` representing an instance |


### utils
`map.rkt` contains an implementation of a map (associative list)


## Map Keys
fixed keys (defined constants) are of the form:
`$property`
