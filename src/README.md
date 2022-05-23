
## Notable files

`interpreter.rkt` contains 
 - `interpret`
 - `Mstate`s
 - `Mvalue`s
 - `Mboolean`s
 - `Mname`s

`interpreter-extension.rkt` contains overloads for interpret, including legacy versions

`conts.rkt` contains the continuation mapping

`src-gen.rkt` converts AST from parsers -> source code

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

`testing.rkt` contains helpers for creating concise unit tests


## Map Keys
fixed keys (defined constants) are of the form:
`$property`
