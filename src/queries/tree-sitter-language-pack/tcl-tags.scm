; Tcl tags for Atlas repo map

; Procedure definitions
(procedure
  name: (simple_word) @name.definition.function) @definition.function

; Namespace definitions
(namespace
  name: (simple_word) @name.definition.class) @definition.class

; Variable declarations (set, variable)
(command
  name: (simple_word) @_cmd
  argument: (simple_word) @name.definition.constant
  (#match? @_cmd "^(set|variable)$")) @definition.constant

; Procedure calls
(command
  name: (simple_word) @name.reference.call) @reference.call

