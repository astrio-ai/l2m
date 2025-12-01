; Fortran tags for Atlas repo map

; Program definitions
(program
  (program_statement
    name: (name) @name.definition.class)) @definition.class

; Module definitions
(module
  (module_statement
    name: (name) @name.definition.class)) @definition.class

; Subroutine definitions
(subroutine
  (subroutine_statement
    name: (name) @name.definition.function)) @definition.function

; Function definitions
(function
  (function_statement
    name: (name) @name.definition.function)) @definition.function

; Variable declarations
(variable_declaration
  (identifier) @name.definition.constant) @definition.constant

; Subroutine calls
(subroutine_call
  (identifier) @name.reference.call) @reference.call

; Function calls
(call_expression
  (identifier) @name.reference.call) @reference.call

