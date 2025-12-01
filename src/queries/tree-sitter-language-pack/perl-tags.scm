; Perl tags for Atlas repo map

; Package definitions
(package_statement
  (package) @name.definition.class) @definition.class

; Subroutine definitions
(subroutine_declaration_statement
  (bareword) @name.definition.function) @definition.function

; Named subroutine definitions
(named_block_statement
  (bareword) @name.definition.function) @definition.function

; Variable declarations (my, our, local)
(variable_declaration
  (scalar) @name.definition.constant) @definition.constant

(variable_declaration
  (array) @name.definition.constant) @definition.constant

(variable_declaration
  (hash) @name.definition.constant) @definition.constant

; Function calls
(function_call_expression
  (function) @name.reference.call) @reference.call

; Method calls
(method_call_expression
  (method) @name.reference.call) @reference.call

