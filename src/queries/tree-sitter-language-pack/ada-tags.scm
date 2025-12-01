; Ada tags for Atlas repo map

; Package definitions
(package_declaration
  (identifier) @name.definition.class) @definition.class

(package_body
  (identifier) @name.definition.class) @definition.class

; Procedure definitions
(procedure_specification
  name: (identifier) @name.definition.function) @definition.function

(subprogram_body
  (procedure_specification
    name: (identifier) @name.definition.function)) @definition.function

; Function definitions
(function_specification
  name: (identifier) @name.definition.function) @definition.function

; Type definitions
(full_type_declaration
  (identifier) @name.definition.class) @definition.class

; Variable/constant declarations
(object_declaration
  (defining_identifier_list
    (identifier) @name.definition.constant)) @definition.constant

; Procedure calls
(procedure_call_statement
  (identifier) @name.reference.call) @reference.call

; Function calls
(function_call
  (identifier) @name.reference.call) @reference.call

