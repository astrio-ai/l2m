; VHDL tags for Atlas repo map

; Entity declarations
(entity_declaration
  (identifier) @name.definition.class) @definition.class

; Architecture bodies
(architecture_body
  (identifier) @name.definition.class) @definition.class

; Package declarations
(package_declaration
  (identifier) @name.definition.class) @definition.class

; Package bodies
(package_body
  (identifier) @name.definition.class) @definition.class

; Process statements
(process_statement
  (label
    (identifier) @name.definition.function)?) @definition.function

; Procedure declarations
(procedure_declaration
  (identifier) @name.definition.function) @definition.function

; Function declarations
(function_declaration
  (identifier) @name.definition.function) @definition.function

; Signal declarations
(signal_declaration
  (identifier_list
    (identifier) @name.definition.constant)) @definition.constant

; Variable declarations
(variable_declaration
  (identifier_list
    (identifier) @name.definition.constant)) @definition.constant

; Constant declarations
(constant_declaration
  (identifier_list
    (identifier) @name.definition.constant)) @definition.constant

; Component declarations
(component_declaration
  (identifier) @name.definition.class) @definition.class

; Procedure/function calls
(procedure_call_statement
  (simple_name) @name.reference.call) @reference.call

(function_call
  (simple_name) @name.reference.call) @reference.call

