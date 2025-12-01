; Verilog tags for Atlas repo map

; Module declarations
(module_declaration
  (module_header
    (simple_identifier) @name.definition.class)) @definition.class

; Interface declarations
(interface_declaration
  (interface_header
    (simple_identifier) @name.definition.class)) @definition.class

; Package declarations
(package_declaration
  (simple_identifier) @name.definition.class) @definition.class

; Class declarations
(class_declaration
  (simple_identifier) @name.definition.class) @definition.class

; Task declarations
(task_declaration
  (task_identifier
    (simple_identifier) @name.definition.function)) @definition.function

; Function declarations
(function_declaration
  (function_identifier
    (simple_identifier) @name.definition.function)) @definition.function

; Always blocks (named)
(always_construct
  (statement
    (statement_item
      (seq_block
        (identifier) @name.definition.function)))) @definition.function

; Net declarations
(net_declaration
  (list_of_net_decl_assignments
    (net_decl_assignment
      (simple_identifier) @name.definition.constant))) @definition.constant

; Variable declarations
(data_declaration
  (list_of_variable_decl_assignments
    (variable_decl_assignment
      (simple_identifier) @name.definition.constant))) @definition.constant

; Parameter declarations
(parameter_declaration
  (list_of_param_assignments
    (param_assignment
      (simple_identifier) @name.definition.constant))) @definition.constant

; Module instantiation
(module_instantiation
  (simple_identifier) @name.reference.call) @reference.call

; Task/function calls
(subroutine_call
  (tf_call
    (simple_identifier) @name.reference.call)) @reference.call

