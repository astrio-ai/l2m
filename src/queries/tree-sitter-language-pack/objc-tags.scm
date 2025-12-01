; Objective-C tags for Atlas repo map

; Class interface declarations
(class_interface
  name: (identifier) @name.definition.class) @definition.class

; Class implementation
(class_implementation
  name: (identifier) @name.definition.class) @definition.class

; Category interface
(category_interface
  name: (identifier) @name.definition.class) @definition.class

; Category implementation
(category_implementation
  name: (identifier) @name.definition.class) @definition.class

; Protocol declarations
(protocol_declaration
  name: (identifier) @name.definition.class) @definition.class

; Method declarations
(method_declaration
  selector: (identifier) @name.definition.function) @definition.function

; Method definitions
(method_definition
  selector: (identifier) @name.definition.function) @definition.function

; Property declarations
(property_declaration
  (identifier) @name.definition.constant) @definition.constant

; Function definitions (C-style)
(function_definition
  declarator: (function_declarator
    declarator: (identifier) @name.definition.function)) @definition.function

; Message expressions (method calls)
(message_expression
  selector: (identifier) @name.reference.call) @reference.call

; Function calls
(call_expression
  function: (identifier) @name.reference.call) @reference.call

