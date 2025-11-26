;; https://tree-sitter.github.io/tree-sitter/4-code-navigation.html

;;Function definitions
(binding
  attrpath: (attrpath attr: (identifier)) @name
  expression: (function_expression) @definition.function)
;;Function/method calls
(apply_expression function: (apply_expression function: (variable_expression name: (identifier) @name)) @reference.call)

;; TODO: (if even applicable?)
;;Interface definitions       @definition.interface
;;Interface implementation    @reference.implementation
;;Class definitions           @definition.class
;;Class reference             @reference.class
;;Method definitions          @definition.method
;;Module definitions          @definition.module
