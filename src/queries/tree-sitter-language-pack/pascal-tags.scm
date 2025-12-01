; Pascal tags for Atlas repo map

; Program definition
(program
  (moduleName
    (identifier) @name.definition.class)) @definition.class

; Unit definition
(unit
  (moduleName
    (identifier) @name.definition.class)) @definition.class

; Procedure definitions
(defProc
  (declProc
    (identifier) @name.definition.function)) @definition.function

; Function definitions
(defFunc
  (declFunc
    (identifier) @name.definition.function)) @definition.function

; Type definitions
(declType
  (identifier) @name.definition.class) @definition.class

; Variable declarations
(declVar
  (identifier) @name.definition.constant) @definition.constant

; Constant declarations
(declConst
  (identifier) @name.definition.constant) @definition.constant

; Procedure/function calls
(call
  (identifier) @name.reference.call) @reference.call

