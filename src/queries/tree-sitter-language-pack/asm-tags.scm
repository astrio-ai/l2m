; Assembly tags for Atlas repo map

; Label definitions (function/procedure entry points)
(label
  (identifier) @name.definition.function) @definition.function

; Constant definitions
(constant
  name: (identifier) @name.definition.constant) @definition.constant

; Data definitions
(directive
  name: (identifier) @name.definition.constant) @definition.constant

; Macro definitions
(macro
  name: (identifier) @name.definition.function) @definition.function

; Section definitions
(section
  name: (identifier) @name.definition.class) @definition.class

; Call instructions
(instruction
  mnemonic: (identifier) @_mnemonic
  (identifier) @name.reference.call
  (#match? @_mnemonic "^(call|jmp|je|jne|jz|jnz|jg|jl)$")) @reference.call

