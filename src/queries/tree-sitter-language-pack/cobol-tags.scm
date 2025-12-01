; COBOL tags for Atlas repo map
; Based on tree-sitter-cobol grammar from https://github.com/nolanlwin/tree-sitter-cobol

; Program definitions
(program_definition
  (identification_division
    (program_name) @name.definition.class)) @definition.class

; Function definitions (COBOL functions)
(function_definition
  (function_division
    (program_name) @name.definition.function)) @definition.function

; Paragraph definitions (COBOL procedures)
(paragraph
  name: (_) @name.definition.function) @definition.function

; Section definitions in PROCEDURE DIVISION
(section
  name: (_) @name.definition.function) @definition.function

; Data item definitions (WORKING-STORAGE, FILE SECTION, etc.)
(data_description_entry
  name: (_) @name.definition.constant) @definition.constant

; File definitions
(file_description_entry
  (fd_file_name) @name.definition.constant) @definition.constant

; PERFORM calls (references to paragraphs/sections)
(perform_statement
  procedure: (_) @name.reference.call) @reference.call

; CALL statements (external program calls)
(call_statement
  program: (_) @name.reference.call) @reference.call

; GO TO statements
(go_to_statement
  procedure: (_) @name.reference.call) @reference.call

