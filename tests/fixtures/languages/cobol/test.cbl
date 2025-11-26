       IDENTIFICATION DIVISION.
       PROGRAM-ID. GREETER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(20).
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           MOVE "World" TO WS-NAME.
           PERFORM GREET-PROCEDURE.
           STOP RUN.
       
       GREET-PROCEDURE.
           DISPLAY "Hello, " WS-NAME.

