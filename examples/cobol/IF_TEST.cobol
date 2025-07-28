       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           IF-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-VAR                    PIC X(10).
       01  RESULT-VAR                  PIC X(10).
       01  CHOICE                      PIC 9(1).
       
       PROCEDURE DIVISION.
       100-MAIN.
           MOVE 'HELLO' TO TEST-VAR
           MOVE 1 TO CHOICE
           
           IF CHOICE = 1 THEN
               MOVE 'ONE' TO RESULT-VAR
               DISPLAY 'CHOICE IS ONE'
           ELSE
               MOVE 'OTHER' TO RESULT-VAR
               DISPLAY 'CHOICE IS OTHER'
           END-IF
           
           IF TEST-VAR = 'HELLO' THEN
               DISPLAY 'TEST-VAR IS HELLO'
           ELSE
               DISPLAY 'TEST-VAR IS NOT HELLO'
           END-IF
           
           GOBACK. 