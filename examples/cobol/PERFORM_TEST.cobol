       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           PERFORM-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  COUNTER                     PIC 9(3).
       01  LOOP-VAR                    PIC X(10).
       01  MORE-DATA                   PIC X(3) VALUE 'YES'.
       
       PROCEDURE DIVISION.
       100-MAIN.
           MOVE 0 TO COUNTER
           MOVE 'LOOP' TO LOOP-VAR
           
           PERFORM UNTIL MORE-DATA = 'NO'
               DISPLAY 'COUNTER IS ' COUNTER
               ADD 1 TO COUNTER
               IF COUNTER > 5 THEN
                   MOVE 'NO' TO MORE-DATA
               END-IF
           END-PERFORM
           
           PERFORM A000-COUNT 3 TIMES
           
           GOBACK.
       
       A000-COUNT.
           DISPLAY 'COUNTING...'
           ADD 1 TO COUNTER. 