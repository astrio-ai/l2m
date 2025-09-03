       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           FILE-IO-TEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'INPUT.TXT'.
           SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.TXT'.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  INPUT-DATA              PIC X(80).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  OUTPUT-DATA             PIC X(80).

       WORKING-STORAGE SECTION.
       01  EOF-FLAG                    PIC X VALUE 'N'.
       01  LINE-COUNT                  PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       100-MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ INPUT-FILE
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
               
               IF EOF-FLAG NOT = 'Y'
                   MOVE INPUT-DATA TO OUTPUT-DATA
                   WRITE OUTPUT-RECORD
                   ADD 1 TO LINE-COUNT
               END-IF
           END-PERFORM
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           
           DISPLAY 'PROCESSED ' LINE-COUNT ' LINES'
           
           GOBACK. 