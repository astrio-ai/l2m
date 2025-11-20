      ******************************************************************        
      * Program     : COTRN00C.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Program
      * Function    : List Transactions from TRANSACT file
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COTRN00C.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'COTRN00C'.
         05 WS-TRANID                  PIC X(04) VALUE 'CT00'.
         05 WS-MESSAGE                 PIC X(80) VALUE SPACES.
         05 WS-TRANSACT-FILE             PIC X(08) VALUE 'TRANSACT'.
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.
           88 ERR-FLG-ON                         VALUE 'Y'.
           88 ERR-FLG-OFF                        VALUE 'N'.
         05 WS-TRANSACT-EOF            PIC X(01) VALUE 'N'.
           88 TRANSACT-EOF                       VALUE 'Y'.
           88 TRANSACT-NOT-EOF                   VALUE 'N'.
         05 WS-SEND-ERASE-FLG          PIC X(01) VALUE 'Y'.
           88 SEND-ERASE-YES                     VALUE 'Y'.
           88 SEND-ERASE-NO                      VALUE 'N'.

         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REC-COUNT               PIC S9(04) COMP VALUE ZEROS.
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.
         05 WS-PAGE-NUM                PIC S9(04) COMP VALUE ZEROS.

         05 WS-TRAN-AMT                PIC +99999999.99.
         05 WS-TRAN-DATE               PIC X(08) VALUE '00/00/00'.



       COPY COCOM01Y.
          05 CDEMO-CT00-INFO.
             10 CDEMO-CT00-TRNID-FIRST     PIC X(16).
             10 CDEMO-CT00-TRNID-LAST      PIC X(16).
             10 CDEMO-CT00-PAGE-NUM        PIC 9(08).
             10 CDEMO-CT00-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
                88 NEXT-PAGE-YES                     VALUE 'Y'.
                88 NEXT-PAGE-NO                      VALUE 'N'.
             10 CDEMO-CT00-TRN-SEL-FLG     PIC X(01).
             10 CDEMO-CT00-TRN-SELECTED    PIC X(16).

       COPY COTRN00.

       COPY COTTL01Y.
       COPY CSDAT01Y.
       COPY CSMSG01Y.

       COPY CVTRA05Y.

       COPY DFHAID.
       COPY DFHBMSCA.

      *----------------------------------------------------------------*
      *                        LINKAGE SECTION
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

      *----------------------------------------------------------------*
      *                       PROCEDURE DIVISION
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PARA.

           SET ERR-FLG-OFF TO TRUE
           SET TRANSACT-NOT-EOF TO TRUE
           SET NEXT-PAGE-NO TO TRUE
           SET SEND-ERASE-YES TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COTRN0AO

           MOVE -1       TO TRNIDINL OF COTRN0AI

           IF EIBCALEN = 0
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO COTRN0AO
                   PERFORM PROCESS-ENTER-KEY
                   PERFORM SEND-TRNLST-SCREEN
               ELSE
                   PERFORM RECEIVE-TRNLST-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           MOVE 'COMEN01C' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN DFHPF7
                           PERFORM PROCESS-PF7-KEY
                       WHEN DFHPF8
                           PERFORM PROCESS-PF8-KEY
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
                           MOVE -1       TO TRNIDINL OF COTRN0AI
                           MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                           PERFORM SEND-TRNLST-SCREEN
                   END-EVALUATE
               END-IF
           END-IF

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      PROCESS-ENTER-KEY
      *----------------------------------------------------------------*
       PROCESS-ENTER-KEY.

           EVALUATE TRUE
               WHEN SEL0001I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0001I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID01I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0002I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0002I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID02I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0003I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0003I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID03I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0004I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0004I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID04I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0005I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0005I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID05I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0006I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0006I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID06I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0007I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0007I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID07I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0008I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0008I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID08I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0009I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0009I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID09I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN SEL0010I OF COTRN0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0010I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE TRNID10I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
               WHEN OTHER
                   MOVE SPACES   TO CDEMO-CT00-TRN-SEL-FLG
                   MOVE SPACES   TO CDEMO-CT00-TRN-SELECTED
           END-EVALUATE
           IF (CDEMO-CT00-TRN-SEL-FLG NOT = SPACES AND LOW-VALUES) AND
              (CDEMO-CT00-TRN-SELECTED NOT = SPACES AND LOW-VALUES)
               EVALUATE CDEMO-CT00-TRN-SEL-FLG
                   WHEN 'S'
                   WHEN 's'
                        MOVE 'COTRN01C'   TO CDEMO-TO-PROGRAM
                        MOVE WS-TRANID    TO CDEMO-FROM-TRANID
                        MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
                        MOVE 0        TO CDEMO-PGM-CONTEXT
                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                   WHEN OTHER
      *                SET TRANSACT-EOF TO TRUE
                       MOVE
                       'Invalid selection. Valid value is S' TO
                                       WS-MESSAGE
                       MOVE -1       TO TRNIDINL OF COTRN0AI
      *                PERFORM SEND-TRNLST-SCREEN
               END-EVALUATE
           END-IF

           IF TRNIDINI OF COTRN0AI = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO TRAN-ID
           ELSE
               IF TRNIDINI  OF COTRN0AI IS NUMERIC
                   MOVE TRNIDINI  OF COTRN0AI    TO TRAN-ID
               ELSE
                   MOVE 'Y'                       TO WS-ERR-FLG
                   MOVE
                   'Tran ID must be Numeric ...' TO
                                   WS-MESSAGE
                   MOVE -1                 TO TRNIDINL OF COTRN0AI
                   PERFORM SEND-TRNLST-SCREEN
               END-IF
           END-IF

           MOVE -1       TO TRNIDINL OF COTRN0AI


           MOVE 0       TO CDEMO-CT00-PAGE-NUM
           PERFORM PROCESS-PAGE-FORWARD

           IF NOT ERR-FLG-ON
               MOVE SPACE   TO TRNIDINO  OF COTRN0AO
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PF7-KEY
      *----------------------------------------------------------------*
       PROCESS-PF7-KEY.

           IF CDEMO-CT00-TRNID-FIRST = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO TRAN-ID
           ELSE
               MOVE CDEMO-CT00-TRNID-FIRST TO TRAN-ID
           END-IF

           SET NEXT-PAGE-YES TO TRUE
           MOVE -1       TO TRNIDINL OF COTRN0AI

           IF CDEMO-CT00-PAGE-NUM > 1
               PERFORM PROCESS-PAGE-BACKWARD
           ELSE
               MOVE 'You are already at the top of the page...' TO
                               WS-MESSAGE
               SET SEND-ERASE-NO TO TRUE
               PERFORM SEND-TRNLST-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PF8-KEY
      *----------------------------------------------------------------*
       PROCESS-PF8-KEY.

           IF CDEMO-CT00-TRNID-LAST = SPACES OR LOW-VALUES
               MOVE HIGH-VALUES TO TRAN-ID
           ELSE
               MOVE CDEMO-CT00-TRNID-LAST TO TRAN-ID
           END-IF

           MOVE -1       TO TRNIDINL OF COTRN0AI

           IF NEXT-PAGE-YES
               PERFORM PROCESS-PAGE-FORWARD
           ELSE
               MOVE 'You are already at the bottom of the page...' TO
                               WS-MESSAGE
               SET SEND-ERASE-NO TO TRUE
               PERFORM SEND-TRNLST-SCREEN
           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PAGE-FORWARD
      *----------------------------------------------------------------*
       PROCESS-PAGE-FORWARD.

           PERFORM STARTBR-TRANSACT-FILE

           IF NOT ERR-FLG-ON

               IF EIBAID NOT = DFHENTER AND DFHPF7 AND DFHPF3
                   PERFORM READNEXT-TRANSACT-FILE
               END-IF

               IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                  PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
                      PERFORM INITIALIZE-TRAN-DATA
                  END-PERFORM
               END-IF

               MOVE 1             TO  WS-IDX

               PERFORM UNTIL WS-IDX >= 11 OR TRANSACT-EOF OR ERR-FLG-ON
                   PERFORM READNEXT-TRANSACT-FILE
                   IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                       PERFORM POPULATE-TRAN-DATA
                       COMPUTE WS-IDX = WS-IDX + 1
                   END-IF
               END-PERFORM

               IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                   COMPUTE CDEMO-CT00-PAGE-NUM =
                           CDEMO-CT00-PAGE-NUM + 1
                   PERFORM READNEXT-TRANSACT-FILE
                   IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                       SET NEXT-PAGE-YES TO TRUE
                   ELSE
                       SET NEXT-PAGE-NO TO TRUE
                   END-IF
               ELSE
                   SET NEXT-PAGE-NO TO TRUE
                   IF WS-IDX > 1
                       COMPUTE CDEMO-CT00-PAGE-NUM = CDEMO-CT00-PAGE-NUM
                        + 1
                   END-IF
               END-IF

               PERFORM ENDBR-TRANSACT-FILE

               MOVE CDEMO-CT00-PAGE-NUM TO PAGENUMI  OF COTRN0AI
               MOVE SPACE   TO TRNIDINO  OF COTRN0AO
               PERFORM SEND-TRNLST-SCREEN

           END-IF.

      *----------------------------------------------------------------*
      *                      PROCESS-PAGE-BACKWARD
      *----------------------------------------------------------------*
       PROCESS-PAGE-BACKWARD.

           PERFORM STARTBR-TRANSACT-FILE

           IF NOT ERR-FLG-ON

               IF EIBAID NOT = DFHENTER AND DFHPF8
                   PERFORM READPREV-TRANSACT-FILE
               END-IF

               IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                  PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
                     PERFORM INITIALIZE-TRAN-DATA
                  END-PERFORM
               END-IF

               MOVE 10          TO  WS-IDX

               PERFORM UNTIL WS-IDX <= 0 OR TRANSACT-EOF OR ERR-FLG-ON
                   PERFORM READPREV-TRANSACT-FILE
                   IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                       PERFORM POPULATE-TRAN-DATA
                       COMPUTE WS-IDX = WS-IDX - 1
                   END-IF
               END-PERFORM

               IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
                  PERFORM READPREV-TRANSACT-FILE
                  IF NEXT-PAGE-YES
                     IF TRANSACT-NOT-EOF AND ERR-FLG-OFF AND
                        CDEMO-CT00-PAGE-NUM > 1
                        SUBTRACT 1 FROM CDEMO-CT00-PAGE-NUM
                     ELSE
                        MOVE 1 TO CDEMO-CT00-PAGE-NUM
                     END-IF
                  END-IF
               END-IF

               PERFORM ENDBR-TRANSACT-FILE

               MOVE CDEMO-CT00-PAGE-NUM TO PAGENUMI  OF COTRN0AI
               PERFORM SEND-TRNLST-SCREEN

           END-IF.

      *----------------------------------------------------------------*
      *                      POPULATE-TRAN-DATA
      *----------------------------------------------------------------*
       POPULATE-TRAN-DATA.

           MOVE TRAN-AMT                  TO WS-TRAN-AMT
           MOVE TRAN-ORIG-TS              TO WS-TIMESTAMP
           MOVE WS-TIMESTAMP-DT-YYYY(3:2) TO WS-CURDATE-YY
           MOVE WS-TIMESTAMP-DT-MM        TO WS-CURDATE-MM
           MOVE WS-TIMESTAMP-DT-DD        TO WS-CURDATE-DD
           MOVE WS-CURDATE-MM-DD-YY       TO WS-TRAN-DATE

           EVALUATE WS-IDX
               WHEN 1
                   MOVE TRAN-ID    TO TRNID01I OF COTRN0AI
                                         CDEMO-CT00-TRNID-FIRST
                   MOVE WS-TRAN-DATE TO TDATE01I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC01I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT001I OF COTRN0AI
               WHEN 2
                   MOVE TRAN-ID    TO TRNID02I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE02I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC02I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT002I OF COTRN0AI
               WHEN 3
                   MOVE TRAN-ID    TO TRNID03I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE03I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC03I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT003I OF COTRN0AI
               WHEN 4
                   MOVE TRAN-ID    TO TRNID04I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE04I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC04I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT004I OF COTRN0AI
               WHEN 5
                   MOVE TRAN-ID    TO TRNID05I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE05I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC05I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT005I OF COTRN0AI
               WHEN 6
                   MOVE TRAN-ID    TO TRNID06I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE06I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC06I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT006I OF COTRN0AI
               WHEN 7
                   MOVE TRAN-ID    TO TRNID07I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE07I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC07I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT007I OF COTRN0AI
               WHEN 8
                   MOVE TRAN-ID    TO TRNID08I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE08I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC08I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT008I OF COTRN0AI
               WHEN 9
                   MOVE TRAN-ID    TO TRNID09I OF COTRN0AI
                   MOVE WS-TRAN-DATE TO TDATE09I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC09I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT009I OF COTRN0AI
               WHEN 10
                   MOVE TRAN-ID    TO TRNID10I OF COTRN0AI
                                         CDEMO-CT00-TRNID-LAST
                   MOVE WS-TRAN-DATE TO TDATE10I OF COTRN0AI
                   MOVE TRAN-DESC TO TDESC10I OF COTRN0AI
                   MOVE WS-TRAN-AMT  TO TAMT010I OF COTRN0AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      INITIALIZE-TRAN-DATA
      *----------------------------------------------------------------*
       INITIALIZE-TRAN-DATA.

           EVALUATE WS-IDX
               WHEN 1
                   MOVE SPACES TO TRNID01I OF COTRN0AI
                   MOVE SPACES TO TDATE01I OF COTRN0AI
                   MOVE SPACES TO TDESC01I OF COTRN0AI
                   MOVE SPACES TO TAMT001I OF COTRN0AI
               WHEN 2
                   MOVE SPACES TO TRNID02I OF COTRN0AI
                   MOVE SPACES TO TDATE02I OF COTRN0AI
                   MOVE SPACES TO TDESC02I OF COTRN0AI
                   MOVE SPACES TO TAMT002I OF COTRN0AI
               WHEN 3
                   MOVE SPACES TO TRNID03I OF COTRN0AI
                   MOVE SPACES TO TDATE03I OF COTRN0AI
                   MOVE SPACES TO TDESC03I OF COTRN0AI
                   MOVE SPACES TO TAMT003I OF COTRN0AI
               WHEN 4
                   MOVE SPACES TO TRNID04I OF COTRN0AI
                   MOVE SPACES TO TDATE04I OF COTRN0AI
                   MOVE SPACES TO TDESC04I OF COTRN0AI
                   MOVE SPACES TO TAMT004I OF COTRN0AI
               WHEN 5
                   MOVE SPACES TO TRNID05I OF COTRN0AI
                   MOVE SPACES TO TDATE05I OF COTRN0AI
                   MOVE SPACES TO TDESC05I OF COTRN0AI
                   MOVE SPACES TO TAMT005I OF COTRN0AI
               WHEN 6
                   MOVE SPACES TO TRNID06I OF COTRN0AI
                   MOVE SPACES TO TDATE06I OF COTRN0AI
                   MOVE SPACES TO TDESC06I OF COTRN0AI
                   MOVE SPACES TO TAMT006I OF COTRN0AI
               WHEN 7
                   MOVE SPACES TO TRNID07I OF COTRN0AI
                   MOVE SPACES TO TDATE07I OF COTRN0AI
                   MOVE SPACES TO TDESC07I OF COTRN0AI
                   MOVE SPACES TO TAMT007I OF COTRN0AI
               WHEN 8
                   MOVE SPACES TO TRNID08I OF COTRN0AI
                   MOVE SPACES TO TDATE08I OF COTRN0AI
                   MOVE SPACES TO TDESC08I OF COTRN0AI
                   MOVE SPACES TO TAMT008I OF COTRN0AI
               WHEN 9
                   MOVE SPACES TO TRNID09I OF COTRN0AI
                   MOVE SPACES TO TDATE09I OF COTRN0AI
                   MOVE SPACES TO TDESC09I OF COTRN0AI
                   MOVE SPACES TO TAMT009I OF COTRN0AI
               WHEN 10
                   MOVE SPACES TO TRNID10I OF COTRN0AI
                   MOVE SPACES TO TDATE10I OF COTRN0AI
                   MOVE SPACES TO TDESC10I OF COTRN0AI
                   MOVE SPACES TO TAMT010I OF COTRN0AI
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      RETURN-TO-PREV-SCREEN
      *----------------------------------------------------------------*
       RETURN-TO-PREV-SCREEN.

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-TRANID    TO CDEMO-FROM-TRANID
           MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
           MOVE ZEROS        TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.


      *----------------------------------------------------------------*
      *                      SEND-TRNLST-SCREEN
      *----------------------------------------------------------------*
       SEND-TRNLST-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF COTRN0AO

           IF SEND-ERASE-YES
               EXEC CICS SEND
                         MAP('COTRN0A')
                         MAPSET('COTRN00')
                         FROM(COTRN0AO)
                         ERASE
                         CURSOR
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP('COTRN0A')
                         MAPSET('COTRN00')
                         FROM(COTRN0AO)
      *                  ERASE
                         CURSOR
               END-EXEC
           END-IF.

      *----------------------------------------------------------------*
      *                      RECEIVE-TRNLST-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-TRNLST-SCREEN.

           EXEC CICS RECEIVE
                     MAP('COTRN0A')
                     MAPSET('COTRN00')
                     INTO(COTRN0AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF COTRN0AO
           MOVE CCDA-TITLE02           TO TITLE02O OF COTRN0AO
           MOVE WS-TRANID              TO TRNNAMEO OF COTRN0AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF COTRN0AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF COTRN0AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF COTRN0AO.

      *----------------------------------------------------------------*
      *                      STARTBR-TRANSACT-FILE
      *----------------------------------------------------------------*
       STARTBR-TRANSACT-FILE.

           EXEC CICS STARTBR
                DATASET   (WS-TRANSACT-FILE)
                RIDFLD    (TRAN-ID)
                KEYLENGTH (LENGTH OF TRAN-ID)
      *         GTEQ
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   CONTINUE
                   SET TRANSACT-EOF TO TRUE
                   MOVE 'You are at the top of the page...' TO
                                   WS-MESSAGE
                   MOVE -1       TO TRNIDINL OF COTRN0AI
                   PERFORM SEND-TRNLST-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup transaction...' TO
                                   WS-MESSAGE
                   MOVE -1       TO TRNIDINL OF COTRN0AI
                   PERFORM SEND-TRNLST-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READNEXT-TRANSACT-FILE
      *----------------------------------------------------------------*
       READNEXT-TRANSACT-FILE.

           EXEC CICS READNEXT
                DATASET   (WS-TRANSACT-FILE)
                INTO      (TRAN-RECORD)
                LENGTH    (LENGTH OF TRAN-RECORD)
                RIDFLD    (TRAN-ID)
                KEYLENGTH (LENGTH OF TRAN-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(ENDFILE)
                   CONTINUE
                   SET TRANSACT-EOF TO TRUE
                   MOVE 'You have reached the bottom of the page...' TO
                                   WS-MESSAGE
                   MOVE -1       TO TRNIDINL OF COTRN0AI
                   PERFORM SEND-TRNLST-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup transaction...' TO
                                   WS-MESSAGE
                   MOVE -1       TO TRNIDINL OF COTRN0AI
                   PERFORM SEND-TRNLST-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      READPREV-TRANSACT-FILE
      *----------------------------------------------------------------*
       READPREV-TRANSACT-FILE.

           EXEC CICS READPREV
                DATASET   (WS-TRANSACT-FILE)
                INTO      (TRAN-RECORD)
                LENGTH    (LENGTH OF TRAN-RECORD)
                RIDFLD    (TRAN-ID)
                KEYLENGTH (LENGTH OF TRAN-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(ENDFILE)
                   CONTINUE
                   SET TRANSACT-EOF TO TRUE
                   MOVE 'You have reached the top of the page...' TO
                                   WS-MESSAGE
                   MOVE -1       TO TRNIDINL OF COTRN0AI
                   PERFORM SEND-TRNLST-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to lookup transaction...' TO
                                   WS-MESSAGE
                   MOVE -1       TO TRNIDINL OF COTRN0AI
                   PERFORM SEND-TRNLST-SCREEN
           END-EVALUATE.

      *----------------------------------------------------------------*
      *                      ENDBR-TRANSACT-FILE
      *----------------------------------------------------------------*
       ENDBR-TRANSACT-FILE.

           EXEC CICS ENDBR
                DATASET   (WS-TRANSACT-FILE)
           END-EXEC.
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:34 CDT
      *
