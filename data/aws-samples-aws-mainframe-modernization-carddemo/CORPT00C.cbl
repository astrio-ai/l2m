      ******************************************************************        
      * Program     : CORPT00C.CBL
      * Application : CardDemo
      * Type        : CICS COBOL Program
      * Function    : Print Transaction reports by submitting batch 
      *               job from online using extra partition TDQ.  
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
       PROGRAM-ID. CORPT00C.
       AUTHOR.     AWS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
      *----------------------------------------------------------------*
      *                     WORKING STORAGE SECTION
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.

       01 WS-VARIABLES.
         05 WS-PGMNAME                 PIC X(08) VALUE 'CORPT00C'.
         05 WS-TRANID                  PIC X(04) VALUE 'CR00'.
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
         05 WS-END-LOOP                PIC X(01) VALUE 'N'.
           88 END-LOOP-YES                       VALUE 'Y'.
           88 END-LOOP-NO                        VALUE 'N'.

         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
         05 WS-REC-COUNT               PIC S9(04) COMP VALUE ZEROS.
         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.
         05 WS-REPORT-NAME             PIC X(10) VALUE SPACES.

         05 WS-START-DATE.
            10 WS-START-DATE-YYYY      PIC X(04) VALUE SPACES.
            10 FILLER                  PIC X(01) VALUE '-'.
            10 WS-START-DATE-MM        PIC X(02) VALUE SPACES.
            10 FILLER                  PIC X(01) VALUE '-'.
            10 WS-START-DATE-DD        PIC X(02) VALUE SPACES.
         05 WS-END-DATE.
            10 WS-END-DATE-YYYY        PIC X(04) VALUE SPACES.
            10 FILLER                  PIC X(01) VALUE '-'.
            10 WS-END-DATE-MM          PIC X(02) VALUE SPACES.
            10 FILLER                  PIC X(01) VALUE '-'.
            10 WS-END-DATE-DD          PIC X(02) VALUE SPACES.
         05 WS-DATE-FORMAT             PIC X(10) VALUE 'YYYY-MM-DD'.

         05 WS-NUM-99                  PIC 99   VALUE 0.
         05 WS-NUM-9999                PIC 9999 VALUE 0.

         05 WS-TRAN-AMT                PIC +99999999.99.
         05 WS-TRAN-DATE               PIC X(08) VALUE '00/00/00'.
         05 JCL-RECORD                 PIC X(80) VALUE ' '.

       01 JOB-DATA.
        02 JOB-DATA-1.
         05 FILLER                     PIC X(80) VALUE
         "//TRNRPT00 JOB 'TRAN REPORT',CLASS=A,MSGCLASS=0,".
         05 FILLER                     PIC X(80) VALUE
         "// NOTIFY=&SYSUID".
         05 FILLER                     PIC X(80) VALUE
         "//*".
         05 FILLER                     PIC X(80) VALUE
         "//JOBLIB JCLLIB ORDER=('AWS.M2.CARDDEMO.PROC')".
         05 FILLER                     PIC X(80) VALUE
         "//*".
         05 FILLER                     PIC X(80) VALUE
         "//STEP10 EXEC PROC=TRANREPT".
         05 FILLER                     PIC X(80) VALUE
         "//*".
         05 FILLER                     PIC X(80) VALUE
         "//STEP05R.SYMNAMES DD *".
         05 FILLER                     PIC X(80) VALUE
         "TRAN-CARD-NUM,263,16,ZD".
         05 FILLER                     PIC X(80) VALUE
         "TRAN-PROC-DT,305,10,CH".
         05 FILLER-1.
            10 FILLER                  PIC X(18) VALUE
         "PARM-START-DATE,C'".
            10 PARM-START-DATE-1       PIC X(10) VALUE SPACES.
            10 FILLER                  PIC X(52) VALUE "'".
         05 FILLER-2.
            10 FILLER                  PIC X(16) VALUE
         "PARM-END-DATE,C'".
            10 PARM-END-DATE-1         PIC X(10) VALUE SPACES.
            10 FILLER                  PIC X(54) VALUE "'".
         05 FILLER                     PIC X(80) VALUE
         "/*".
         05 FILLER                     PIC X(80) VALUE
         "//STEP10R.DATEPARM DD *".
         05 FILLER-3.
            10 PARM-START-DATE-2       PIC X(10) VALUE SPACES.
            10 FILLER                  PIC X VALUE SPACE.
            10 PARM-END-DATE-2         PIC X(10) VALUE SPACES.
            10 FILLER                  PIC X(59) VALUE SPACES.
         05 FILLER                     PIC X(80) VALUE
         "/*".
         05 FILLER                     PIC X(80) VALUE
         "/*EOF".
        02 JOB-DATA-2 REDEFINES JOB-DATA-1.
         05 JOB-LINES OCCURS 1000 TIMES PIC X(80).

       01 CSUTLDTC-PARM.
          05 CSUTLDTC-DATE                   PIC X(10).
          05 CSUTLDTC-DATE-FORMAT            PIC X(10).
          05 CSUTLDTC-RESULT.
             10 CSUTLDTC-RESULT-SEV-CD       PIC X(04).
             10 FILLER                       PIC X(11).
             10 CSUTLDTC-RESULT-MSG-NUM      PIC X(04).
             10 CSUTLDTC-RESULT-MSG          PIC X(61).

       COPY COCOM01Y.

       COPY CORPT00.

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
           SET SEND-ERASE-YES TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF CORPT0AO

           IF EIBCALEN = 0
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
           ELSE
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
               IF NOT CDEMO-PGM-REENTER
                   SET CDEMO-PGM-REENTER    TO TRUE
                   MOVE LOW-VALUES          TO CORPT0AO
                   MOVE -1       TO MONTHLYL OF CORPT0AI
                   PERFORM SEND-TRNRPT-SCREEN
               ELSE
                   PERFORM RECEIVE-TRNRPT-SCREEN
                   EVALUATE EIBAID
                       WHEN DFHENTER
                           PERFORM PROCESS-ENTER-KEY
                       WHEN DFHPF3
                           MOVE 'COMEN01C' TO CDEMO-TO-PROGRAM
                           PERFORM RETURN-TO-PREV-SCREEN
                       WHEN OTHER
                           MOVE 'Y'                       TO WS-ERR-FLG
                           MOVE -1       TO MONTHLYL OF CORPT0AI
                           MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                           PERFORM SEND-TRNRPT-SCREEN
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

           DISPLAY 'PROCESS ENTER KEY'

           EVALUATE TRUE
               WHEN MONTHLYI OF CORPT0AI NOT = SPACES AND LOW-VALUES
                   MOVE 'Monthly'   TO WS-REPORT-NAME
                   MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

                   MOVE WS-CURDATE-YEAR     TO WS-START-DATE-YYYY
                   MOVE WS-CURDATE-MONTH    TO WS-START-DATE-MM
                   MOVE '01'                TO WS-START-DATE-DD
                   MOVE WS-START-DATE       TO PARM-START-DATE-1
                                               PARM-START-DATE-2

                   MOVE 1              TO WS-CURDATE-DAY
                   ADD 1               TO WS-CURDATE-MONTH
                   IF WS-CURDATE-MONTH > 12
                       ADD 1           TO WS-CURDATE-YEAR
                       MOVE 1          TO WS-CURDATE-MONTH
                   END-IF
                   COMPUTE WS-CURDATE-N = FUNCTION DATE-OF-INTEGER(
                           FUNCTION INTEGER-OF-DATE(WS-CURDATE-N) - 1)

                   MOVE WS-CURDATE-YEAR     TO WS-END-DATE-YYYY
                   MOVE WS-CURDATE-MONTH    TO WS-END-DATE-MM
                   MOVE WS-CURDATE-DAY      TO WS-END-DATE-DD
                   MOVE WS-END-DATE         TO PARM-END-DATE-1
                                               PARM-END-DATE-2

                   PERFORM SUBMIT-JOB-TO-INTRDR
               WHEN YEARLYI OF CORPT0AI NOT = SPACES AND LOW-VALUES
                   MOVE 'Yearly'   TO WS-REPORT-NAME
                   MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

                   MOVE WS-CURDATE-YEAR     TO WS-START-DATE-YYYY
                                               WS-END-DATE-YYYY
                   MOVE '01'                TO WS-START-DATE-MM
                                               WS-START-DATE-DD
                   MOVE WS-START-DATE       TO PARM-START-DATE-1
                                               PARM-START-DATE-2

                   MOVE '12'                TO WS-END-DATE-MM
                   MOVE '31'                TO WS-END-DATE-DD
                   MOVE WS-END-DATE         TO PARM-END-DATE-1
                                               PARM-END-DATE-2

                   PERFORM SUBMIT-JOB-TO-INTRDR
               WHEN CUSTOMI OF CORPT0AI NOT = SPACES AND LOW-VALUES

                   EVALUATE TRUE
                       WHEN SDTMMI OF CORPT0AI = SPACES OR
                                                   LOW-VALUES
                           MOVE 'Start Date - Month can NOT be empty...'
                             TO WS-MESSAGE
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE -1       TO SDTMML OF CORPT0AI
                           PERFORM SEND-TRNRPT-SCREEN
                       WHEN SDTDDI OF CORPT0AI = SPACES OR
                                                   LOW-VALUES
                           MOVE 'Start Date - Day can NOT be empty...'
                             TO WS-MESSAGE
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE -1       TO SDTDDL OF CORPT0AI
                           PERFORM SEND-TRNRPT-SCREEN
                       WHEN SDTYYYYI OF CORPT0AI = SPACES OR
                                                   LOW-VALUES
                           MOVE 'Start Date - Year can NOT be empty...'
                             TO WS-MESSAGE
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE -1       TO SDTYYYYL OF CORPT0AI
                           PERFORM SEND-TRNRPT-SCREEN
                       WHEN EDTMMI OF CORPT0AI = SPACES OR
                                                   LOW-VALUES
                           MOVE 'End Date - Month can NOT be empty...'
                             TO WS-MESSAGE
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE -1       TO EDTMML OF CORPT0AI
                           PERFORM SEND-TRNRPT-SCREEN
                       WHEN EDTDDI OF CORPT0AI = SPACES OR
                                                   LOW-VALUES
                           MOVE 'End Date - Day can NOT be empty...'
                             TO WS-MESSAGE
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE -1       TO EDTDDL OF CORPT0AI
                           PERFORM SEND-TRNRPT-SCREEN
                       WHEN EDTYYYYI OF CORPT0AI = SPACES OR
                                                   LOW-VALUES
                           MOVE 'End Date - Year can NOT be empty...'
                             TO WS-MESSAGE
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE -1       TO EDTYYYYL OF CORPT0AI
                           PERFORM SEND-TRNRPT-SCREEN
                       WHEN OTHER
                           CONTINUE
                   END-EVALUATE

                   COMPUTE WS-NUM-99 = FUNCTION NUMVAL-C
                                         (SDTMMI OF CORPT0AI)
                   MOVE WS-NUM-99      TO SDTMMI OF CORPT0AI

                   COMPUTE WS-NUM-99 = FUNCTION NUMVAL-C
                                         (SDTDDI OF CORPT0AI)
                   MOVE WS-NUM-99      TO SDTDDI OF CORPT0AI

                   COMPUTE WS-NUM-9999 = FUNCTION NUMVAL-C
                                           (SDTYYYYI OF CORPT0AI)
                   MOVE WS-NUM-9999      TO SDTYYYYI OF CORPT0AI

                   COMPUTE WS-NUM-99 = FUNCTION NUMVAL-C
                                         (EDTMMI OF CORPT0AI)
                   MOVE WS-NUM-99      TO EDTMMI OF CORPT0AI

                   COMPUTE WS-NUM-99 = FUNCTION NUMVAL-C
                                         (EDTDDI OF CORPT0AI)
                   MOVE WS-NUM-99      TO EDTDDI OF CORPT0AI

                   COMPUTE WS-NUM-9999 = FUNCTION NUMVAL-C
                                           (EDTYYYYI OF CORPT0AI)
                   MOVE WS-NUM-9999      TO EDTYYYYI OF CORPT0AI

                   IF SDTMMI OF CORPT0AI IS NOT NUMERIC OR
                      SDTMMI OF CORPT0AI > '12'
                       MOVE 'Start Date - Not a valid Month...'
                         TO WS-MESSAGE
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE -1       TO SDTMML OF CORPT0AI
                       PERFORM SEND-TRNRPT-SCREEN
                   END-IF

                   IF SDTDDI OF CORPT0AI IS NOT NUMERIC OR
                      SDTDDI OF CORPT0AI > '31'
                       MOVE 'Start Date - Not a valid Day...'
                         TO WS-MESSAGE
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE -1       TO SDTDDL OF CORPT0AI
                       PERFORM SEND-TRNRPT-SCREEN
                   END-IF

                   IF SDTYYYYI OF CORPT0AI IS NOT NUMERIC
                       MOVE 'Start Date - Not a valid Year...'
                         TO WS-MESSAGE
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE -1       TO SDTYYYYL OF CORPT0AI
                       PERFORM SEND-TRNRPT-SCREEN
                   END-IF

                   IF EDTMMI OF CORPT0AI IS NOT NUMERIC OR
                      EDTMMI OF CORPT0AI > '12'
                       MOVE 'End Date - Not a valid Month...'
                         TO WS-MESSAGE
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE -1       TO EDTMML OF CORPT0AI
                       PERFORM SEND-TRNRPT-SCREEN
                   END-IF

                   IF EDTDDI OF CORPT0AI IS NOT NUMERIC OR
                      EDTDDI OF CORPT0AI > '31'
                       MOVE 'End Date - Not a valid Day...'
                         TO WS-MESSAGE
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE -1       TO EDTDDL OF CORPT0AI
                       PERFORM SEND-TRNRPT-SCREEN
                   END-IF

                   IF EDTYYYYI OF CORPT0AI IS NOT NUMERIC
                       MOVE 'End Date - Not a valid Year...'
                         TO WS-MESSAGE
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE -1       TO EDTYYYYL OF CORPT0AI
                       PERFORM SEND-TRNRPT-SCREEN
                   END-IF

                   MOVE SDTYYYYI OF CORPT0AI TO WS-START-DATE-YYYY
                   MOVE SDTMMI   OF CORPT0AI TO WS-START-DATE-MM
                   MOVE SDTDDI   OF CORPT0AI TO WS-START-DATE-DD
                   MOVE EDTYYYYI OF CORPT0AI TO WS-END-DATE-YYYY
                   MOVE EDTMMI   OF CORPT0AI TO WS-END-DATE-MM
                   MOVE EDTDDI   OF CORPT0AI TO WS-END-DATE-DD

                   MOVE WS-START-DATE        TO CSUTLDTC-DATE
                   MOVE WS-DATE-FORMAT       TO CSUTLDTC-DATE-FORMAT
                   MOVE SPACES               TO CSUTLDTC-RESULT

                   CALL 'CSUTLDTC' USING   CSUTLDTC-DATE
                                           CSUTLDTC-DATE-FORMAT
                                           CSUTLDTC-RESULT

                   IF CSUTLDTC-RESULT-SEV-CD = '0000'
                       CONTINUE
                   ELSE
                       IF CSUTLDTC-RESULT-MSG-NUM NOT = '2513'
                           MOVE 'Start Date - Not a valid date...'
                             TO WS-MESSAGE
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE -1       TO SDTMML OF CORPT0AI
                           PERFORM SEND-TRNRPT-SCREEN
                       END-IF
                   END-IF

                   MOVE WS-END-DATE          TO CSUTLDTC-DATE
                   MOVE WS-DATE-FORMAT       TO CSUTLDTC-DATE-FORMAT
                   MOVE SPACES               TO CSUTLDTC-RESULT

                   CALL 'CSUTLDTC' USING   CSUTLDTC-DATE
                                           CSUTLDTC-DATE-FORMAT
                                           CSUTLDTC-RESULT

                   IF CSUTLDTC-RESULT-SEV-CD = '0000'
                       CONTINUE
                   ELSE
                       IF CSUTLDTC-RESULT-MSG-NUM NOT = '2513'
                           MOVE 'End Date - Not a valid date...'
                             TO WS-MESSAGE
                           MOVE 'Y'     TO WS-ERR-FLG
                           MOVE -1       TO EDTMML OF CORPT0AI
                           PERFORM SEND-TRNRPT-SCREEN
                       END-IF
                   END-IF


                   MOVE WS-START-DATE       TO PARM-START-DATE-1
                                               PARM-START-DATE-2
                   MOVE WS-END-DATE         TO PARM-END-DATE-1
                                               PARM-END-DATE-2
                   MOVE 'Custom'   TO WS-REPORT-NAME
                   IF NOT ERR-FLG-ON
                       PERFORM SUBMIT-JOB-TO-INTRDR
                   END-IF
               WHEN OTHER
                   MOVE 'Select a report type to print report...' TO
                                   WS-MESSAGE
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE -1       TO MONTHLYL OF CORPT0AI
                   PERFORM SEND-TRNRPT-SCREEN
           END-EVALUATE

           IF NOT ERR-FLG-ON

               PERFORM INITIALIZE-ALL-FIELDS
               MOVE DFHGREEN           TO ERRMSGC  OF CORPT0AO
               STRING WS-REPORT-NAME   DELIMITED BY SPACE
                 ' report submitted for printing ...'
                                       DELIMITED BY SIZE
                 INTO WS-MESSAGE
               MOVE -1       TO MONTHLYL OF CORPT0AI
               PERFORM SEND-TRNRPT-SCREEN

           END-IF.


      *----------------------------------------------------------------*
      *                      SUBMIT-JOB-TO-INTRDR
      *----------------------------------------------------------------*
       SUBMIT-JOB-TO-INTRDR.

           IF CONFIRMI OF CORPT0AI = SPACES OR LOW-VALUES
               STRING
                 'Please confirm to print the '
                                   DELIMITED BY SIZE
                 WS-REPORT-NAME    DELIMITED BY SPACE
                 ' report...'      DELIMITED BY SIZE
                 INTO WS-MESSAGE
               MOVE 'Y'     TO WS-ERR-FLG
               MOVE -1       TO CONFIRML OF CORPT0AI
               PERFORM SEND-TRNRPT-SCREEN
           END-IF

           IF NOT ERR-FLG-ON
               EVALUATE TRUE
                   WHEN CONFIRMI OF CORPT0AI = 'Y' OR 'y'
                       CONTINUE
                   WHEN CONFIRMI OF CORPT0AI = 'N' OR 'n'
                       PERFORM INITIALIZE-ALL-FIELDS
                       MOVE 'Y'     TO WS-ERR-FLG
                       PERFORM SEND-TRNRPT-SCREEN
                   WHEN OTHER
                       STRING
                         '"'               DELIMITED BY SIZE
                         CONFIRMI OF CORPT0AI    DELIMITED BY SPACE
                         '" is not a valid value to confirm...'
                                           DELIMITED BY SIZE
                         INTO WS-MESSAGE
                       MOVE 'Y'     TO WS-ERR-FLG
                       MOVE -1       TO CONFIRML OF CORPT0AI
                       PERFORM SEND-TRNRPT-SCREEN
               END-EVALUATE

               SET END-LOOP-NO TO TRUE

               PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 1000 OR
                                      END-LOOP-YES  OR ERR-FLG-ON

                   MOVE JOB-LINES(WS-IDX) TO JCL-RECORD
                   IF JCL-RECORD = '/*EOF' OR
                      JCL-RECORD = SPACES OR LOW-VALUES
                       SET END-LOOP-YES TO TRUE
                   END-IF

                   PERFORM WIRTE-JOBSUB-TDQ
               END-PERFORM

           END-IF.

      *----------------------------------------------------------------*
      *                      WIRTE-JOBSUB-TDQ
      *----------------------------------------------------------------*
       WIRTE-JOBSUB-TDQ.

           EXEC CICS WRITEQ TD
             QUEUE ('JOBS')
             FROM (JCL-RECORD)
             LENGTH (LENGTH OF JCL-RECORD)
             RESP(WS-RESP-CD)
             RESP2(WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to Write TDQ (JOBS)...' TO
                                   WS-MESSAGE
                   MOVE -1       TO MONTHLYL OF CORPT0AI
                   PERFORM SEND-TRNRPT-SCREEN
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
      *                      SEND-TRNRPT-SCREEN
      *----------------------------------------------------------------*
       SEND-TRNRPT-SCREEN.

           PERFORM POPULATE-HEADER-INFO

           MOVE WS-MESSAGE TO ERRMSGO OF CORPT0AO

           IF SEND-ERASE-YES
               EXEC CICS SEND
                         MAP('CORPT0A')
                         MAPSET('CORPT00')
                         FROM(CORPT0AO)
                         ERASE
                         CURSOR
               END-EXEC
           ELSE
               EXEC CICS SEND
                         MAP('CORPT0A')
                         MAPSET('CORPT00')
                         FROM(CORPT0AO)
      *                  ERASE
                         CURSOR
               END-EXEC
           END-IF.

           GO TO RETURN-TO-CICS.

      *----------------------------------------------------------------*
      *                         RETURN-TO-CICS
      *----------------------------------------------------------------*
       RETURN-TO-CICS.

           EXEC CICS RETURN
                     TRANSID (WS-TRANID)
                     COMMAREA (CARDDEMO-COMMAREA)
      *              LENGTH(LENGTH OF CARDDEMO-COMMAREA)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      RECEIVE-TRNRPT-SCREEN
      *----------------------------------------------------------------*
       RECEIVE-TRNRPT-SCREEN.

           EXEC CICS RECEIVE
                     MAP('CORPT0A')
                     MAPSET('CORPT00')
                     INTO(CORPT0AI)
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

      *----------------------------------------------------------------*
      *                      POPULATE-HEADER-INFO
      *----------------------------------------------------------------*
       POPULATE-HEADER-INFO.

           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA

           MOVE CCDA-TITLE01           TO TITLE01O OF CORPT0AO
           MOVE CCDA-TITLE02           TO TITLE02O OF CORPT0AO
           MOVE WS-TRANID              TO TRNNAMEO OF CORPT0AO
           MOVE WS-PGMNAME             TO PGMNAMEO OF CORPT0AO

           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY

           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF CORPT0AO

           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS

           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF CORPT0AO.

      *----------------------------------------------------------------*
      *                      INITIALIZE-ALL-FIELDS
      *----------------------------------------------------------------*
       INITIALIZE-ALL-FIELDS.

           MOVE -1              TO MONTHLYL OF CORPT0AI
           INITIALIZE              MONTHLYI OF CORPT0AI
                                   YEARLYI  OF CORPT0AI
                                   CUSTOMI  OF CORPT0AI
                                   SDTMMI   OF CORPT0AI
                                   SDTDDI   OF CORPT0AI
                                   SDTYYYYI OF CORPT0AI
                                   EDTMMI   OF CORPT0AI
                                   EDTDDI   OF CORPT0AI
                                   EDTYYYYI OF CORPT0AI
                                   CONFIRMI OF CORPT0AI
                                   WS-MESSAGE.
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *
