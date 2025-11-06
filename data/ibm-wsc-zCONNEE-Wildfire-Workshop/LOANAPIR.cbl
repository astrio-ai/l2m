      *****************************************************************
      *    LOANAPIR                                                   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOANAPIR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Error Message structure
       01  ERROR-MSG.
           03 EM-ORIGIN                PIC X(8)  VALUE SPACES.
           03 EM-CODE                  PIC S9(9) COMP-5 SYNC VALUE 0.
           03 EM-DETAIL                PIC X(1024) VALUE SPACES.

      * Copy API requester required copybook
       COPY BAQRINFO SUPPRESS.

      * POSTMINI and Response
       01 POST-REQUEST.
           COPY MIN00Q01 SUPPRESS.
       01 POST-RESPONSE.
           COPY MIN00P01 SUPPRESS.
      * Structure with the API information
       01 POST-INFO-OPER1.
           COPY MIN00I01 SUPPRESS.

      * Request and Response segment, used to store request and
      * response content.
       01 BAQ-REQUEST-PTR             USAGE POINTER.
       01 BAQ-REQUEST-LEN             PIC S9(9) COMP-5 SYNC.
       01 BAQ-RESPONSE-PTR            USAGE POINTER.
       01 BAQ-RESPONSE-LEN            PIC S9(9) COMP-5 SYNC.
       01 WS-LOOP-COUNTER             PIC S9(2) COMP.
       77 COMM-STUB-PGM-NAME         PIC X(8) VALUE 'BAQCSTUB'.
       01 COMMAREA-BUFFER.
           10 name pic X(20).
           10 creditScore pic 9(18).
           10 yearlyIncome pic 9(18).
           10 age pic 9(10).
           10 amount pic  9(18).
           10 approved pic X.
               88 BoolValue value 'T'.
           10 effectDate pic X(8).
           10 yearlyInterestRate pic S9(5).
           10 yearlyRepayment pic 9(18).
           10 uid             pic X(8).
           10 messages-Num pic 9(9).
           10 messages pic X(60) occurs 10 times.
        01 MESSAGE-TO-SEND PIC X(80) VALUE SPACES.

        COPY MINIMAP.
        COPY DFHAID.

        LINKAGE SECTION.

        01  DFHCOMMAREA                   PIC X(725).

        PROCEDURE DIVISION.

      ***********************************************************
      * Main Program loop starts here....                       *
      ***********************************************************

        MAIN-PROCESS SECTION.

      ***********************************************************
      * FIRST TIME PROCESSING                                   *
      ***********************************************************
            IF EIBCALEN = 0
               EXEC CICS SEND MAP('MINIMAP')
                              MAPSET('MINIMAP')
                              MAPONLY
                              FREEKB
                              ERASE
                              END-EXEC

               EXEC CICS RETURN TRANSID('APIR')
                                COMMAREA (COMMAREA-BUFFER)
                                LENGTH(LENGTH of COMMAREA-BUFFER)
                                END-EXEC
            END-IF.

      ***********************************************************
      * END OF FIRST TIME PROCESSING                            *
      ***********************************************************

           MOVE DFHCOMMAREA TO COMMAREA-BUFFER.
      * initialize working storage variables
           INITIALIZE POST-REQUEST.
           INITIALIZE POST-RESPONSE.

               EXEC CICS RECEIVE MAP('MINIMAP')
                           MAPSET('MINIMAP')
                           NOHANDLE
                           END-EXEC
               EVALUATE EIBAID
                   WHEN DFHPF3   PERFORM EXIT-TRANSACTION
                   WHEN DFHPF12  PERFORM EXIT-TRANSACTION
                   WHEN DFHCLEAR CONTINUE
                   WHEN DFHENTER
                     MOVE  NAMEI     to NAME2 in POST-REQUEST
                     MOVE  LENGTH of NAME2 in POST-REQUEST to
                           NAME2-length in POST-REQUEST
                     MOVE  AGEI      to AGE in POST-REQUEST
                     MOVE  CRDSCREI  to CREDITSCORE in POST-REQUEST
                     MOVE  INCOMEI   to YEARLYINCOME in POST-REQUEST
                     MOVE  AMOUNTI   to AMOUNT       in POST-REQUEST
                     MOVE  YRPAYMNTI
                               To YEARLYREPAYMENT in POST-REQUEST
                     MOVE 1 to MINILOAN-COMMAREA2-num IN POST-REQUEST
                     MOVE 1 to NAME-num               IN POST-REQUEST
                     MOVE 1 to CREDITSCORE-num        IN POST-REQUEST
                     MOVE 1 to YEARLYINCOME-num       IN POST-REQUEST
                     MOVE 1 to AGE-num                IN POST-REQUEST
                     MOVE 1 to AMOUNT-num             IN POST-REQUEST
                     MOVE 1 to YEARLYREPAYMENT-num    IN POST-REQUEST
                     PERFORM INVOKE-API
                     IF BAQ-SUCCESS THEN
                         DISPLAY APPROVEDX2 in POST-RESPONSE
                         IF APPROVEDX2 in POST-RESPONSE = 'T'
                           MOVE 'Loan approved' to approvedo
                         ELSE
                            MOVE 'Loan not approved' to approvedo
                         END-IF
                         MOVE UID2         TO UIDO
                         MOVE MESSAGES2(1) TO MSG1O
                         MOVE MESSAGES2(2) TO MSG2O
                         MOVE MESSAGES2(3) TO MSG3O
                         MOVE MESSAGES2(4) TO MSG4O
                         MOVE MESSAGES2(5) TO MSG5O
                         MOVE MESSAGES2(6) TO MSG6O
                         MOVE MESSAGES2(7) TO MSG7O
                         MOVE MESSAGES2(8) TO MSG8O
                         MOVE MESSAGES2(9) TO MSG9O
                     ELSE
                         PERFORM CHECK-API-ERROR
                         MOVE BAQ-STATUS-CODE TO MSG1O
                         MOVE BAQ-STATUS-MESSAGE(1:60) TO MSG2O
                         MOVE BAQ-STATUS-MESSAGE(61:120) TO MSG3O
                         MOVE BAQ-STATUS-MESSAGE(121:180) TO MSG4O
                         MOVE BAQ-STATUS-MESSAGE(181:240) TO MSG5O
                         MOVE BAQ-STATUS-MESSAGE(241:300) TO MSG6O
                         MOVE BAQ-STATUS-MESSAGE(301:360) TO MSG7O
                         MOVE BAQ-STATUS-MESSAGE(361:420) TO MSG8O
                         MOVE BAQ-STATUS-MESSAGE(421:480) TO MSG9O
                         MOVE BAQ-STATUS-MESSAGE(481:540) TO MSGAO
                     END-IF.
            EXEC CICS SEND CONTROL ERASE END-EXEC
            EXEC CICS SEND MAP('MINIMAP')
                 MAPSET('MINIMAP')
                 FREEKB ERASE END-EXEC
            EXEC CICS RETURN TRANSID ('APIR')
                 COMMAREA (COMMAREA-BUFFER) END-EXEC.

         MAIN-PROCESS-EXIT.
            EXEC CICS RETURN END-EXEC.
            EXIT.
      ***********************************************************
      * Main Program loop ENDS here....                         *
      ***********************************************************

        INVOKE-API SECTION.
      *---------------------------------------------------------------*
      * Initialize API Requester PTRs & LENs                          *
      *---------------------------------------------------------------*
      * Use pointer and length to specify the location of
      *  request and response segment.
      * This procedure is general and necessary.
           SET BAQ-REQUEST-PTR TO ADDRESS OF POST-REQUEST
           MOVE LENGTH OF POST-REQUEST TO BAQ-REQUEST-LEN
           SET BAQ-RESPONSE-PTR TO ADDRESS OF POST-RESPONSE
           MOVE LENGTH OF POST-RESPONSE TO BAQ-RESPONSE-LEN

      *---------------------------------------------------------------*
      * Call the communication stub                                   *
      *---------------------------------------------------------------*
      * Call the subsystem-supplied stub code to send
      * API request to zCEE
           CALL COMM-STUB-PGM-NAME USING
                BY REFERENCE   POST-INFO-OPER1
                BY REFERENCE   BAQ-REQUEST-INFO
                BY REFERENCE   BAQ-REQUEST-PTR
                BY REFERENCE   BAQ-REQUEST-LEN
                BY REFERENCE   BAQ-RESPONSE-INFO
                BY REFERENCE   BAQ-RESPONSE-PTR
                BY REFERENCE   BAQ-RESPONSE-LEN.
      * The BAQ-RETURN-CODE field in 'BAQRINFO' indicates whether this
      * API call is successful.

        CHECK-API-ERROR SECTION.
      * Some error happened in API, z/OS Connect EE server
      * or communication stub. 'BAQ-STATUS-CODE' and
      * 'BAQ-STATUS-MESSAGE' contain the detailed information
      *  of this error.
              DISPLAY "Error code: " BAQ-STATUS-CODE
              DISPLAY "Error msg:" BAQ-STATUS-MESSAGE
              MOVE BAQ-STATUS-CODE TO EM-CODE
              MOVE BAQ-STATUS-MESSAGE TO EM-DETAIL
              EVALUATE TRUE
      * When error happens in API, BAQ-RETURN-CODE is BAQ-ERROR-IN-API.
      * BAQ-STATUS-CODE is the HTTP response code of API.
                 WHEN BAQ-ERROR-IN-API
                   MOVE 'API' TO EM-ORIGIN
      * When error happens in server, BAQ-RETURN-CODE is
      * BAQ-ERROR-IN-ZCEE
      * BAQ-STATUS-CODE is the HTTP response code of
      * z/OS Connect EE server.
                 WHEN BAQ-ERROR-IN-ZCEE
                   MOVE 'ZCEE' TO EM-ORIGIN
      * When error happens in communication stub, BAQ-RETURN-CODE is
      * BAQ-ERROR-IN-STUB, BAQ-STATUS-CODE is the error code of STUB.
                 WHEN BAQ-ERROR-IN-STUB
                   MOVE 'STUB' TO EM-ORIGIN
              END-EVALUATE
              DISPLAY "Error origin:" EM-ORIGIN.

        EXIT-TRANSACTION SECTION.
      *********************************************************
      * End of this transaction, put out message and die...
      *********************************************************

            EXEC CICS SEND CONTROL ERASE END-EXEC
            MOVE 'LOANAPIR Session Over' to MESSAGE-TO-SEND.
            EXEC CICS SEND TEXT FROM(MESSAGE-TO-SEND)
                 ERASE
                 FREEKB
            END-EXEC.
            EXEC CICS RETURN END-EXEC.

        EXIT-TRANSACTION-EXIT.
           EXIT.
