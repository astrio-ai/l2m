       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATSFILEA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Filea-VSAM ASSIGN TO FILEA
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS numb of FileA-record
              FILE STATUS IS STATUS-CODE
                          EXTENDED-STATUS-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  Filea-VSAM.
       01  FileA-record.
               05  stat         PIC X.
               05  numb         PIC X(6).
               05  name         PIC X(20).
               05  addrx        PIC X(20).
               05  phone        PIC X(8).
               05  datex        PIC X(8).
               05  amount       PIC X(8).
               05  comment      PIC X(9).
       WORKING-STORAGE SECTION.
       COPY FILEAREQ.
       COPY FILEARSP.
       01  STATUS-CODE            PIC X(2).
           88 NORMAL              VALUE '00'.
           88 DUPLICATE           VALUE '22'.
           88 NOTFOUND            VALUE '23'.

       01  EXTENDED-STATUS-CODE.
           05 EXTENDED-RETURN-CODE   PIC S9(4) COMP.
           05 EXTENDED-FUNCTION-CODE PIC S9(4) COMP.
           05 EXTENDED-FEEDBACK-CODE PIC S9(4) COMP.
      *---------------------------------------------------------------
      *   DATA-NAME                    DATA-TYPE
      *---------------------------------------------------------------
      *
      * REGISTRATION VARIABLES
      *
       01 REG-VARIABLES.
          05 REG-DAEMONGRP             PIC X(8) VALUE LOW-VALUES.
          05 REG-NODE                  PIC X(8).
          05 REG-SVRNAME               PIC X(8).
          05 REG-REGNAME               PIC X(12) VALUE SPACES.
          05 REG-MINCONN               PIC 9(8) COMP VALUE 1.
          05 REG-MAXCONN               PIC 9(8) COMP VALUE 10.
          05 REG-FLAGS                 PIC 9(8) COMP VALUE 0.
          05 REG-URG-FLAGS             PIC 9(8) COMP VALUE 0.
      *
      * SERVICE VARIABLES
      *
       01 SVC-VARIABLES.
          05 SVC-SERVICE-NAME          PIC X(255).
          05 SVC-SERVICE-NAME-LENGTH   PIC 9(8) COMP.
          05 SVC-RQST-DATA-ADDR        USAGE POINTER.
          05 SVC-RQST-DATA-LENGTH      PIC 9(8) COMP.
          05 SVC-RESP-DATA-ADDR        USAGE POINTER.
          05 SVC-RESP-DATA-LENGTH      PIC 9(8) COMP.
          05 SVC-CONNECT-HANDLE        PIC X(12).
          05 SVC-WAIT-TIME             PIC 9(8) USAGE BINARY.
      *
      * WOLA APIS RESPONSE VARIABLES
      *
       01 RSP-VARIABLES.
          05 RSP-RC                    PIC 9(8) COMP VALUE 0.
          05 RSP-RSN                   PIC 9(8) COMP VALUE 0.
          05 RSP-RV                    PIC 9(8) COMP VALUE 0.

      *
      * WORKING VARIABLES
      *
       01 HTTP-VERB                    PIC X(01).
       01 STOP-FLAG                    PIC 9(1) COMP VALUE 0.
       01 CLEAR-WITH-LOW               PIC X(255) VALUE LOW-VALUES.

       PROCEDURE DIVISION.
      *********************
       MAIN-CONTROL SECTION.

       INITIALIZATION.
           OPEN I-O Filea-VSAM
           IF NOT NORMAL
             THEN GO TO ERROR-EXIT
           END-IF
      *
      * SET THE VALUES FOR USE WITH WOLA REGISTRATION
      *
           MOVE 'FILEAZCON'                    TO REG-REGNAME.
           MOVE 'ZCEESRVR'                     TO REG-DAEMONGRP.
           MOVE 'ZCEESRVR'                     TO REG-NODE.
           MOVE 'ZCEESRVR'                     TO REG-SVRNAME.
           MOVE 'ATSFILEA'                     TO SVC-SERVICE-NAME.

           INSPECT REG-DAEMONGRP CONVERTING ' ' to LOW-VALUES.
      *
      * Register to a Local Liberty server
      *
           CALL 'BBOA1REG' USING
                 REG-DAEMONGRP,
                 REG-NODE,
                 REG-SVRNAME,
                 REG-REGNAME,
                 REG-MINCONN,
                 REG-MAXCONN,
                 REG-FLAGS,
                 RSP-RC,
                 RSP-RSN.

           IF RSP-RC > 0 THEN
             DISPLAY "ERROR: Call to BBOA1REG failed"
             GO TO Bad-RC
           ELSE
             DISPLAY "======================================"
             DISPLAY " Register Name : " REG-REGNAME
             DISPLAY "======================================"
             DISPLAY " Successfully registered into "
             DISPLAY " " REG-DAEMONGRP " " REG-NODE " " REG-SVRNAME
             DISPLAY "======================================"
           END-IF.

           MOVE LENGTH OF REQUEST-DATA TO SVC-RQST-DATA-LENGTH.
           SET SVC-RQST-DATA-ADDR TO ADDRESS OF REQUEST-DATA.
           INSPECT SVC-SERVICE-NAME CONVERTING ' ' to LOW-VALUES.

           PERFORM UNTIL STOP-FLAG EQUAL 1

             PERFORM Clear-Fields
      *
      * Setup HOST service
      * ==================
      *
             CALL 'BBOA1SRV' USING
                 REG-REGNAME,
                 SVC-SERVICE-NAME,
                 SVC-SERVICE-NAME-LENGTH,
                 SVC-RQST-DATA-ADDR,
                 SVC-RQST-DATA-LENGTH,
                 SVC-CONNECT-HANDLE,
                 SVC-WAIT-TIME,
                 RSP-RC,
                 RSP-RSN,
                 RSP-RV

             DISPLAY " "
             DISPLAY " Service Name        : " SVC-SERVICE-NAME
             DISPLAY " Data length         : " SVC-RQST-DATA-LENGTH
             DISPLAY " Return value length : " RSP-RV
             DISPLAY " "

             IF RSP-RC > 0 THEN
               DISPLAY "ERROR: Call to BBOA1SRV failed"
               GO TO Bad-RC
             END-IF
      *
      * Setup the response for the requested service
      * ============================================
      *
             DISPLAY "Service request processed"
             MOVE REQUEST-TYPE TO HTTP-VERB
             MOVE CORRESPONDING REQUEST-DATA to FileA-record

             EVALUATE HTTP-VERB
               WHEN 'U'
                 DISPLAY 'PUT - FileA-record: ' FileA-record
                 READ  Filea-VSAM KEY IS numb of FileA-record
                 IF NOTFOUND THEN
                    MOVE 'PUT unsuccessful' to results-message
                 ELSE
                    MOVE CORRESPONDING REQUEST-DATA to FileA-record
                    REWRITE FileA-record
                    MOVE 'PUT successful' to results-message
                    DISPLAY 'REWRITE FileA-record'
                 END-IF
                 MOVE STATUS-CODE TO vsam-status-code
                 DISPLAY 'STATUS CODE = ' STATUS-CODE

               WHEN 'G'
                 DISPLAY 'GET - FileA-record: ' FileA-record
                 READ  Filea-VSAM KEY IS numb of FileA-record
                 MOVE 'GET successful' to results-message
                 DISPLAY 'GET - FileA-record: ' FileA-record
                 MOVE STATUS-CODE TO vsam-status-code
                 DISPLAY 'STATUS CODE = ' STATUS-CODE

               WHEN 'P'
                 DISPLAY 'POST - FileA-record: ' FileA-record
                 WRITE FileA-record
                 MOVE 'POST successful' to results-message
                 DISPLAY 'WRITE FileA-record'
                 MOVE STATUS-CODE TO vsam-status-code
                 DISPLAY 'STATUS CODE = ' STATUS-CODE

               WHEN 'D'
                 DISPLAY 'DELETE - FileA-record: ' FileA-record
                 DELETE Filea-VSAM
                 MOVE 'DELETE successful' to results-message
                 DISPLAY 'DELETE FileA-record'
                 MOVE STATUS-CODE TO vsam-status-code
                 DISPLAY 'STATUS CODE = ' STATUS-CODE

               WHEN OTHER
                 DISPLAY "-> Unknown action was specified"
                 DISPLAY "   Program will terminate ..."
                 MOVE 1 TO STOP-FLAG

             END-EVALUATE

            MOVE CORRESPONDING FileA-record to RESPONSE-DATA

            IF DUPLICATE THEN
               MOVE 'Duplicate record' to results-message
               END-IF
            IF NOTFOUND THEN
               MOVE 'No record found' to results-message
               END-IF
            DISPLAY results-message

            DISPLAY RESPONSE-DATA
            MOVE LENGTH OF RESPONSE-DATA TO SVC-RESP-DATA-LENGTH
            SET SVC-RESP-DATA-ADDR TO ADDRESS OF RESPONSE-DATA

      *
      *  Send response to the service request
      *  ====================================
      *
             CALL 'BBOA1SRP' USING
                 SVC-CONNECT-HANDLE,
                 SVC-RESP-DATA-ADDR,
                 SVC-RESP-DATA-LENGTH,
                 RSP-RC,
                 RSP-RSN

             IF RSP-RC > 0 THEN
               DISPLAY "ERROR: Call to BBOA1RP failed"
               GO TO Bad-RC
             END-IF
      *
      *  Release WOLA connect
      *  ====================
      *
             CALL 'BBOA1CNR' USING
                   SVC-CONNECT-HANDLE,
                   RSP-RC,
                   RSP-RSN

             IF RSP-RC > 0 THEN
               DISPLAY "ERROR: Call to BBOA1CNR failed"
               GO TO Bad-RC
             END-IF

            END-PERFORM.

            CLOSE Filea-VSAM.
      *
      *  Unregister service
      *  ==================
      *
           CALL 'BBOA1URG' USING
               REG-REGNAME,
               REG-URG-FLAGS,
               RSP-RC,
               RSP-RSN

           IF RSP-RC > 0 THEN
             DISPLAY "ERROR: Call to BBOA1URG failed"
             GO TO Bad-RC
           ELSE
             DISPLAY " "
             DISPLAY " Successfully unregistered from "
             DISPLAY " " REG-DAEMONGRP " " REG-NODE " " REG-SVRNAME
             DISPLAY " "
           END-IF.

           GOBACK.
      *
      *  Clear the fields
      *  ========================================
      *
       Clear-Fields.
           MOVE CLEAR-WITH-LOW TO REQUEST-DATA.
           MOVE CLEAR-WITH-LOW TO RESPONSE-DATA.
      *
      *  Section used to exit batch if any WOLA API returned RC>0
      *  ===================================================
      *
       Bad-RC.
           DISPLAY "                          "
           DISPLAY " Return Code = " RSP-RC
           DISPLAY " Reason Code = " RSP-RSN
           DISPLAY "                          "
           DISPLAY " Program ended with Error "
           GOBACK.
       ERROR-EXIT.
           DISPLAY "                              "
           DISPLAY " VSAM status code:            " STATUS-CODE
           DISPLAY " VSAM extended return code:   " EXTENDED-RETURN-CODE
           DISPLAY " VSAM extended function code: "
                                   EXTENDED-FUNCTION-CODE
           DISPLAY " VSAM extended feedback code: "
                                   EXTENDED-FEEDBACK-CODE
           DISPLAY "                          "
           DISPLAY " Program ended with Error "
           GOBACK.
