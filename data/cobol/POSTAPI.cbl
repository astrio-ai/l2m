       CBL APOST
      *----------------------------------------------------------------*
      *                                                                *
      * ENTRY POINT = POSTAPI                                          *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POSTAPI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*

      * Error Message structure
       01  ERROR-MSG.
           03 EM-ORIGIN                PIC X(8)  VALUE SPACES.
           03 EM-CODE                  PIC S9(9) COMP-5 SYNC VALUE 0.
           03 EM-DETAIL                PIC X(1024) VALUE SPACES.

      * Copy API requester required copybook
       COPY BAQRINFO SUPPRESS.

      * POSTAPI and Response
       01 POST-REQUEST.
           COPY CSC00Q01 SUPPRESS.
       01 POST-RESPONSE.
           COPY CSC00P01 SUPPRESS.
      * Structure with the API information
       01 POST-INFO-OPER1.
           COPY CSC00I01 SUPPRESS.

      * Request and Response segment, used to store request and
      * response content.
       01 BAQ-REQUEST-PTR             USAGE POINTER.
       01 BAQ-REQUEST-LEN             PIC S9(9) COMP-5 SYNC.
       01 BAQ-RESPONSE-PTR            USAGE POINTER.
       01 BAQ-RESPONSE-LEN            PIC S9(9) COMP-5 SYNC.
       01 EIBRESP                     PIC X(8).
       01 EIBRESP2                    PIC X(8).
       77 COMM-STUB-PGM-NAME          PIC X(8) VALUE 'BAQCSTUB'.

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01   PARM-BUFFER.
            05 PARM-LENGTH   PIC S9(4) COMP.
            05 PARM-DATA.
               10 employeeNumber PIC X(6).
               10 filler     PIC X(250).
      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION using PARM-BUFFER.

      *----------------------------------------------------------------*
       MAINLINE SECTION.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE POST-REQUEST.
           INITIALIZE POST-RESPONSE.

      *---------------------------------------------------------------*
      * Set up the data for the API Requester call                    *
      *---------------------------------------------------------------*
           MOVE 1 to cscvincInsertServiceOp-num in POST-REQUEST
                     request2-num in POST-REQUEST
                     filea2-num in POST-REQUEST
                     employeeNumber-num in POST-REQUEST
                     name-num in POST-REQUEST
                     Xaddress-num in POST-REQUEST
                     phoneNumber-num in POST-REQUEST
                     Xdate-num in POST-REQUEST
                     amount-num in POST-REQUEST.

           MOVE employeeNumber of PARM-DATA TO employeeNumber2
                                IN POST-REQUEST.
           MOVE LENGTH of employeeNumber2 in POST-REQUEST to
               employeeNumber2-length IN POST-REQUEST.

           MOVE "John" TO name2 IN POST-REQUEST.
           MOVE LENGTH of name2 in POST-REQUEST to
               name2-length IN POST-REQUEST.

           MOVE "Apex" TO Xaddress2 IN POST-REQUEST.
           MOVE LENGTH of Xaddress2 in POST-REQUEST to
               Xaddress2-length IN POST-REQUEST.

           MOVE "0065" TO phoneNumber2 IN POST-REQUEST.
           MOVE LENGTH of phoneNumber2 in POST-REQUEST to
               phoneNumber2-length IN POST-REQUEST.

           MOVE "11 22 65" TO Xdate2 IN POST-REQUEST.
           MOVE LENGTH of Xdate2 in POST-REQUEST to
               Xdate2-length IN POST-REQUEST.

           MOVE "$1000.65" TO amount2 IN POST-REQUEST.
           MOVE LENGTH of amount2 in POST-REQUEST to
               amount2-length IN POST-REQUEST.

      *---------------------------------------------------------------*
      * Initialize API Requester PTRs & LENs                          *
      *---------------------------------------------------------------*
      * Use pointer and length to specify the location of
      *  request and response segment.
      * This procedure is general and necessary.
           SET BAQ-REQUEST-PTR TO ADDRESS OF POST-REQUEST.
           MOVE LENGTH OF POST-REQUEST TO BAQ-REQUEST-LEN.
           SET BAQ-RESPONSE-PTR TO ADDRESS OF POST-RESPONSE.
           MOVE LENGTH OF POST-RESPONSE TO BAQ-RESPONSE-LEN.

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

      * When BAQ-RETURN-CODE is 'BAQ-SUCCESS', response is
      * successfully returned and fields in RESPONSE copybook
      * can be obtained. Display the translation result.
           IF BAQ-SUCCESS THEN
              MOVE CEIBRESP of POST-RESPONSE to EIBRESP
              MOVE CEIBRESP2 of POST-RESPONSE to EIBRESP2
              DISPLAY "EIBRESP:    " EIBRESP
              DISPLAY "EIBRESP2:   " EIBRESP2
              DISPLAY "USERID:     " USERID2
              DISPLAY "HTTP CODE:  " BAQ-STATUS-CODE

      * Otherwise, some error happened in API, z/OS Connect EE server
      * or communication stub. 'BAQ-STATUS-CODE' and
      * 'BAQ-STATUS-MESSAGE' contain the detailed information
      *  of this error.
           ELSE
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
              DISPLAY "Error origin:" EM-ORIGIN
           END-IF.

       MAINLINE-EXIT.
           MOVE BAQ-STATUS-CODE to RETURN-CODE.
           GOBACK.
