       CBL APOST
      *----------------------------------------------------------------*
      *                                                                *
      * ENTRY POINT = MQPUT                                            *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQPUT.
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

      * MQPUT and Response
       01 PUT-REQUEST.
           COPY MQ001Q01 SUPPRESS.
       01 PUT-RESPONSE.
           10 FILLER        PIC X(1).
      * Structure with the API information
       01 PUT-INFO-OPER1.
           COPY MQ001I01 SUPPRESS.

      * Request and Response segment, used to store request and
      * response content.
       01 BAQ-REQUEST-PTR             USAGE POINTER.
       01 BAQ-REQUEST-LEN             PIC S9(9) COMP-5 SYNC.
       01 BAQ-RESPONSE-PTR            USAGE POINTER.
       01 BAQ-RESPONSE-LEN            PIC S9(9) COMP-5 SYNC.
       77 COMM-STUB-PGM-NAME          PIC X(8) VALUE 'BAQCSTUB'.

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
       MAINLINE SECTION.

      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE PUT-REQUEST.

      *---------------------------------------------------------------*
      * Set up the data for the API Requester call                    *
      *---------------------------------------------------------------*

           MOVE 1 to MQMESSAGE2-num.
           MOVE "837367" TO numb2 IN PUT-REQUEST.
           MOVE LENGTH of numb2 in PUT-REQUEST to
               numb2-length IN PUT-REQUEST.

           MOVE "John" TO name2 IN PUT-REQUEST.
           MOVE LENGTH of name2 in PUT-REQUEST to
               name2-length IN PUT-REQUEST.

           MOVE "Apex" TO addrx2 IN PUT-REQUEST.
           MOVE LENGTH of addrx2 in PUT-REQUEST to
               addrx2-length IN PUT-REQUEST.

           MOVE "0065" TO phone2 IN PUT-REQUEST.
           MOVE LENGTH of phone2 in PUT-REQUEST to
               phone2-length IN PUT-REQUEST.

           MOVE "11 22 65" TO datex2 IN PUT-REQUEST.
           MOVE LENGTH of datex2 in PUT-REQUEST to
               datex2-length IN PUT-REQUEST.

           MOVE "$1000.65" TO amount2 IN PUT-REQUEST.
           MOVE LENGTH of amount2 in PUT-REQUEST to
               amount2-length IN PUT-REQUEST.

      *---------------------------------------------------------------*
      * Initialize API Requester PTRs & LENs                          *
      *---------------------------------------------------------------*
      * Use pointer and length to specify the location of
      *  request and response segment.
      * This procedure is general and necessary.
           SET BAQ-REQUEST-PTR TO ADDRESS OF PUT-REQUEST.
           MOVE LENGTH OF PUT-REQUEST TO BAQ-REQUEST-LEN.
           SET BAQ-RESPONSE-PTR TO ADDRESS OF PUT-RESPONSE.
           MOVE LENGTH OF PUT-RESPONSE TO BAQ-RESPONSE-LEN.

      *---------------------------------------------------------------*
      * Call the communication stub                                   *
      *---------------------------------------------------------------*
      * Call the subsystem-supplied stub code to send
      * API request to zCEE
           CALL COMM-STUB-PGM-NAME USING
                BY REFERENCE   PUT-INFO-OPER1
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
              DISPLAY "HTTP CODE: " BAQ-STATUS-CODE

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
           MOVE  BAQ-STATUS-CODE TO RETURN-CODE.
           GOBACK.
