       CBL APOST
      *----------------------------------------------------------------*
      *                                                                *
      * ENTRY POINT = CSCVINCI                                         *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSCVINCI.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * DL/I FUNCTION CODES
       77  GET-UNIQUE      PIC  X(4)  VALUE 'GU  '.
       77  ISRT            PIC  X(4)  VALUE 'ISRT'.
      * DL/I CALL STATUS CODE
       77  NORMAL          PIC  X(2)  VALUE '  '.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Error Message structure
       01  ERROR-MSG.
           03 EM-ORIGIN                PIC X(8)  VALUE SPACES.
           03 EM-CODE                  PIC S9(9) COMP-5 SYNC VALUE 0.
           03 EM-DETAIL                PIC X(1024) VALUE SPACES.
           03 TIMESTAMP                 PIC 9(14).
       01 IN-BUFFER.
          03 IN-LL                     PIC S9(3) COMP.
          03 IN-ZZ                     PIC S9(3) COMP.
          03 IN-TRCD                   PIC X(10).
          03 IN-COMMAREA.
             05 NUMB                   PIC X(06).
             05 NAME                   PIC X(20).
             05 ADDRX                  PIC X(20).
             05 PHONE                  PIC X(08).
             05 DATEX                  PIC X(08).
             05 AMOUNT                 PIC X(08).
       01 OUT-BUFFER.
         03 OUT-LL                    PIC S9(3) COMP VALUE 400.
         03 OUT-ZZ                    PIC S9(3) COMP VALUE 0.
         03 OUT-COMMAREA.
            05 NUMB                   PIC X(06).
            05 NAME                   PIC X(20).
            05 ADDRX                  PIC X(20).
            05 PHONE                  PIC X(08).
            05 DATEX                  PIC X(08).
            05 AMOUNT                 PIC X(08).
            05 USERID                 PIC X(08).
            05 HTTPCODE               PIC 9(10) USAGE DISPLAY.
            05 MSG1                   PIC X(75).
            05 MSG2                   PIC X(75).
            05 MSG3                   PIC X(75).
            05 MSG4                   PIC X(75).
         03 OUT-SEGNO                 PIC S9(4).

      * COPY API REQUESTER REQUIRED COPYBOOK
       COPY BAQRINFO SUPPRESS.

      * GET Request and Response
       01 GET-REQUEST.
           COPY IMS00Q01 SUPPRESS.
       01 GET-RESPONSE.
           COPY IMS00P01 SUPPRESS.
      * Structure with the API information
       01 GET-INFO-OPER1.
           COPY IMS00I01 SUPPRESS.

      * Request and Response segment, used to store request and
      * response content.
       01 BAQ-REQUEST-PTR             USAGE POINTER.
       01 BAQ-REQUEST-LEN             PIC S9(9) COMP-5 SYNC.
       01 BAQ-RESPONSE-PTR            USAGE POINTER.
       01 BAQ-RESPONSE-LEN            PIC S9(9) COMP-5 SYNC.
       01 SEGNO                       PIC 9(4) VALUE ZERO.
       77 COMM-STUB-PGM-NAME          PIC X(8) VALUE 'BAQCSTUB'.

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01  IOPCB.
           02  LTERM-NAME      PIC  X(8).
           02  IO-RESERVE-IMS  PIC  X(2).
           02  IO-STATUS       PIC  X(2).
           02  CURR-DATE       PIC  X(4).
           02  CURR-TIME       PIC  X(4).
           02  IN-MSN          PIC  X(4).
           02  MODNAME         PIC  X(8).
           02  USERID          PIC  X(8).
       01  ALTPCB.
           02  DBD-NAME        PIC  X(8).
           02  SEG-LEVEL       PIC  X(2).
           02  ALT-STATUS      PIC  X(2).
      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
       PROCEDURE DIVISION USING IOPCB, ALTPCB.
      *----------------------------------------------------------------*
       MAINLINE SECTION.
           CALL 'CBLTDLI' USING GET-UNIQUE, IOPCB, IN-BUFFER.
           MOVE FUNCTION CURRENT-DATE(1:14) TO TIMESTAMP.
           DISPLAY TIMESTAMP ' IOPCB ModNAME: ' MODNAME.
           DISPLAY TIMESTAMP ' IOPCB Userid: '  USERID OF IOPCB.
           DISPLAY TIMESTAMP ' IN-LL: '  IN-LL OF IN-BUFFER.
           DISPLAY TIMESTAMP ' IN-BUFFER: ' IN-BUFFER.
      *----------------------------------------------------------------*
      * Common code                                                    *
      *----------------------------------------------------------------*
      * initialize working storage variables
           INITIALIZE GET-REQUEST.
           INITIALIZE GET-RESPONSE.
      *---------------------------------------------------------------*
      * Set up the data for the API Requester call                    *
      *---------------------------------------------------------------*
           MOVE NUMB of IN-BUFFER TO employee IN GET-REQUEST.
           MOVE LENGTH of employee in GET-REQUEST to
               employee-length IN GET-REQUEST.
           MOVE NUMB of IN-BUFFER to NUMB of OUT-BUFFER.
      *---------------------------------------------------------------*
      * Initialize API Requester PTRs & LENs                          *
      *---------------------------------------------------------------*
      * Use pointer and length to specify the location of
      *  request and response segment.
      * This procedure is general and necessary.
           SET BAQ-REQUEST-PTR TO ADDRESS OF GET-REQUEST.
           MOVE LENGTH OF GET-REQUEST TO BAQ-REQUEST-LEN.
           SET BAQ-RESPONSE-PTR TO ADDRESS OF GET-RESPONSE.
           MOVE LENGTH OF GET-RESPONSE TO BAQ-RESPONSE-LEN.
      *---------------------------------------------------------------*
      * Call the communication stub                                   *
      *---------------------------------------------------------------*
      * Call the subsystem-supplied stub code to send
      * API request to zCEE
           CALL COMM-STUB-PGM-NAME USING
                BY REFERENCE   GET-INFO-OPER1
                BY REFERENCE   BAQ-REQUEST-INFO
                BY REFERENCE   BAQ-REQUEST-PTR
                BY REFERENCE   BAQ-REQUEST-LEN
                BY REFERENCE   BAQ-RESPONSE-INFO
                BY REFERENCE   BAQ-RESPONSE-PTR
                BY REFERENCE   BAQ-RESPONSE-LEN.
           IF MODNAME IS EQUAL TO 'CSCVGET'
              MOVE 'CSCOGET' TO MODNAME
      * The BAQ-RETURN-CODE field in 'BAQRINFO' indicates whether this
      * API call is successful.
      * When BAQ-RETURN-CODE is 'BAQ-SUCCESS', response is
      * successfully returned and fields in RESPONSE copybook
      * can be obtained. Display the translation result.
           IF BAQ-SUCCESS THEN
              MOVE name2        of GET-RESPONSE TO NAME  of OUT-BUFFER
              MOVE Xaddress2    of GET-RESPONSE TO ADDRX  of OUT-BUFFER
              MOVE phoneNumber2 of GET-RESPONSE TO PHONE  of OUT-BUFFER
              MOVE Xdate2       of GET-RESPONSE TO DATEX  of OUT-BUFFER
              MOVE amount2      of GET-RESPONSE TO AMOUNT of OUT-BUFFER
              MOVE USERID2      of GET-RESPONSE TO USERID of OUT-BUFFER
      * Otherwise, some error happened in API, z/OS Connect EE server
      * or communication stub. 'BAQ-STATUS-CODE' and
      * 'BAQ-STATUS-MESSAGE' contain the detailed information
      *  of this error. Note that GET-RESPONSE is not populated.
           ELSE
              DISPLAY TIMESTAMP " Error code: " BAQ-STATUS-CODE
              DISPLAY TIMESTAMP " Error msg: " BAQ-STATUS-MESSAGE
              MOVE BAQ-STATUS-MESSAGE(1:75) TO MSG1 OF OUT-BUFFER
              MOVE BAQ-STATUS-MESSAGE(76:150) TO MSG2 OF OUT-BUFFER
              MOVE BAQ-STATUS-MESSAGE(151:225) TO MSG3 OF OUT-BUFFER
              MOVE BAQ-STATUS-MESSAGE(226:300) TO MSG4 OF OUT-BUFFER
              MOVE BAQ-STATUS-CODE TO EM-CODE
              MOVE BAQ-STATUS-MESSAGE TO EM-DETAIL
              EVALUATE TRUE
      * When error happens in API, BAQ-RETURN-CODE is BAQ-ERROR-IN-API.
      * BAQ-STATUS-CODE is the HTTP response code of API.
                 WHEN BAQ-ERROR-IN-API
                   MOVE 'API' TO EM-ORIGIN
      * When error happens in server, BAQ-RETURN-CODE is
      * BAQ-ERROR-IN-ZCEE. BAQ-STATUS-CODE is the HTTP response code
      * of z/OS Connect EE server.
                 WHEN BAQ-ERROR-IN-ZCEE
                   MOVE 'ZCEE' TO EM-ORIGIN
      * When error happens in communication stub, BAQ-RETURN-CODE is
      * BAQ-ERROR-IN-STUB, BAQ-STATUS-CODE is the error code of STUB.
                 WHEN BAQ-ERROR-IN-STUB
                   MOVE 'STUB' TO EM-ORIGIN
              END-EVALUATE
              DISPLAY TIMESTAMP " Error origin: " EM-ORIGIN
           END-IF
           DISPLAY TIMESTAMP " HTTP CODE: " BAQ-STATUS-CODE
           MOVE BAQ-STATUS-CODE TO HTTPCODE OF OUT-BUFFER
           ADD  +1 TO SEGNO
           MOVE SEGNO TO OUT-SEGNO
           CALL 'CBLTDLI' USING ISRT, IOPCB, OUT-BUFFER, MODNAME.
       MAINLINE-EXIT.
           GOBACK.
           EXIT.
