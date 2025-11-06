       CBL CICS('COBOL3') APOST
      ******************************************************************
      *                                                                *
      * MODULE NAME = DFH0XVDS                                         *
      *                                                                *
      * DESCRIPTIVE NAME = CICS TS  (Samples) Example Application -    *
      *                                       VSAM Data Store          *
      *                                                                *
      *                                                                *
      *                                                                *
      *      Licensed Materials - Property of IBM                      *
      *                                                                *
      *      "Restricted Materials of IBM"                             *
      *                                                                *
      *      5655-Y04                                                  *
      *                                                                *
      *      (C) Copyright IBM Corp. 2004, 2008"                       *
      *                                                                *
      *                                                                *
      *                                                                *
      *                                                                *
      * STATUS = 7.1.0                                                 *
      *                                                                *
      * TRANSACTION NAME = n/a                                         *
      *                                                                *
      * FUNCTION =                                                     *
      *      This accesses the VSAM file for the example application   *
      *      to perform reads and updates of the catalog               *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      * ENTRY POINT = DFH0XVDS                                         *
      *                                                                *
      *----------------------------------------------------------------*
      *                                                                *
      * CHANGE ACTIVITY :                                              *
      *                                                                *
      *      $MOD(DFH0XVDS),COMP(SAMPLES),PROD(CICS TS ):              *
      *                                                                *
      *   PN= REASON REL YYMMDD HDXXIII : REMARKS                      *
      *  $P0= D13727 640 050217 HDIPCB  : Minor fixes to the web servic*
      *  $P1= D20555 660 080415 HDFFCMS : Typos in Web Service Example *
      *   $D0= I07544 640 040910 HDIPCB  : EXAMPLE - BASE APPLICATION  *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DFH0XVDS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
      * Run time (debug) infomation for this invocation
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'DFH0XVDS------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-CALEN                 PIC S9(4) COMP.

      * Variables for time/date processing
       01  ABS-TIME                    PIC S9(8) COMP VALUE +0.
       01  TIME1                       PIC X(8)  VALUE SPACES.
       01  DATE1                       PIC X(10) VALUE SPACES.

      * Error Message structure
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' EXMPCMAN'.
           03 FILLER                   PIC X(11) VALUE ' REQUESTID='.
           03 EM-REQUEST-ID            PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-DETAIL                PIC X(50) VALUE SPACES.


      * Switches
       01 SWITCHES.
           03 CATALOG-EOF-SW           PIC X(1)  VALUE 'N'.
               88 CATALOG-EOF                    VALUE 'Y'.

      * Work fields
       01 WORKFIELDS.
           03 WS-CURRENT-ITEM-REF      PIC 9(4).
           03 WS-RESPONSE-CODE         PIC S9(8) COMP.
           03 WS-LOOP-COUNTER          PIC S9(2) COMP.
           03 WS-RECORD-COUNT          PIC S9(2) COMP.
           03 WS-RECORD-COUNT-DISPLAY  PIC +9(2) USAGE DISPLAY.
           03 WS-CAT-ITEM.
               05 WS-ITEM-REF          PIC 9(4).
               05 WS-DESCRIPTION       PIC X(40).
               05 WS-DEPARTMENT        PIC 9(3).
               05 WS-COST              PIC ZZZ.99.
               05 WS-IN-STOCK          PIC 9(4).
               05 WS-ON-ORDER          PIC 9(3).
               05 FILLER               PIC X(20).

      * Configuration File Data
       01 WS-CONF-FILE-KEY             PIC X(9) VALUE 'VSAM-NAME'.
       01 WS-CONF-DATA.
           03 FILLER                   PIC X(10).
           03 WS-FILENAME-CONF         PIC X(8).
           03 FILLER                   PIC X(62).

      * Constants
       01 WS-FILENAME                  PIC X(8)  VALUE 'EXMPCAT '.

      * Debug
       01 DEBUG-VARS.
           03 DEBUG-Q                  PIC X(7) VALUE 'DEBUG-Q'.
           03 DEBUG-STRING             PIC X(80).

      *----------------------------------------------------------------*

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY DFH0XCP1.

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
           INITIALIZE WS-HEADER.
           INITIALIZE WORKFIELDS.

      * set up general variable
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.

      *---------------------------------------------------------------*
      * Check commarea and obtain required details                    *
      *---------------------------------------------------------------*
      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-DETAIL
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('EXCA') NODUMP END-EXEC
           END-IF

      * initalize commarea return code to zero
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.

      *----------------------------------------------------------------*
      * Read in VSAM file name from config file
      *----------------------------------------------------------------*
           EXEC CICS READ FILE('EXMPCONF')
                          INTO(WS-CONF-DATA)
                          RIDFLD(WS-CONF-FILE-KEY)
           END-EXEC

           MOVE WS-FILENAME-CONF TO WS-FILENAME

      *----------------------------------------------------------------*
      * Check which operation in being requested
      *----------------------------------------------------------------*
      * Uppercase the value passed in the Request Id field
           MOVE FUNCTION UPPER-CASE(CA-REQUEST-ID) TO CA-REQUEST-ID

           EVALUATE CA-REQUEST-ID
               WHEN '01INQC'
      *        Call routine to read catalog for inquire
                   PERFORM CATALOG-INQUIRE

               WHEN '01INQS'
      *        Call routine to perform for inquire for single item
                   PERFORM CATALOG-INQUIRE-SINGLE

               WHEN '01ORDR'
      *        Call routine to place order
                   PERFORM PLACE-ORDER

               WHEN OTHER
      *        Request is not recognised or supported
                   PERFORM REQUEST-NOT-RECOGNISED

           END-EVALUATE

      * Return to caller
           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.
      *----------------------------------------------------------------*

      *================================================================*
      * Procedure to write error message to TD QUEUE(CSMT)             *
      *   message will include Date, Time, Program Name,               *
      *   and error details.                                           *
      *================================================================*
       WRITE-ERROR-MESSAGE.
      * Obtain and format current time and date
           EXEC CICS ASKTIME ABSTIME(ABS-TIME)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)
                     MMDDYYYY(DATE1)
                     TIME(TIME1)
           END-EXEC
           MOVE DATE1 TO EM-DATE
           MOVE TIME1 TO EM-TIME
      * Write output message to TDQ
           EXEC CICS WRITEQ TD QUEUE('CSMT')
                     FROM(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
           END-EXEC.
           EXIT.
      *================================================================*
      * Procedure to link to Datastore program to inquire              *
      *   on the catalog data                                          *
      *================================================================*
        CATALOG-INQUIRE.


           INITIALIZE CA-INQUIRY-RESPONSE-DATA
      * Following 6 lines added by PM46914                    @PM46914A
           PERFORM
               WITH TEST AFTER
               VARYING  WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER EQUAL 15
              INITIALIZE CA-CAT-ITEM(WS-LOOP-COUNTER)
           END-PERFORM

           MOVE 'EXDSVSAM: CATALOG-INQUIRE' TO CA-RESPONSE-MESSAGE

           MOVE CA-LIST-START-REF TO WS-CURRENT-ITEM-REF

      * Start browse of file
           EXEC CICS STARTBR FILE(WS-FILENAME)
                             RIDFLD(WS-CURRENT-ITEM-REF)
                             RESP(WS-RESPONSE-CODE)
           END-EXEC

           IF WS-RESPONSE-CODE EQUAL DFHRESP(NOTFND)
      *    Item not found
               MOVE 20 TO CA-RETURN-CODE
               MOVE 'ITEM NOT FOUND' TO CA-RESPONSE-MESSAGE
               EXEC CICS RETURN END-EXEC
           ELSE
               IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
                   MOVE 21 TO CA-RETURN-CODE
                   STRING  'ERROR OPENING FILE ' WS-FILENAME
                                               DELIMITED BY SIZE
                           INTO CA-RESPONSE-MESSAGE
                   END-STRING
                   EXEC CICS RETURN END-EXEC
               END-IF
           END-IF


      * Loop thru file read in records until EOF or 15 records read
           PERFORM
               WITH TEST AFTER
               VARYING  WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL CATALOG-EOF
                  OR WS-LOOP-COUNTER EQUAL 15

               EXEC CICS READNEXT FILE(WS-FILENAME)
                                  INTO(WS-CAT-ITEM)
                                  RIDFLD(WS-CURRENT-ITEM-REF)
                                  LENGTH(LENGTH OF WS-CAT-ITEM)
                                  RESP(WS-RESPONSE-CODE)
               END-EXEC

               EVALUATE WS-RESPONSE-CODE
                   WHEN DFHRESP(NORMAL)
                       MOVE WS-LOOP-COUNTER TO WS-RECORD-COUNT

                       MOVE WS-CAT-ITEM TO CA-CAT-ITEM(WS-LOOP-COUNTER)

                       MOVE WS-RECORD-COUNT TO CA-ITEM-COUNT
                       MOVE WS-CURRENT-ITEM-REF TO CA-LAST-ITEM-REF

                   WHEN DFHRESP(ENDFILE)
                       MOVE 'Y' TO CATALOG-EOF-SW
                   WHEN OTHER
                       MOVE 21 TO CA-RETURN-CODE
                       MOVE 'ERROR OCCURED READING FILE'
                         TO CA-RESPONSE-MESSAGE
                       EXEC CICS RETURN END-EXEC
               END-EVALUATE
           END-PERFORM

           MOVE SPACES TO CA-RESPONSE-MESSAGE
           MOVE WS-RECORD-COUNT TO WS-RECORD-COUNT-DISPLAY
           STRING WS-RECORD-COUNT-DISPLAY
                  ' ITEMS RETURNED'
                       DELIMITED BY SIZE
               INTO CA-RESPONSE-MESSAGE
           END-STRING

      * End browse of file
           EXEC CICS ENDBR FILE(WS-FILENAME)
                           RESP(WS-RESPONSE-CODE)
           END-EXEC
           IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
               MOVE 21 TO CA-RETURN-CODE
               MOVE 'ERROR ENDING BROWSE SESSION' TO CA-RESPONSE-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-IF
           EXIT.
        CATALOG-INQUIRE-END.
           EXIT.
      *================================================================*
      * Procedure to link to Datastore program to inquire for a single *
      *   item from the catalog data                                   *
      *================================================================*
        CATALOG-INQUIRE-SINGLE.

           EXEC CICS READ FILE(WS-FILENAME)
                          INTO(WS-CAT-ITEM)
                          RIDFLD(CA-ITEM-REF-REQ)
                          RESP(WS-RESPONSE-CODE)
           END-EXEC

           IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
      *    Item not found
               MOVE 20 TO CA-RETURN-CODE
               MOVE 'ITEM NOT FOUND' TO CA-RESPONSE-MESSAGE
               EXEC CICS RETURN END-EXEC
           ELSE
               IF WS-RESPONSE-CODE NOT EQUAL DFHRESP(NORMAL)
                   MOVE 21 TO CA-RETURN-CODE
                   STRING  'ERROR OPENING FILE ' WS-FILENAME
                       DELIMITED BY SIZE
                       INTO CA-RESPONSE-MESSAGE
                   END-STRING
                   EXEC CICS RETURN END-EXEC
               END-IF
           END-IF

      *    Populate commarea to return single item

           MOVE WS-CAT-ITEM TO CA-SINGLE-ITEM

           MOVE SPACES TO CA-RESPONSE-MESSAGE
           STRING 'RETURNED ITEM: REF ='
                  CA-ITEM-REF-REQ
               DELIMITED BY SIZE
               INTO CA-RESPONSE-MESSAGE
           END-STRING


           EXIT.
        CATALOG-INQUIRE-SINGLE-END.
           EXIT.
      *================================================================*
      * Procedure to link to Datastore program to place order,         *
      *   send request to dispatcher and notify stock manager          *
      *   an order has been placed                                     *
      *================================================================*
        PLACE-ORDER.
           MOVE 'PLACE-ORDER' TO CA-RESPONSE-MESSAGE

      * Check validity of order quantity
           IF CA-QUANTITY-REQ IS NOT GREATER THAN 0
               MOVE 98 TO CA-RETURN-CODE
               MOVE 'ORDER QUANTITY MUST BE POSITIVE'
                    TO CA-RESPONSE-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-IF

      * Read file for update
           EXEC CICS READ FILE(WS-FILENAME)
                          UPDATE
                          INTO(WS-CAT-ITEM)
                          RIDFLD(CA-ITEM-REF-NUMBER)
                          RESP(WS-RESPONSE-CODE)
           END-EXEC

           EVALUATE WS-RESPONSE-CODE
      *        Normal Response
               WHEN DFHRESP(NORMAL)
                   PERFORM UPDATE-FILE
      *        Error Conditions
               WHEN DFHRESP(NOTFND)
                   MOVE 20 TO CA-RETURN-CODE
                   MOVE SPACES TO CA-RESPONSE-MESSAGE
                   STRING  'ITEM - '
                           CA-ITEM-REF-NUMBER
                           ' NOT FOUND'
                       DELIMITED BY SIZE
                       INTO CA-RESPONSE-MESSAGE
                   END-STRING
                   EXEC CICS RETURN END-EXEC
               WHEN NOT DFHRESP(NORMAL)
                   MOVE 21 TO CA-RETURN-CODE
                   MOVE 'ERROR OCCURED READING FILE'
                        TO CA-RESPONSE-MESSAGE
                   EXEC CICS RETURN END-EXEC
           END-EVALUATE
           EXIT.
        PLACE-ORDER-END.
           EXIT.

        UPDATE-FILE.
      *    Check there is enough stock to satisfy order
           IF CA-QUANTITY-REQ IS GREATER THAN WS-IN-STOCK
               MOVE 97 TO CA-RETURN-CODE
               MOVE 'INSUFFICENT STOCK TO COMPLETE ORDER'
                    TO CA-RESPONSE-MESSAGE
      *        Unlock file
               EXEC CICS UNLOCK file(WS-FILENAME)END-EXEC
               EXEC CICS RETURN END-EXEC
           END-IF
      *    Update quantity on file
           SUBTRACT CA-QUANTITY-REQ FROM WS-IN-STOCK
           EXEC CICS REWRITE FILE(WS-FILENAME)
                             FROM(WS-CAT-ITEM)
                             RESP(WS-RESPONSE-CODE)
           END-EXEC

           EVALUATE WS-RESPONSE-CODE
               WHEN DFHRESP(NORMAL)
      *                                                            $P1C
                   MOVE 'ORDER SUCCESSFULLY PLACED'
                        TO CA-RESPONSE-MESSAGE
               WHEN OTHER
                   MOVE 22 TO CA-RETURN-CODE
                   MOVE 'ERROR UPDATING FILE' TO CA-RESPONSE-MESSAGE
                   EXEC CICS RETURN END-EXEC
           END-EVALUATE
           EXIT.
        UPDATE-FILE-END.
           EXIT.

      *================================================================*
      * Procedure to handle unknown requests                           *
      *================================================================*
        REQUEST-NOT-RECOGNISED.
           MOVE '99' TO CA-RETURN-CODE
           MOVE CA-REQUEST-ID TO EM-REQUEST-ID
           MOVE ' UNKNOWN REQUEST ID RECEIVED - ' TO EM-DETAIL
           MOVE CA-REQUEST-ID TO EM-DETAIL(31:6)
           MOVE 'OPERATION UNKNOWN' TO CA-RESPONSE-MESSAGE
           PERFORM WRITE-ERROR-MESSAGE
           EXIT.
        REQUEST-NOT-RECOGNISED-END.
           EXIT.
