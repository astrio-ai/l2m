 CBL  APOST

       IDENTIFICATION DIVISION.

       PROGRAM-ID. TRADERBL.
      * Business logic module for Trader ITSO IMS sample

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      * DL/I FUNCTION CODES

       77  GET-UNIQUE      PIC  X(4)  VALUE 'GU  '.
       77  GET-HOLD-UNIQUE PIC  X(4)  VALUE 'GHU '.
       77  GET-NEXT        PIC  X(4)  VALUE 'GN  '.
       77  ISRT            PIC  X(4)  VALUE 'ISRT'.
       77  DLET            PIC  X(4)  VALUE 'DLET'.
       77  REPL            PIC  X(4)  VALUE 'REPL'.

      * DL/I CALL STATUS CODE

       77  NORMAL          PIC  X(2)  VALUE '  '.
       77  NOTFND          PIC  X(2)  VALUE 'GE'.
       77  END-OF-DATABASE PIC  X(2)  VALUE 'GB'.

      * Work field used for blank stripping

       01 WORK-FIELD                   PIC X(50).
       01 SEGNO                        PIC 9(4) VALUE ZERO.

      * SEGMENT SEARCH ARGUMENTS

       01 SSA-COMPANY.
          03 SEGMENT-NAME       PIC X(8) VALUE 'COMPSEG '.
          03 SEG-KEY-NAME       PIC X(11) VALUE '(COMPANY GE'.
          03 SSA-COMPANY-KEY    PIC X(20).
          03 FILLER             PIC X(1) VALUE ')'.

       01 SSA-CUSTOMER.
          03 SEGMENT-NAME       PIC X(8) VALUE 'CUSTSEG '.
          03 SEG-KEY-NAME       PIC X(11) VALUE '(KEYREC  EQ'.
          03 SSA-CUSTOMER-KEY   PIC X(60).
          03 FILLER             PIC X(1) VALUE '.'.
          03 SSA-CUSTOMER-COMP  PIC X(20).
          03 FILLER             PIC X(1) VALUE ')'.

       01 SSA-CUSTOMER-ISRT.
          03 SEGMENT-NAME       PIC X(9) VALUE 'CUSTSEG  '.

      * For Debug tracing (avoids blank stripping etc)

       01 DEBUG-WORDS                  PIC X(67).
       01 MESSAGE-AREAS.
          03 USER-TRACE-MSG.
             05 FILLER  PIC X(25) VALUE 'USER #UUUUUUUUUUUUUU COMP'.
             05 FILLER  PIC X(25) VALUE 'ANY #CCCCCCCCCCCCCCCCCCC '.
          03 COMPANY-NOT-FOUND-MSG.
             05 FILLER  PIC X(25) VALUE 'COMPANY #CCCCCCCCCCCCCCCC'.
             05 FILLER  PIC X(25) VALUE 'CCC NOT FOUND            '.
          03 REQUEST-NOT-FOUND-MSG.
             05 FILLER  PIC X(25) VALUE 'REQUEST CODE OF #RRRRRRRR'.
             05 FILLER  PIC X(25) VALUE 'RRRRRR INVALID           '.
          03 SUB-FUNCTION-NOT-FOUND-MSG.
             05 FILLER  PIC X(25) VALUE 'FUNCTION BUY/SELL CALLED '.
             05 FILLER  PIC X(25) VALUE 'WITH AN INVALID SUBCODE  '.
          03 OVERFLOW-MSG.
             05 FILLER  PIC X(25) VALUE 'OVERFLOW WHEN CALCULATING'.
             05 FILLER  PIC X(25) VALUE ' SHARE VALUE             '.
          03 TOO-MANY-SHARES-MSG.
             05 FILLER  PIC X(25) VALUE 'CUSTOMER TRIED TO SELL MO'.
             05 FILLER  PIC X(25) VALUE 'RE SHARES THAN HE OWNS   '.
          03 NO-SHARES-MSG.
             05 FILLER  PIC X(25) VALUE 'CUSTOMER HAS NO SHARES TO'.
             05 FILLER  PIC X(25) VALUE ' SELL IN SELECTED COMPANY'.
          03 VALIDATE-MSG.
             05 FILLER  PIC X(25) VALUE 'VALIDATING COMPANY #CCCCC'.
             05 FILLER  PIC X(25) VALUE 'CCCCCCCCCCCCCC           '.
          03 TOO-MANY-MSG.
             05 FILLER  PIC X(25) VALUE 'TOO MANY SHARES REQUESTED'.
             05 FILLER  PIC X(25) VALUE ', MAX OWNERSHIP IS 9999  '.

       01 IN-BUFFER.
          03 IN-LL                     PIC S9(3) COMP.
          03 IN-ZZ                     PIC S9(3) COMP.
          03 IN-TRCD                   PIC X(10).
          03 IN-COMMAREA.
             05 REQUEST-TYPE           PIC X(15).
             05 RETURN-VALUE           PIC X(02).
             05 USERID                 PIC X(60).
             05 USER-PASSWORD          PIC X(10).
             05 COMPANY-NAME           PIC X(20).
             05 CORRELID               PIC X(32).
             05 UNIT-SHARE-VALUES.
                07 UNIT-SHARE-PRICE PIC X(08).
                07 UNIT-VALUE-7-DAYS PIC X(08).
                07 UNIT-VALUE-6-DAYS PIC X(08).
                07 UNIT-VALUE-5-DAYS PIC X(08).
                07 UNIT-VALUE-4-DAYS PIC X(08).
                07 UNIT-VALUE-3-DAYS PIC X(08).
                07 UNIT-VALUE-2-DAYS PIC X(08).
                07 UNIT-VALUE-1-DAYS PIC X(08).
             05 COMMISSION-COST-SELL PIC X(03).
             05 COMMISSION-COST-BUY PIC X(03).
             05 SHARES.
                07 NO-OF-SHARES        PIC X(04).
             05 SHARES-CONVERT REDEFINES SHARES.
                07 NO-OF-SHARES-DEC PIC 9(04).
             05 TOTAL-SHARE-VALUE      PIC X(12).
             05 BUY-SELL1              PIC X(04).
             05 BUY-SELL-PRICE1        PIC X(08).
             05 BUY-SELL2              PIC X(04).
             05 BUY-SELL-PRICE2        PIC X(08).
             05 BUY-SELL3              PIC X(04).
             05 BUY-SELL-PRICE3        PIC X(08).
             05 BUY-SELL4              PIC X(04).
             05 BUY-SELL-PRICE4        PIC X(08).
             05 ALARM-CHANGE           PIC X(03).
             05 UPDATE-BUY-SELL        PIC X(01).
             05 FILLER                 PIC X(15).
             05 COMPANY-NAME-BUFFER.
                07 COMPANY-NAME-TAB OCCURS 4 TIMES
                       INDEXED BY IN-COMPANY-NAME-IDX PIC X(20).

       01 OUT-BUFFER.
          03 OUT-LL                    PIC S9(3) COMP VALUE 384.
          03 OUT-ZZ                    PIC S9(3) COMP VALUE 0.
          03 OUT-COMMAREA.
             05 REQUEST-TYPE           PIC X(15).
             05 RETURN-VALUE           PIC X(02).
             05 USERID                 PIC X(60).
             05 USER-PASSWORD          PIC X(10).
             05 COMPANY-NAME           PIC X(20).
             05 CORRELID               PIC X(32).
             05 UNIT-SHARE-VALUES.
                07 UNIT-SHARE-PRICE PIC X(08).
                07 UNIT-VALUE-7-DAYS PIC X(08).
                07 UNIT-VALUE-6-DAYS PIC X(08).
                07 UNIT-VALUE-5-DAYS PIC X(08).
                07 UNIT-VALUE-4-DAYS PIC X(08).
                07 UNIT-VALUE-3-DAYS PIC X(08).
                07 UNIT-VALUE-2-DAYS PIC X(08).
                07 UNIT-VALUE-1-DAYS PIC X(08).
             05 COMMISSION-COST-SELL PIC X(03).
             05 COMMISSION-COST-BUY PIC X(03).
             05 SHARES.
                07 NO-OF-SHARES        PIC X(04).
             05 SHARES-CONVERT REDEFINES SHARES.
                07 NO-OF-SHARES-DEC PIC 9(04).
             05 TOTAL-SHARE-VALUE      PIC X(12).
             05 BUY-SELL1              PIC X(04).
             05 BUY-SELL-PRICE1        PIC X(08).
             05 BUY-SELL2              PIC X(04).
             05 BUY-SELL-PRICE2        PIC X(08).
             05 BUY-SELL3              PIC X(04).
             05 BUY-SELL-PRICE3        PIC X(08).
             05 BUY-SELL4              PIC X(04).
             05 BUY-SELL-PRICE4        PIC X(08).
             05 ALARM-CHANGE           PIC X(03).
             05 UPDATE-BUY-SELL        PIC X(01).
             05 IMSUSERID              PIC X(08).
             05 FILLER                 PIC X(07).
             05 COMPANY-NAME-BUFFER.
                07 COMPANY-NAME-TAB OCCURS 4 TIMES
                       INDEXED BY COMPANY-NAME-IDX PIC X(20).
         03  OUT-SEGNO            PIC 9(4).

       01 CUSTOMER-IO-BUFFER.
          03 KEYREC.
             05 CUSTOMER               PIC X(60).
             05 KEYREC-DOT             PIC X(01).
             05 COMPANY                PIC X(20).
          03 CONVERT1.
             05 NO-SHARES              PIC X(04).
          03 CONVERT2 REDEFINES CONVERT1.
             05 DEC-NO-SHARES          PIC 9(04).
          03 BUY-FROM                  PIC X(08).
          03 BUY-FROM-NO               PIC X(04).
          03 BUY-TO                    PIC X(08).
          03 BUY-TO-NO                 PIC X(04).
          03 SELL-FROM                 PIC X(08).
          03 SELL-FROM-NO              PIC X(04).
          03 SELL-TO                   PIC X(08).
          03 SELL-TO-NO                PIC X(04).
          03 ALARM-PERCENT             PIC X(03).

       01 COMPANY-IO-BUFFER.
          03 COMPANY                   PIC X(20).
          03 SHARE-VALUE.
             05 SHARE-VALUE-INT-PART        PIC X(05).
             05 FILLER                 PIC X(01).
             05 SHARE-VALUE-DEC-PART        PIC X(02).
          03 VALUE-1                   PIC X(08).
          03 VALUE-2                   PIC X(08).
          03 VALUE-3                   PIC X(08).
          03 VALUE-4                   PIC X(08).
          03 VALUE-5                   PIC X(08).
          03 VALUE-6                   PIC X(08).
          03 VALUE-7                   PIC X(08).
          03 COMMISSION-BUY            PIC X(03).
          03 COMMISSION-SELL           PIC X(03).

       01 MISCEL-VARS.

          03 REQUEST-CODE              PIC X(15).
          03 TIMESTAMP                 PIC 9(14).

      * Index fields

          03 INDEX-FIELDS.
             05 I                      PIC 99.
             05 J                      PIC 99.
             05 K                      PIC 99.

      * Work fields

          03 SHR-FLD.
             05 SHARES-OVERFLOW        PIC 9.
             05 SHARES-NORMAL          PIC 9(04).
          03 SHR-FLD-DEC REDEFINES SHR-FLD.
             05 SHARES-WORK1           PIC 9(5).

      * To true/false values

          03 TRUEFALSE.
             05 MOVE-DONE              PIC 9.
             05 BOOLEAN-TRUE           PIC 9 VALUE 1.
             05 BOOLEAN-FALSE          PIC 9 VALUE 0.

      * Constants for error codes and function calls

          03 CONSTANTS.
             05 RETURN-VALUES.
                07 CLEAN-RETURN        PIC X(02) VALUE '00'.
                07 UNKNOWN-REQUEST     PIC X(02) VALUE '01'.
                07 UNKNOWN-SUBTYPE     PIC X(02) VALUE '01'.
                07 BAD-CUST-READ       PIC X(02) VALUE '02'.
                07 BAD-CUST-WRITE      PIC X(02) VALUE '02'.
                07 BAD-CUST-REWRITE    PIC X(02) VALUE '02'.
                07 BAD-COMP-READ       PIC X(02) VALUE '03'.
                07 OVERFLOW-RC         PIC X(02) VALUE '04'.
                07 COMPANY-NOT-FOUND   PIC X(02) VALUE '05'.
                07 INVALID-SALE        PIC X(02) VALUE '06'.
                07 INVALID-BUY         PIC X(02) VALUE '06'.
                07 PGM-LOGIC-ERROR     PIC X(02) VALUE '98'.
                07 CUSTOMER-NOT-FOUND  PIC X(02) VALUE '99'.
             05 REQUEST-TYPES.
                07 GET-COMPANY-REQ     PIC X(15)
                       VALUE 'GET_COMPANY    '.
                07 GET-COMPANY-REQ1    PIC X(15)
                       VALUE 'Get_Company    '.
                07 SHARE-VALUE-REQ     PIC X(15)
                       VALUE 'SHARE_VALUE    '.
                07 SHARE-VALUE-REQ1    PIC X(15)
                       VALUE 'Share_Value    '.
                07 BUY-SELL-REQ        PIC X(15)
                       VALUE 'BUY_SELL       '.
                07 BUY-SELL-REQ1       PIC X(15)
                       VALUE 'Buy_Sell       '.
             05 SUBTYPES.
                07 SUBTYPE-UPDATE      PIC X(01) VALUE '0'.
                07 SUBTYPE-BUY         PIC X(01) VALUE '1'.
                07 SUBTYPE-SELL        PIC X(01) VALUE '2'.
             05 MISC.
                07 OVERFLOW-VALUE      PIC X(12) VALUE 'XXXXXXXXX.XX'.


      * FIELDS FOR CONVERTING THE TYPE OF THE FIELDS

          03 CONVERSION-FIELDS.
             05 CHAR-VALUE.
                07 CHAR-INT-PART       PIC X(09).
                07 FILLER              PIC X VALUE '.'.
                07 CHAR-DEC-PART       PIC X(02).
             05 NUM-VALUE REDEFINES CHAR-VALUE.
                07 NUM-INT-PART        PIC 9(09).
                07 GUB92               PIC X.
                07 NUM-DEC-PART        PIC 9(02).
             05 DEC-VALUE.
                07 DECIMAL-SHARE-VALUE PIC 9(11)V99.
             05 WORKING-DECIMAL-VALUE REDEFINES DEC-VALUE.
                07 WORKING-OVERFLOW    PIC 9(02).
                07 WORKING-INT-PART    PIC 9(09).
                07 WORKING-DEC-PART    PIC 9(02).
             05 WORKING-CHAR-VALUE REDEFINES WORKING-DECIMAL-VALUE.
                07 WCHAR-OVERFLOW      PIC X(02).
                07 WCHAR-INT-PART      PIC X(09).
                07 WCHAR-DEC-PART      PIC X(02).

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
           02  PROC-OPTIONS    PIC  X(4).
           02  RESERVE-DLI     PIC  X(4).
           02  SEG-NAME-FB     PIC  X(8).
           02  LENGTH-FB-KEY   PIC  9(4).
           02  NUMB-SENS-SEGS  PIC  9(4).
           02  KEY-FB-AREA     PIC  X(17).
       01  DBPCB-CUST.
           02  DBD-NAME        PIC  X(8).
           02  SEG-LEVEL       PIC  X(2).
           02  DB-CUST-STATUS  PIC  X(2).
           02  PROC-OPTIONS    PIC  X(4).
           02  RESERVE-DLI     PIC  X(4).
           02  SEG-NAME-FB     PIC  X(8).
           02  LENGTH-FB-KEY   PIC  9(4).
           02  NUMB-SENS-SEGS  PIC  9(4).
           02  KEY-FB-AREA     PIC  X(17).
       01  DBPCB-COMP.
           02  DBD-NAME        PIC  X(8).
           02  SEG-LEVEL       PIC  X(2).
           02  DB-COMP-STATUS  PIC  X(2).
           02  PROC-OPTIONS    PIC  X(4).
           02  RESERVE-DLI     PIC  x(4).
           02  SEG-NAME-FB     PIC  X(8).
           02  LENGTH-FB-KEY   PIC  9(4).
           02  NUMB-SENS-SEGS  PIC  9(4).
           02  KEY-FB-AREA     PIC  X(17).

       PROCEDURE DIVISION USING IOPCB, ALTPCB, DBPCB-CUST, DBPCB-COMP.

       MAINLINE SECTION.

           INITIALIZE CUSTOMER-IO-BUFFER.
           INITIALIZE COMPANY-IO-BUFFER.
           CALL 'CBLTDLI' USING GET-UNIQUE, IOPCB, IN-BUFFER.

           MOVE FUNCTION CURRENT-DATE(1:14) TO TIMESTAMP.
           DISPLAY TIMESTAMP ' IOPCB ModNAME: ' MODNAME.
           DISPLAY TIMESTAMP ' IOPCB Userid: '  USERID OF IOPCB.

           DISPLAY TIMESTAMP ' IN-LL       : '  IN-LL OF IN-BUFFER.
           DISPLAY TIMESTAMP ' REQUEST-TYPE: '
                                 REQUEST-TYPE OF IN-COMMAREA.
           DISPLAY TIMESTAMP ' USERID      : '
                                 USERID OF IN-COMMAREA.
           DISPLAY TIMESTAMP ' COMPANY-NAME: '
                                 COMPANY-NAME OF IN-COMMAREA.
           DISPLAY TIMESTAMP ' IN-COMMAREA : ' IN-COMMAREA.
           MOVE IN-COMMAREA TO OUT-COMMAREA.
           MOVE USERID OF IOPCB TO IMSUSERID.
           IF MODNAME IS EQUAL TO 'TRDRQUO'
                MOVE 'TRDOQUO' TO MODNAME.
           IF MODNAME IS EQUAL TO 'TRDRCMP'
                MOVE 'TRDOCMP' TO MODNAME.
           IF MODNAME IS EQUAL TO 'TRDRBUY'
                MOVE 'TRDOBUY' TO MODNAME.
           IF MODNAME IS EQUAL TO 'TRDRSEL'
                MOVE 'TRDOSEL' TO MODNAME.
           MOVE REQUEST-TYPE OF IN-COMMAREA TO REQUEST-CODE.

           EVALUATE REQUEST-CODE
              WHEN GET-COMPANY-REQ
                   PERFORM GET-COMPANY
              WHEN GET-COMPANY-REQ1
                   PERFORM GET-COMPANY
              WHEN SHARE-VALUE-REQ
                   PERFORM GET-SHARE-VALUE
              WHEN SHARE-VALUE-REQ1
                   PERFORM GET-SHARE-VALUE
              WHEN BUY-SELL-REQ
                   PERFORM BUY-SELL
              WHEN BUY-SELL-REQ1
                   PERFORM BUY-SELL
              WHEN OTHER
                   MOVE UNKNOWN-REQUEST TO RETURN-VALUE OF OUT-BUFFER
                   DISPLAY REQUEST-NOT-FOUND-MSG
           END-EVALUATE.

           MOVE FUNCTION CURRENT-DATE(1:14) TO TIMESTAMP.
           ADD +1 TO SEGNO.
           MOVE SEGNO TO OUT-SEGNO.
           DISPLAY TIMESTAMP ' OUT-LL      : ' OUT-LL OF OUT-BUFFER.
           DISPLAY TIMESTAMP ' OUT-BUFFER  : ' OUT-BUFFER.
           IF MODNAME EQUAL SPACES
              THEN
                 CALL 'CBLTDLI' USING ISRT, IOPCB, OUT-BUFFER
              ELSE
                 DISPLAY TIMESTAMP ' MODNAME     : ' MODNAME
                 CALL 'CBLTDLI' USING ISRT, IOPCB, OUT-BUFFER, MODNAME
                 MOVE SPACES TO MODNAME.

           GOBACK.

       MAINLINE-EXIT.

           EXIT.
      /
      *****************************************************************
      * GET ALL THE COMPANY NAMES FROM THE 'COMPFILE'                 *
      *****************************************************************

       GET-COMPANY SECTION.

           MOVE SPACES TO SSA-COMPANY-KEY.

           CALL 'CBLTDLI' USING GET-UNIQUE, DBPCB-COMP,
                    COMPANY-IO-BUFFER.

           PERFORM VARYING COMPANY-NAME-IDX FROM 1 BY 1
               UNTIL DB-COMP-STATUS = END-OF-DATABASE OR
                     COMPANY-NAME-IDX > 4

               MOVE COMPANY OF COMPANY-IO-BUFFER TO
                  COMPANY-NAME-TAB OF OUT-BUFFER (COMPANY-NAME-IDX)
               CALL 'CBLTDLI' USING GET-NEXT, DBPCB-COMP
                               COMPANY-IO-BUFFER

           END-PERFORM.

       GET-COMPANY-EXIT.

           EXIT.
      /
      *****************************************************************
       BUY-SELL SECTION.
           EVALUATE UPDATE-BUY-SELL OF OUT-BUFFER
             WHEN SUBTYPE-UPDATE
               PERFORM VALIDATE-COMPANY-EXISTS
               IF RETURN-VALUE OF OUT-BUFFER = CLEAN-RETURN
                  PERFORM BUY-SELL-UPDATE-FUNCTION
               END-IF
             WHEN SUBTYPE-BUY
               PERFORM VALIDATE-COMPANY-EXISTS
               IF RETURN-VALUE OF OUT-BUFFER = CLEAN-RETURN
                  PERFORM BUY-SELL-BUY-FUNCTION
               END-IF
             WHEN SUBTYPE-SELL
               PERFORM VALIDATE-COMPANY-EXISTS
               IF RETURN-VALUE OF OUT-BUFFER = CLEAN-RETURN
                  PERFORM BUY-SELL-SELL-FUNCTION
               END-IF
             WHEN OTHER
               MOVE UNKNOWN-SUBTYPE TO RETURN-VALUE OF OUT-BUFFER
               DISPLAY SUB-FUNCTION-NOT-FOUND-MSG
           END-EVALUATE
           .
       BUY-SELL-EXIT.
           EXIT.
      *****************************************************************
       BUY-SELL-UPDATE-FUNCTION SECTION.
           PERFORM READ-CUSTFILE-FOR-UPDATE
           EVALUATE RETURN-VALUE OF OUT-BUFFER
             WHEN CLEAN-RETURN
                  PERFORM UPDATE-BUY-SELL-FIELDS
                  PERFORM REWRITE-CUSTFILE
             WHEN CUSTOMER-NOT-FOUND
                  MOVE CLEAN-RETURN TO RETURN-VALUE OF OUT-BUFFER
                  PERFORM BUILD-NEW-CUSTOMER
                  PERFORM UPDATE-BUY-SELL-FIELDS
                  PERFORM WRITE-CUSTFILE
             WHEN OTHER
                  MOVE BAD-CUST-READ TO RETURN-VALUE OF OUT-BUFFER
           END-EVALUATE
           .
       BUY-SELL-UPDATE-FUNCTION-EXIT.
           EXIT.
      *****************************************************************
       UPDATE-BUY-SELL-FIELDS SECTION.
      * Move the values from the buy/sell area into the custarea for
      * the rewrite regardless of whether they are blank or not
           MOVE BUY-SELL1       OF OUT-BUFFER TO BUY-FROM-NO
           MOVE BUY-SELL-PRICE1 OF OUT-BUFFER TO BUY-FROM
           MOVE BUY-SELL2       OF OUT-BUFFER TO BUY-TO-NO
           MOVE BUY-SELL-PRICE2 OF OUT-BUFFER TO BUY-TO
           MOVE BUY-SELL3       OF OUT-BUFFER TO SELL-FROM-NO
           MOVE BUY-SELL-PRICE3 OF OUT-BUFFER TO SELL-FROM
           MOVE BUY-SELL4       OF OUT-BUFFER TO SELL-TO-NO
           MOVE BUY-SELL-PRICE4 OF OUT-BUFFER TO SELL-TO
           MOVE ALARM-CHANGE    OF OUT-BUFFER TO ALARM-PERCENT
           .
       UPDATE-BUY-SELL-FIELDS-EXIT.
           EXIT.
      *****************************************************************
       BUY-SELL-BUY-FUNCTION SECTION.
      * Check we have a record for this customer.company
      * Apply the effect of the buy and issue the WRITE/REWRITE
      * Calcuate new number of shares and UPDATE CUSTFILE
      * Check whether we have any shares yet
           PERFORM READ-CUSTFILE-FOR-UPDATE
           EVALUATE RETURN-VALUE OF OUT-BUFFER
             WHEN CLEAN-RETURN
                  PERFORM CALCULATE-SHARES-BOUGHT
                  IF RETURN-VALUE OF OUT-BUFFER = CLEAN-RETURN
                  THEN
                    PERFORM UPDATE-BUY-SELL-FIELDS
                    PERFORM REWRITE-CUSTFILE
      * @test 2 lines
      *             PERFORM CALCULATE-SHARE-VALUE
                    PERFORM BUILD-RESP-COMMAREA
                  END-IF
             WHEN CUSTOMER-NOT-FOUND
                  MOVE CLEAN-RETURN TO RETURN-VALUE OF OUT-BUFFER
                  PERFORM BUILD-NEW-CUSTOMER
                  PERFORM CALCULATE-SHARES-BOUGHT
                  IF RETURN-VALUE OF OUT-BUFFER = CLEAN-RETURN
                  THEN
                    PERFORM UPDATE-BUY-SELL-FIELDS
                    PERFORM WRITE-CUSTFILE
      * @test 2 lines
      *             PERFORM CALCULATE-SHARE-VALUE
                    PERFORM BUILD-RESP-COMMAREA
                  END-IF
             WHEN OTHER
                  MOVE BAD-CUST-READ TO RETURN-VALUE OF OUT-BUFFER
           END-EVALUATE
           .
       BUY-SELL-BUY-FUNCTION-EXIT.
           EXIT.
      *****************************************************************
       CALCULATE-SHARES-BOUGHT SECTION.
      * Move new number of shares into i/p Commarea and
      * customer file write commarea for update
           ADD NO-OF-SHARES-DEC OF OUT-BUFFER TO DEC-NO-SHARES
                             GIVING SHARES-WORK1
           EVALUATE SHARES-OVERFLOW
             WHEN 0
               MOVE SHARES-NORMAL TO NO-OF-SHARES-DEC OF OUT-BUFFER
               MOVE SHARES-NORMAL TO DEC-NO-SHARES
               PERFORM UPDATE-BUY-SELL-FIELDS
             WHEN OTHER
               MOVE INVALID-BUY TO RETURN-VALUE OF OUT-BUFFER
               DISPLAY TOO-MANY-MSG
           END-EVALUATE
           .
       CALCULATE-SHARES-BOUGHT-EXIT.
           EXIT.
      *****************************************************************
       CALCULATE-SHARES-SOLD SECTION.
      * Move new number of shares into i/p Commarea and
      * customer file write commarea for update
           SUBTRACT NO-OF-SHARES-DEC OF OUT-BUFFER
              FROM DEC-NO-SHARES GIVING DEC-NO-SHARES
           MOVE DEC-NO-SHARES TO NO-OF-SHARES-DEC OF OUT-BUFFER
           .
       CALCULATE-SHARES-SOLD-EXIT.
           EXIT.
      *****************************************************************
       BUY-SELL-SELL-FUNCTION SECTION.
      * Check we have a record for this customer.company, if not EXIT
      * Check that we can meet the sell request, if not EXIT
      * Calcuate new number of shares and UPDATE CUSTFILE
      * Calculate new share TOTAL SHARE VALUE
      * Check whether we have any shares to sell
           PERFORM READ-CUSTFILE-FOR-UPDATE
           EVALUATE RETURN-VALUE OF OUT-BUFFER
             WHEN CLEAN-RETURN
                  IF NO-OF-SHARES-DEC OF OUT-BUFFER IS GREATER THAN
                     DEC-NO-SHARES
                  THEN
                    MOVE INVALID-SALE TO RETURN-VALUE OF OUT-BUFFER
                    DISPLAY TOO-MANY-MSG
                  ELSE
                    PERFORM CALCULATE-SHARES-SOLD
                    PERFORM UPDATE-BUY-SELL-FIELDS
                    PERFORM REWRITE-CUSTFILE
      * @test 2 lines
      *             PERFORM CALCULATE-SHARE-VALUE
                    PERFORM BUILD-RESP-COMMAREA
                  END-IF
             WHEN CUSTOMER-NOT-FOUND
                  MOVE INVALID-SALE TO RETURN-VALUE OF OUT-BUFFER
                  DISPLAY NO-SHARES-MSG
             WHEN OTHER
                  MOVE BAD-CUST-READ TO RETURN-VALUE OF OUT-BUFFER
           END-EVALUATE
           .
       BUY-SELL-SELL-FUNCTION-EXIT.
           EXIT.
      *****************************************************************
       VALIDATE-COMPANY-EXISTS SECTION.
           PERFORM READ-COMPFILE
           .
       VALIDATE-COMPANY-EXISTS-EXIT.
           EXIT.
      *****************************************************************
       GET-SHARE-VALUE SECTION.
           PERFORM READ-CUSTFILE
           EVALUATE RETURN-VALUE OF OUT-BUFFER
             WHEN CLEAN-RETURN
                  CONTINUE
             WHEN CUSTOMER-NOT-FOUND
                  MOVE CLEAN-RETURN TO RETURN-VALUE OF OUT-BUFFER
                  PERFORM SET-DUMMY-CUST-RECORD
             WHEN OTHER
                  CONTINUE
           END-EVALUATE
           IF RETURN-VALUE OF OUT-BUFFER IS EQUAL TO CLEAN-RETURN
              PERFORM READ-COMPFILE
           IF RETURN-VALUE OF OUT-BUFFER IS EQUAL TO CLEAN-RETURN
              PERFORM BUILD-RESP-COMMAREA
           .
       GET-SHARE-VALUE-EXIT.
           EXIT.
      *****************************************************************
       READ-CUSTFILE SECTION.
      * Build record key
           MOVE USERID OF OUT-BUFFER TO SSA-CUSTOMER-KEY
           MOVE COMPANY-NAME OF OUT-BUFFER TO SSA-CUSTOMER-COMP

           CALL 'CBLTDLI' USING GET-UNIQUE, DBPCB-CUST,
            CUSTOMER-IO-BUFFER, SSA-CUSTOMER

           EVALUATE DB-CUST-STATUS
              WHEN NORMAL
                   MOVE CLEAN-RETURN TO RETURN-VALUE OF OUT-BUFFER
              WHEN NOTFND
                   MOVE CUSTOMER-NOT-FOUND TO RETURN-VALUE OF OUT-BUFFER
              WHEN OTHER
                   MOVE BAD-CUST-READ TO RETURN-VALUE OF OUT-BUFFER
                   DISPLAY 'BAD-CUST-READ:' DB-CUST-STATUS
           END-EVALUATE
           .
       READ-CUSTFILE-EXIT.
           EXIT.
      *****************************************************************
       READ-CUSTFILE-FOR-UPDATE SECTION.
      * Build record key
           MOVE USERID OF OUT-BUFFER TO SSA-CUSTOMER-KEY
           MOVE COMPANY-NAME OF OUT-BUFFER TO SSA-CUSTOMER-COMP

           CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DBPCB-CUST,
            CUSTOMER-IO-BUFFER, SSA-CUSTOMER

           EVALUATE DB-CUST-STATUS
              WHEN NORMAL
                   MOVE CLEAN-RETURN TO RETURN-VALUE OF OUT-BUFFER
              WHEN NOTFND
                   MOVE CUSTOMER-NOT-FOUND TO RETURN-VALUE OF OUT-BUFFER
              WHEN OTHER
                   MOVE BAD-CUST-READ TO RETURN-VALUE OF OUT-BUFFER
                   DISPLAY 'BAD-CUST-READ:' DB-CUST-STATUS
           END-EVALUATE
           .
       READ-CUSTFILE-FOR-UPDATE-EXIT.
           EXIT.
      *****************************************************************
       WRITE-CUSTFILE SECTION.
      * Build record key
      *    MOVE USERID OF OUT-BUFFER TO SSA-CUSTOMER-KEY
      *    MOVE COMPANY-NAME OF OUT-BUFFER TO SSA-CUSTOMER-COMP

           CALL 'CBLTDLI' USING ISRT, DBPCB-CUST,
            CUSTOMER-IO-BUFFER, SSA-CUSTOMER-ISRT

           EVALUATE DB-CUST-STATUS
             WHEN NORMAL
                  CONTINUE
             WHEN OTHER
                  MOVE BAD-CUST-WRITE TO RETURN-VALUE OF OUT-BUFFER
                  DISPLAY 'BAD-CUST-WRITE:' DB-CUST-STATUS
           END-EVALUATE
           .
       WRITE-CUSTFILE-EXIT.
           EXIT.
      *****************************************************************
       REWRITE-CUSTFILE SECTION.
      * Update an existing record in the CUSTFILE

           MOVE USERID OF OUT-BUFFER TO SSA-CUSTOMER-KEY
           MOVE COMPANY-NAME OF OUT-BUFFER TO SSA-CUSTOMER-COMP

           CALL 'CBLTDLI' USING REPL, DBPCB-CUST,
            CUSTOMER-IO-BUFFER

           EVALUATE DB-CUST-STATUS
             WHEN NORMAL
                  CONTINUE
             WHEN OTHER
                  MOVE BAD-CUST-REWRITE TO RETURN-VALUE OF OUT-BUFFER
                  DISPLAY 'BAD-CUST-REWRITE: ' DB-CUST-STATUS
           END-EVALUATE
           .
       WRITE-CUSTFILE-EXIT.
           EXIT.
      *****************************************************************
       READ-COMPFILE SECTION.
           MOVE COMPANY-NAME OF OUT-BUFFER  TO SSA-COMPANY-KEY

           CALL 'CBLTDLI' USING GET-UNIQUE, DBPCB-COMP,
                    COMPANY-IO-BUFFER, SSA-COMPANY

           EVALUATE DB-COMP-STATUS
              WHEN NORMAL
                   MOVE CLEAN-RETURN TO RETURN-VALUE OF OUT-BUFFER
              WHEN NOTFND
                   MOVE COMPANY-NOT-FOUND TO RETURN-VALUE OF OUT-BUFFER
              WHEN OTHER
                   MOVE BAD-COMP-READ TO RETURN-VALUE OF OUT-BUFFER
                   DISPLAY 'BAD-COMP-READ: ' DB-COMP-STATUS
           END-EVALUATE
           .
       READ-COMPFILE-EXIT.
           EXIT.
      *****************************************************************
       BUILD-NEW-CUSTOMER SECTION.
      * We are creating a new customer in the customer file.  Since the
      * read failed we create a blank record
           MOVE USERID OF OUT-BUFFER TO CUSTOMER
           MOVE '.' TO KEYREC-DOT
           MOVE COMPANY-NAME OF OUT-BUFFER TO COMPANY
                    OF CUSTOMER-IO-BUFFER
           MOVE '0000' TO NO-SHARES
           MOVE '00000.00' TO BUY-FROM
           MOVE '0000' TO BUY-FROM-NO
           MOVE '00000.00' TO BUY-TO
           MOVE '0000' TO BUY-TO-NO
           MOVE '00000.00' TO SELL-FROM
           MOVE '0000' TO SELL-FROM-NO
           MOVE '00000.00' TO SELL-TO
           MOVE '0000' TO SELL-TO-NO
           MOVE '000' TO ALARM-PERCENT
           .
       BUILD-NEW-CUSTOMER-EXIT.
           EXIT.
      *****************************************************************
       SET-DUMMY-CUST-RECORD SECTION.
      * We are trying to return a quote but the user does not exist for
      * this company but we still need to return meaningful values
           MOVE ' ' TO CUSTOMER OF CUSTOMER-IO-BUFFER
           MOVE ' ' TO COMPANY OF CUSTOMER-IO-BUFFER
           MOVE '0000' TO NO-SHARES
           MOVE '        ' TO BUY-FROM
           MOVE '        ' TO BUY-TO
           MOVE '        ' TO SELL-FROM
           MOVE '        ' TO SELL-TO
           .
       SET-DUMMY-CUST-RECORD-EXIT.
           EXIT.
      *****************************************************************
       BUILD-RESP-COMMAREA SECTION.
      * Calculate the value of the shares today
           PERFORM CALCULATE-SHARE-VALUE
      * Return no of shares and unit value today
           MOVE SHARE-VALUE OF COMPANY-IO-BUFFER TO
                UNIT-SHARE-PRICE OF OUT-BUFFER
           MOVE NO-SHARES OF CUSTOMER-IO-BUFFER TO
                NO-OF-SHARES OF OUT-BUFFER
      * Return old unit share prices
           MOVE VALUE-1 TO UNIT-VALUE-1-DAYS OF OUT-BUFFER
           MOVE VALUE-2 TO UNIT-VALUE-2-DAYS OF OUT-BUFFER
           MOVE VALUE-3 TO UNIT-VALUE-3-DAYS OF OUT-BUFFER
           MOVE VALUE-4 TO UNIT-VALUE-4-DAYS OF OUT-BUFFER
           MOVE VALUE-5 TO UNIT-VALUE-5-DAYS OF OUT-BUFFER
           MOVE VALUE-6 TO UNIT-VALUE-6-DAYS OF OUT-BUFFER
           MOVE VALUE-7 TO UNIT-VALUE-7-DAYS OF OUT-BUFFER
      * Return commision figures
           MOVE COMMISSION-SELL OF COMPANY-IO-BUFFER TO
                COMMISSION-COST-SELL OF OUT-BUFFER
           MOVE COMMISSION-BUY OF COMPANY-IO-BUFFER TO
                 COMMISSION-COST-BUY OF OUT-BUFFER
      * Fill in buy/sell numbers
           MOVE BUY-FROM-NO  TO BUY-SELL1       OF OUT-BUFFER
           MOVE BUY-FROM     TO BUY-SELL-PRICE1 OF OUT-BUFFER
           MOVE BUY-TO-NO    TO BUY-SELL2       OF OUT-BUFFER
           MOVE BUY-TO       TO BUY-SELL-PRICE2 OF OUT-BUFFER
           MOVE SELL-FROM-NO TO BUY-SELL3       OF OUT-BUFFER
           MOVE SELL-FROM    TO BUY-SELL-PRICE3 OF OUT-BUFFER
           MOVE SELL-TO-NO   TO BUY-SELL4       OF OUT-BUFFER
           MOVE SELL-TO      TO BUY-SELL-PRICE4 OF OUT-BUFFER
      * Fill in alarm value
           MOVE ALARM-PERCENT TO ALARM-CHANGE OF OUT-BUFFER
           .
       BUILD-RESP-COMMAREA-EXIT.
           EXIT.
      *****************************************************************
       CALCULATE-SHARE-VALUE SECTION.
      * Calculate value of shares today
      * The following works on AIX but not on NT...it should do!
      *    COMPUTE FP-NO-SHARES = FUNCTION NUMVAL (NO-SHARES)
      *    COMPUTE FP-SHARE-VALUE = FUNCTION NUMVAL (SHARE-VALUE)
      * Sum is DECIMAL-SHARE-VALUE = SHARE-VALUE * NO-SHARES
           MOVE 0 TO WORKING-OVERFLOW
           MOVE SHARE-VALUE-INT-PART TO WORKING-INT-PART
           MOVE SHARE-VALUE-DEC-PART TO WORKING-DEC-PART
           MULTIPLY DECIMAL-SHARE-VALUE BY DEC-NO-SHARES
             GIVING DECIMAL-SHARE-VALUE
           EVALUATE WORKING-OVERFLOW
             WHEN 0
               MOVE WORKING-INT-PART TO NUM-INT-PART
               MOVE WORKING-DEC-PART TO NUM-DEC-PART
               MOVE CHAR-VALUE TO TOTAL-SHARE-VALUE OF OUT-BUFFER
             WHEN OTHER
               MOVE OVERFLOW-VALUE TO TOTAL-SHARE-VALUE OF OUT-BUFFER
               MOVE OVERFLOW-RC TO RETURN-VALUE OF OUT-BUFFER
           END-EVALUATE
           .
       CALCULATE-SHARE-VALUE-EXIT.
           EXIT.
