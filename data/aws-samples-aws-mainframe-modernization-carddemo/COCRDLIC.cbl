      *****************************************************************         
      * Program:     COCRDLIC.CBL                                     *         
      * Layer:       Business logic                                   *         
      * Function:    List Credit Cards                                          
      *              a) All cards if no context passed and admin user           
      *              b) Only the ones associated with ACCT in COMMAREA          
      *                 if user is not admin                                    
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
       PROGRAM-ID.                                                              
           COCRDLIC.                                                            
       DATE-WRITTEN.                                                            
           April 2022.                                                          
       DATE-COMPILED.                                                           
           Today.                                                               
                                                                                
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       DATA DIVISION.                                                           
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
                                                                                
       01  WS-MISC-STORAGE.                                                     
      ******************************************************************        
      * General CICS related                                                    
      ******************************************************************        
                                                                                
         05 WS-CICS-PROCESSNG-VARS.                                             
            07 WS-RESP-CD                          PIC S9(09) COMP              
                                                   VALUE ZEROS.                 
            07 WS-REAS-CD                          PIC S9(09) COMP              
                                                   VALUE ZEROS.                 
            07 WS-TRANID                           PIC X(4)                     
                                                   VALUE SPACES.                
      ******************************************************************        
      * Input edits                                                             
      ******************************************************************        
         05 WS-INPUT-FLAG                          PIC X(1).                    
           88  INPUT-OK                            VALUES '0'                   
                                                          ' '                   
                                                   LOW-VALUES.                  
           88  INPUT-ERROR                         VALUE '1'.                   
         05  WS-EDIT-ACCT-FLAG                     PIC X(1).                    
           88  FLG-ACCTFILTER-NOT-OK               VALUE '0'.                   
           88  FLG-ACCTFILTER-ISVALID             VALUE '1'.                    
           88  FLG-ACCTFILTER-BLANK                VALUE ' '.                   
         05  WS-EDIT-CARD-FLAG                     PIC X(1).                    
           88  FLG-CARDFILTER-NOT-OK               VALUE '0'.                   
           88  FLG-CARDFILTER-ISVALID             VALUE '1'.                    
           88  FLG-CARDFILTER-BLANK                VALUE ' '.                   
         05 WS-EDIT-SELECT-COUNTER                PIC S9(04)                    
                                                  USAGE COMP-3                  
                                                  VALUE 0.                      
         05 WS-EDIT-SELECT-FLAGS                  PIC X(7)                      
                                                  VALUE LOW-VALUES.             
         05 WS-EDIT-SELECT-ARRAY REDEFINES  WS-EDIT-SELECT-FLAGS.               
            10 WS-EDIT-SELECT                      PIC X(1)                     
                                                  OCCURS 7 TIMES.               
               88 SELECT-OK                        VALUES 'S', 'U'.             
               88 VIEW-REQUESTED-ON                VALUE 'S'.                   
               88 UPDATE-REQUESTED-ON              VALUE 'U'.                   
               88 SELECT-BLANK                     VALUES                       
                                                   ' ',                         
                                                   LOW-VALUES.                  
         05 WS-EDIT-SELECT-ERROR-FLAGS             PIC X(7).                    
         05 WS-EDIT-SELECT-ERROR-FLAGX     REDEFINES                            
            WS-EDIT-SELECT-ERROR-FLAGS.                                         
            10 WS-EDIT-SELECT-ERRORS OCCURS 7 TIMES.                            
               20 WS-ROW-CRDSELECT-ERROR          PIC X(1).                     
                  88 WS-ROW-SELECT-ERROR          VALUE '1'.                    
         05 WS-SUBSCRIPT-VARS.                                                  
            10 I                                  PIC S9(4) COMP                
                                                  VALUE 0.                      
            10 I-SELECTED                         PIC S9(4) COMP                
                                                  VALUE 0.                      
               88 DETAIL-WAS-REQUESTED            VALUES 1 THRU 7.              
      ******************************************************************        
      * Output edits                                                            
      ******************************************************************        
         05 CICS-OUTPUT-EDIT-VARS.                                              
           10  CARD-ACCT-ID-X                      PIC X(11).                   
           10  CARD-ACCT-ID-N REDEFINES CARD-ACCT-ID-X                          
                                                   PIC 9(11).                   
           10  CARD-CVV-CD-X                       PIC X(03).                   
           10  CARD-CVV-CD-N REDEFINES  CARD-CVV-CD-X                           
                                                   PIC 9(03).                   
           10  FLG-PROTECT-SELECT-ROWS             PIC X(1).                    
           88  FLG-PROTECT-SELECT-ROWS-NO          VALUE '0'.                   
           88  FLG-PROTECT-SELECT-ROWS-YES         VALUE '1'.                   
      ******************************************************************        
      * Output Message Construction                                             
      ******************************************************************        
         05  WS-LONG-MSG                           PIC X(500).                  
         05  WS-INFO-MSG                           PIC X(45).                   
           88  WS-NO-INFO-MESSAGE                 VALUES                        
                                                  SPACES LOW-VALUES.            
           88  WS-INFORM-REC-ACTIONS          VALUE                             
               'TYPE S FOR DETAIL, U TO UPDATE ANY RECORD'.                     
         05  WS-ERROR-MSG                         PIC X(75).                    
           88  WS-ERROR-MSG-OFF                   VALUE SPACES.                 
           88  WS-EXIT-MESSAGE                     VALUE                        
               'PF03 PRESSED.EXITING'.                                          
           88  WS-NO-RECORDS-FOUND                 VALUE                        
               'NO RECORDS FOUND FOR THIS SEARCH CONDITION.'.                   
           88  WS-MORE-THAN-1-ACTION              VALUE                         
               'PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE'.               
           88  WS-INVALID-ACTION-CODE              VALUE                        
               'INVALID ACTION CODE'.                                           
         05  WS-PFK-FLAG                           PIC X(1).                    
           88  PFK-VALID                           VALUE '0'.                   
           88  PFK-INVALID                         VALUE '1'.                   
         05  WS-CONTEXT-FLAG                       PIC X(1).                    
           88  WS-CONTEXT-FRESH-START              VALUE '0'.                   
           88  WS-CONTEXT-FRESH-START-NO           VALUE '1'.                   
      ******************************************************************        
      * File and data Handling                                                  
      ******************************************************************        
         05 WS-FILE-HANDLING-VARS.                                              
            10  WS-CARD-RID.                                                    
                20  WS-CARD-RID-CARDNUM            PIC X(16).                   
                20  WS-CARD-RID-ACCT-ID            PIC 9(11).                   
                20  WS-CARD-RID-ACCT-ID-X          REDEFINES                    
                    WS-CARD-RID-ACCT-ID            PIC X(11).                   
                                                                                
                                                                                
                                                                                
         05  WS-SCRN-COUNTER               PIC S9(4) COMP VALUE 0.              
                                                                                
         05  WS-FILTER-RECORD-FLAG                 PIC X(1).                    
           88  WS-EXCLUDE-THIS-RECORD               VALUE '0'.                  
           88  WS-DONOT-EXCLUDE-THIS-RECORD         VALUE '1'.                  
         05  WS-RECORDS-TO-PROCESS-FLAG            PIC X(1).                    
           88  READ-LOOP-EXIT                      VALUE '0'.                   
           88  MORE-RECORDS-TO-READ                VALUE '1'.                   
         05  WS-FILE-ERROR-MESSAGE.                                             
           10  FILLER                              PIC X(12)                    
                                                   VALUE 'File Error:'.         
           10  ERROR-OPNAME                        PIC X(8)                     
                                                   VALUE SPACES.                
           10  FILLER                              PIC X(4)                     
                                                   VALUE ' on '.                
           10  ERROR-FILE                          PIC X(9)                     
                                                   VALUE SPACES.                
           10  FILLER                              PIC X(15)                    
                                                   VALUE                        
                                                   ' returned RESP '.           
           10  ERROR-RESP                          PIC X(10)                    
                                                   VALUE SPACES.                
           10  FILLER                              PIC X(7)                     
                                                   VALUE ',RESP2 '.             
           10  ERROR-RESP2                         PIC X(10)                    
                                                   VALUE SPACES.                
          10  FILLER                               PIC X(5).                    
                                                                                
      ******************************************************************
      * Literals and Constants                                                  
      ******************************************************************        
       01 WS-CONSTANTS.                                                         
         05  WS-MAX-SCREEN-LINES                    PIC S9(4) COMP              
                                                    VALUE 7.                    
         05  LIT-THISPGM                            PIC X(8)                    
             VALUE 'COCRDLIC'.                                                  
         05  LIT-THISTRANID                         PIC X(4)                    
             VALUE 'CCLI'.                                                      
         05  LIT-THISMAPSET                         PIC X(7)                    
             VALUE 'COCRDLI'.                                                   
         05  LIT-THISMAP                            PIC X(7)                    
             VALUE 'CCRDLIA'.                                                   
         05  LIT-MENUPGM                            PIC X(8)                    
             VALUE 'COMEN01C'.                                                  
         05  LIT-MENUTRANID                         PIC X(4)                    
             VALUE 'CM00'.                                                      
         05  LIT-MENUMAPSET                         PIC X(7)                    
             VALUE 'COMEN01'.                                                   
         05  LIT-MENUMAP                            PIC X(7)                    
             VALUE 'COMEN1A'.                                                   
         05  LIT-CARDDTLPGM                         PIC X(8)                    
             VALUE 'COCRDSLC'.                                                  
         05  LIT-CARDDTLTRANID                      PIC X(4)                    
             VALUE 'CCDL'.                                                      
         05  LIT-CARDDTLMAPSET                      PIC X(7)                    
             VALUE 'COCRDSL'.                                                   
         05  LIT-CARDDTLMAP                         PIC X(7)                    
             VALUE 'CCRDSLA'.                                                   
         05  LIT-CARDUPDPGM                         PIC X(8)                    
             VALUE 'COCRDUPC'.                                                  
         05  LIT-CARDUPDTRANID                      PIC X(4)                    
             VALUE 'CCUP'.                                                      
         05  LIT-CARDUPDMAPSET                      PIC X(7)                    
             VALUE 'COCRDUP'.                                                   
         05  LIT-CARDUPDMAP                         PIC X(7)                    
             VALUE 'CCRDUPA'.                                                   
                                                                                
                                                                                
         05  LIT-CARD-FILE                          PIC X(8)                    
                                                   VALUE 'CARDDAT '.            
         05  LIT-CARD-FILE-ACCT-PATH                PIC X(8)                    
                                                                                
                                                   VALUE 'CARDAIX '.            
      ******************************************************************        
      *Other common working storage Variables                                   
      ******************************************************************        
       COPY CVCRD01Y.                                                           
                                                                                
      ******************************************************************        
      *  Commarea manipulations                                                 
      ******************************************************************        
      *Application Commmarea Copybook                                           
       COPY COCOM01Y.                                                           
                                                                                
       01 WS-THIS-PROGCOMMAREA.                                                 
            10 WS-CA-LAST-CARDKEY.                                              
               15  WS-CA-LAST-CARD-NUM                PIC X(16).                
               15  WS-CA-LAST-CARD-ACCT-ID            PIC 9(11).                
            10 WS-CA-FIRST-CARDKEY.                                             
               15  WS-CA-FIRST-CARD-NUM               PIC X(16).                
               15  WS-CA-FIRST-CARD-ACCT-ID           PIC 9(11).                
                                                                                
            10 WS-CA-SCREEN-NUM                       PIC 9(1).                 
               88 CA-FIRST-PAGE                          VALUE 1.               
            10 WS-CA-LAST-PAGE-DISPLAYED              PIC 9(1).                 
               88 CA-LAST-PAGE-SHOWN                     VALUE 0.               
               88 CA-LAST-PAGE-NOT-SHOWN                 VALUE 9.               
            10 WS-CA-NEXT-PAGE-IND                    PIC X(1).                 
               88 CA-NEXT-PAGE-NOT-EXISTS             VALUE LOW-VALUES.         
               88 CA-NEXT-PAGE-EXISTS                 VALUE 'Y'.                
                                                                                
            10 WS-RETURN-FLAG                        PIC X(1).                  
           88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.            
           88  WS-RETURN-FLAG-ON                   VALUE '1'.                   
      ******************************************************************        
      *  File Data Array         28 CHARS X 7 ROWS = 196                        
      ******************************************************************        
         05 WS-SCREEN-DATA.                                                     
            10 WS-ALL-ROWS                         PIC X(196).                  
            10 FILLER REDEFINES WS-ALL-ROWS.                                    
               15 WS-SCREEN-ROWS OCCURS  7 TIMES.                               
                  20 WS-EACH-ROW.                                               
                     25 WS-EACH-CARD.                                           
                        30 WS-ROW-ACCTNO           PIC X(11).                   
                        30 WS-ROW-CARD-NUM         PIC X(16).                   
                        30 WS-ROW-CARD-STATUS      PIC X(1).                    
                                                                                
       01  WS-COMMAREA                             PIC X(2000).                 
                                                                                
                                                                                
                                                                                
      *IBM SUPPLIED COPYBOOKS                                                   
       COPY DFHBMSCA.                                                           
       COPY DFHAID.                                                             
                                                                                
      *COMMON COPYBOOKS                                                         
      *Screen Titles                                                            
       COPY COTTL01Y.                                                           
      *Credit Card Search Screen Layout                                         
      *COPY COCRDSL.                                                            
      *Credit Card List Screen Layout                                           
       COPY COCRDLI.                                                            
                                                                                
      *Current Date                                                             
       COPY CSDAT01Y.                                                           
      *Common Messages                                                          
       COPY CSMSG01Y.                                                           
      *Abend Variables                                                          
      *COPY CSMSG02Y.                                                           
      *Signed on user data                                                      
       COPY CSUSR01Y.                                                           
                                                                                
      *Dataset layouts                                                          
                                                                                
      *CARD RECORD LAYOUT                                                       
       COPY CVACT02Y.                                                           
                                                                                
       LINKAGE SECTION.                                                         
       01  DFHCOMMAREA.                                                         
         05  FILLER                                PIC X(1)                     
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.                     
                                                                                
       PROCEDURE DIVISION.                                                      
       0000-MAIN.                                                               
                                                                                
           INITIALIZE CC-WORK-AREA                                              
                      WS-MISC-STORAGE                                           
                      WS-COMMAREA                                               
                                                                                
      *****************************************************************         
      * Store our context                                                       
      *****************************************************************         
           MOVE LIT-THISTRANID       TO WS-TRANID                               
      *****************************************************************         
      * Ensure error message is cleared                               *         
      *****************************************************************         
           SET WS-ERROR-MSG-OFF  TO TRUE                                        
      *****************************************************************         
      * Retrived passed data if  any. Initialize them if first run.             
      *****************************************************************         
           IF EIBCALEN = 0
              INITIALIZE CARDDEMO-COMMAREA
                         WS-THIS-PROGCOMMAREA 
              MOVE LIT-THISTRANID        TO CDEMO-FROM-TRANID                   
              MOVE LIT-THISPGM           TO CDEMO-FROM-PROGRAM                  
              SET CDEMO-USRTYP-USER      TO TRUE                                
              SET CDEMO-PGM-ENTER        TO TRUE                                
              MOVE LIT-THISMAP           TO CDEMO-LAST-MAP                      
              MOVE LIT-THISMAPSET        TO CDEMO-LAST-MAPSET                   
              SET CA-FIRST-PAGE          TO TRUE                                
              SET CA-LAST-PAGE-NOT-SHOWN TO TRUE                                
           ELSE
              MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA) TO               
                                CARDDEMO-COMMAREA                               
              MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:                 
                               LENGTH OF WS-THIS-PROGCOMMAREA )TO               
                                WS-THIS-PROGCOMMAREA                            
           END-IF                                                               
      *****************************************************************         
      * If coming in from menu. Lets forget the past and start afresh *         
      *****************************************************************         
           IF (CDEMO-PGM-ENTER                                                  
           AND CDEMO-FROM-PROGRAM NOT EQUAL LIT-THISPGM)                        
               INITIALIZE WS-THIS-PROGCOMMAREA
               SET CDEMO-PGM-ENTER      TO TRUE                                 
               MOVE LIT-THISMAP         TO CDEMO-LAST-MAP                       
               SET CA-FIRST-PAGE        TO TRUE                                 
               SET CA-LAST-PAGE-NOT-SHOWN TO TRUE                               
           END-IF 
                                                                                
      ******************************************************************        
      * Remap PFkeys as needed.                                                 
      * Store the Mapped PF Key                                                 
      *****************************************************************         
           PERFORM YYYY-STORE-PFKEY                                             
              THRU YYYY-STORE-PFKEY-EXIT                                        
                                                                                
      ******************************************************************        
      * If something is present in commarea                                     
      * and the from program is this program itself,                            
      * read and edit the inputs given                                          
      *****************************************************************         
           IF  EIBCALEN > 0                                                     
           AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM                            
               PERFORM 2000-RECEIVE-MAP                                         
               THRU    2000-RECEIVE-MAP-EXIT                                    
                                                                                
           END-IF                                                               
      *****************************************************************         
      * Check the mapped key  to see if its valid at this point       *         
      * F3    - Exit                                                            
      * Enter - List of cards for current start key                             
      * F8    - Page down                                                       
      * F7    - Page up                                                         
      *****************************************************************         
           SET PFK-INVALID TO TRUE                                              
           IF CCARD-AID-ENTER OR                                                
              CCARD-AID-PFK03 OR                                                
              CCARD-AID-PFK07 OR                                                
              CCARD-AID-PFK08                                                   
               SET PFK-VALID TO TRUE                                            
           END-IF                                                               
                                                                                
           IF PFK-INVALID                                                       
              SET CCARD-AID-ENTER TO TRUE                                       
           END-IF                                                               
      *****************************************************************         
      * If the user pressed PF3 go back to main menu                            
      *****************************************************************         
           IF  (CCARD-AID-PFK03                                                 
           AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM)                           
              MOVE LIT-THISTRANID   TO CDEMO-FROM-TRANID                        
              MOVE LIT-THISPGM      TO CDEMO-FROM-PROGRAM                       
              SET  CDEMO-USRTYP-USER TO TRUE                                    
              SET  CDEMO-PGM-ENTER  TO TRUE                                     
              MOVE LIT-THISMAPSET   TO CDEMO-LAST-MAPSET                        
              MOVE LIT-THISMAP      TO CDEMO-LAST-MAP                           
              MOVE LIT-MENUPGM      TO CDEMO-TO-PROGRAM                         
                                                                                
              MOVE LIT-MENUMAPSET   TO CCARD-NEXT-MAPSET                        
              MOVE LIT-THISMAP      TO CCARD-NEXT-MAP                           
              SET WS-EXIT-MESSAGE            TO TRUE                            
                                                                                
      *       CALL MENU PROGRAM                                                 
      *                                                                         
              SET CDEMO-PGM-ENTER   TO TRUE                                     
      *                                                                         
              EXEC CICS XCTL                                                    
                        PROGRAM (LIT-MENUPGM)                                   
                        COMMAREA(CARDDEMO-COMMAREA)                             
              END-EXEC                                                          
           END-IF                                                               
      *****************************************************************         
      * If the user did not press PF8, lets reset the last page flag            
      *****************************************************************         
           IF CCARD-AID-PFK08                                                   
              CONTINUE                                                          
           ELSE                                                                 
              SET CA-LAST-PAGE-NOT-SHOWN   TO TRUE                              
           END-IF                                                               
      *****************************************************************         
      * Now we decide what to do                                                
      *****************************************************************         
           EVALUATE TRUE                                                        
               WHEN INPUT-ERROR                                                 
      *****************************************************************         
      *        ASK FOR CORRECTIONS TO INPUTS                                    
      *****************************************************************         
                    MOVE WS-ERROR-MSG    TO CCARD-ERROR-MSG                     
                    MOVE LIT-THISPGM     TO CDEMO-FROM-PROGRAM                  
                    MOVE LIT-THISMAPSET  TO CDEMO-LAST-MAPSET                   
                    MOVE LIT-THISMAP     TO CDEMO-LAST-MAP                      
                                                                                
                    MOVE LIT-THISPGM     TO CCARD-NEXT-PROG                     
                    MOVE LIT-THISMAPSET  TO CCARD-NEXT-MAPSET                   
                    MOVE LIT-THISMAP     TO CCARD-NEXT-MAP                      
                    IF  NOT FLG-ACCTFILTER-NOT-OK                               
                    AND NOT FLG-CARDFILTER-NOT-OK                               
                       PERFORM 9000-READ-FORWARD                                
                          THRU 9000-READ-FORWARD-EXIT                           
                    END-IF                                                      
                    PERFORM 1000-SEND-MAP                                       
                       THRU 1000-SEND-MAP                                       
                    GO TO COMMON-RETURN                                         
               WHEN CCARD-AID-PFK07                                             
                    AND CA-FIRST-PAGE                                           
      *****************************************************************         
      *        PAGE UP - PF7 - BUT ALREADY ON FIRST PAGE                        
      *****************************************************************         
               WHEN CCARD-AID-PFK07                                             
                    AND CA-FIRST-PAGE                                           
                    MOVE WS-CA-FIRST-CARD-NUM                                   
                                  TO WS-CARD-RID-CARDNUM                        
      *             MOVE WS-CA-FIRST-CARD-ACCT-ID                               
      *                           TO WS-CARD-RID-ACCT-ID                        
                    PERFORM 9000-READ-FORWARD                                   
                       THRU 9000-READ-FORWARD-EXIT                              
                    PERFORM 1000-SEND-MAP                                       
                       THRU 1000-SEND-MAP                                       
                    GO TO COMMON-RETURN                                         
      *****************************************************************         
      *        BACK - PF3 IF WE CAME FROM SOME OTHER PROGRAM                    
      *****************************************************************         
               WHEN CCARD-AID-PFK03                                             
               WHEN CDEMO-PGM-REENTER AND                                       
                    CDEMO-FROM-PROGRAM NOT EQUAL LIT-THISPGM                    
                                                                                
                    INITIALIZE CARDDEMO-COMMAREA                                
                               WS-THIS-PROGCOMMAREA                             
                    MOVE LIT-THISTRANID      TO CDEMO-FROM-TRANID               
                    MOVE LIT-THISPGM         TO CDEMO-FROM-PROGRAM              
                    SET CDEMO-USRTYP-USER    TO TRUE                            
                    SET CDEMO-PGM-ENTER      TO TRUE                            
                    MOVE LIT-THISMAP         TO CDEMO-LAST-MAP                  
                    MOVE LIT-THISMAPSET      TO CDEMO-LAST-MAPSET               
                    SET CA-FIRST-PAGE        TO TRUE                            
                    SET CA-LAST-PAGE-NOT-SHOWN TO TRUE                          
                                                                                
                    MOVE WS-CA-FIRST-CARD-NUM                                   
                                  TO WS-CARD-RID-CARDNUM                        
      *             MOVE WS-CA-FIRST-CARD-ACCT-ID                               
      *                           TO WS-CARD-RID-ACCT-ID                        
                                                                                
                    PERFORM 9000-READ-FORWARD                                   
                       THRU 9000-READ-FORWARD-EXIT                              
                    PERFORM 1000-SEND-MAP                                       
                       THRU 1000-SEND-MAP                                       
                    GO TO COMMON-RETURN                                         
      *****************************************************************         
      *        PAGE DOWN                                                        
      *****************************************************************         
               WHEN CCARD-AID-PFK08                                             
                    AND CA-NEXT-PAGE-EXISTS                                     
                    MOVE WS-CA-LAST-CARD-NUM                                    
                                  TO WS-CARD-RID-CARDNUM                        
      *             MOVE WS-CA-LAST-CARD-ACCT-ID                                
      *                           TO WS-CARD-RID-ACCT-ID                        
                    ADD   +1       TO WS-CA-SCREEN-NUM                          
                    PERFORM 9000-READ-FORWARD                                   
                       THRU 9000-READ-FORWARD-EXIT                              
                    PERFORM 1000-SEND-MAP                                       
                       THRU 1000-SEND-MAP-EXIT                                  
                    GO TO COMMON-RETURN                                         
      *****************************************************************         
      *        PAGE UP                                                          
      *****************************************************************         
               WHEN CCARD-AID-PFK07                                             
                    AND NOT CA-FIRST-PAGE                                       
                                                                                
                    MOVE WS-CA-FIRST-CARD-NUM                                   
                                  TO WS-CARD-RID-CARDNUM                        
      *             MOVE WS-CA-FIRST-CARD-ACCT-ID                               
      *                           TO WS-CARD-RID-ACCT-ID                        
                    SUBTRACT 1    FROM WS-CA-SCREEN-NUM                         
                    PERFORM 9100-READ-BACKWARDS                                 
                       THRU 9100-READ-BACKWARDS-EXIT                            
                    PERFORM 1000-SEND-MAP                                       
                       THRU 1000-SEND-MAP-EXIT                                  
                    GO TO COMMON-RETURN                                         
      *****************************************************************         
      *        TRANSFER TO CARD DETAIL VIEW                                     
      *****************************************************************         
               WHEN CCARD-AID-ENTER                                             
                AND VIEW-REQUESTED-ON(I-SELECTED)                               
                AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM                       
                   MOVE LIT-THISTRANID    TO CDEMO-FROM-TRANID                  
                   MOVE LIT-THISPGM       TO CDEMO-FROM-PROGRAM                 
                   SET  CDEMO-USRTYP-USER TO TRUE                               
                   SET  CDEMO-PGM-ENTER   TO TRUE                               
                   MOVE LIT-THISMAPSET    TO CDEMO-LAST-MAPSET                  
                   MOVE LIT-THISMAP       TO CDEMO-LAST-MAP                     
                   MOVE LIT-CARDDTLPGM    TO CCARD-NEXT-PROG                    
                                                                                
                   MOVE LIT-CARDDTLMAPSET TO CCARD-NEXT-MAPSET                  
                   MOVE LIT-CARDDTLMAP    TO CCARD-NEXT-MAP                     
                                                                                
                   MOVE WS-ROW-ACCTNO (I-SELECTED)                              
                                          TO CDEMO-ACCT-ID                      
                   MOVE WS-ROW-CARD-NUM (I-SELECTED)                            
                                          TO CDEMO-CARD-NUM                     
                                                                                
      *            CALL CARD DETAIL PROGRAM                                     
      *                                                                         
                   EXEC CICS XCTL                                               
                        PROGRAM (CCARD-NEXT-PROG)                               
                        COMMAREA(CARDDEMO-COMMAREA)                             
                   END-EXEC                                                     
      *****************************************************************         
      *        TRANSFER TO CARD UPDATED PROGRAM                                 
      *****************************************************************         
               WHEN CCARD-AID-ENTER                                             
                AND UPDATE-REQUESTED-ON(I-SELECTED)                             
                AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM                       
                   MOVE LIT-THISTRANID    TO CDEMO-FROM-TRANID                  
                   MOVE LIT-THISPGM       TO CDEMO-FROM-PROGRAM                 
                   SET  CDEMO-USRTYP-USER TO TRUE                               
                   SET  CDEMO-PGM-ENTER   TO TRUE                               
                   MOVE LIT-THISMAPSET    TO CDEMO-LAST-MAPSET                  
                   MOVE LIT-THISMAP       TO CDEMO-LAST-MAP                     
                   MOVE LIT-CARDUPDPGM    TO CCARD-NEXT-PROG                    
                                                                                
                   MOVE LIT-CARDUPDMAPSET TO CCARD-NEXT-MAPSET                  
                   MOVE LIT-CARDUPDMAP    TO CCARD-NEXT-MAP                     
                                                                                
                   MOVE WS-ROW-ACCTNO (I-SELECTED)                              
                                          TO CDEMO-ACCT-ID                      
                   MOVE WS-ROW-CARD-NUM (I-SELECTED)                            
                                          TO CDEMO-CARD-NUM                     
                                                                                
      *            CALL CARD UPDATE PROGRAM                                     
      *                                                                         
                   EXEC CICS XCTL                                               
                        PROGRAM (CCARD-NEXT-PROG)                               
                        COMMAREA(CARDDEMO-COMMAREA)                             
                   END-EXEC                                                     
                                                                                
      *****************************************************************         
               WHEN OTHER                                                       
      *****************************************************************         
                    MOVE WS-CA-FIRST-CARD-NUM                                   
                                  TO WS-CARD-RID-CARDNUM                        
      *             MOVE WS-CA-FIRST-CARD-ACCT-ID                               
      *                           TO WS-CARD-RID-ACCT-ID                        
                    PERFORM 9000-READ-FORWARD                                   
                       THRU 9000-READ-FORWARD-EXIT                              
                    PERFORM 1000-SEND-MAP                                       
                       THRU 1000-SEND-MAP                                       
                    GO TO COMMON-RETURN                                         
           END-EVALUATE                                                         
                                                                                
      * If we had an error setup error message to display and return            
           IF INPUT-ERROR                                                       
              MOVE WS-ERROR-MSG   TO CCARD-ERROR-MSG                            
              MOVE LIT-THISPGM     TO CDEMO-FROM-PROGRAM                        
              MOVE LIT-THISMAPSET  TO CDEMO-LAST-MAPSET                         
              MOVE LIT-THISMAP     TO CDEMO-LAST-MAP                            
                                                                                
              MOVE LIT-THISPGM     TO CCARD-NEXT-PROG                           
              MOVE LIT-THISMAPSET  TO CCARD-NEXT-MAPSET                         
              MOVE LIT-THISMAP     TO CCARD-NEXT-MAP                            
      *       PERFORM 1000-SEND-MAP                                             
      *          THRU 1000-SEND-MAP                                             
              GO TO COMMON-RETURN                                               
           END-IF                                                               
                                                                                
           MOVE LIT-THISPGM        TO CCARD-NEXT-PROG                           
           GO TO COMMON-RETURN                                                  
           .                                                                    
                                                                                
       COMMON-RETURN.                                                           
           MOVE  LIT-THISTRANID TO CDEMO-FROM-TRANID                            
           MOVE  LIT-THISPGM     TO CDEMO-FROM-PROGRAM                          
           MOVE  LIT-THISMAPSET  TO CDEMO-LAST-MAPSET                           
           MOVE  LIT-THISMAP     TO CDEMO-LAST-MAP                              
           MOVE  CARDDEMO-COMMAREA    TO WS-COMMAREA                            
           MOVE  WS-THIS-PROGCOMMAREA TO                                        
                  WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:                  
                               LENGTH OF WS-THIS-PROGCOMMAREA )                 
                                                                                
                                                                                
           EXEC CICS RETURN                                                     
                TRANSID (LIT-THISTRANID)                                        
                COMMAREA (WS-COMMAREA)                                          
                LENGTH(LENGTH OF WS-COMMAREA)                                   
           END-EXEC                                                             
           .                                                                    
       0000-MAIN-EXIT.                                                          
           EXIT                                                                 
           .                                                                    
       1000-SEND-MAP.                                                           
           PERFORM 1100-SCREEN-INIT                                             
              THRU 1100-SCREEN-INIT-EXIT                                        
           PERFORM 1200-SCREEN-ARRAY-INIT                                       
              THRU 1200-SCREEN-ARRAY-INIT-EXIT                                  
           PERFORM 1250-SETUP-ARRAY-ATTRIBS                                     
              THRU 1250-SETUP-ARRAY-ATTRIBS-EXIT                                
           PERFORM 1300-SETUP-SCREEN-ATTRS                                      
              THRU 1300-SETUP-SCREEN-ATTRS-EXIT                                 
           PERFORM 1400-SETUP-MESSAGE                                           
              THRU 1400-SETUP-MESSAGE-EXIT                                      
           PERFORM 1500-SEND-SCREEN                                             
              THRU 1500-SEND-SCREEN-EXIT                                        
           .                                                                    
                                                                                
       1000-SEND-MAP-EXIT.                                                      
           EXIT                                                                 
           .                                                                    
       1100-SCREEN-INIT.                                                        
           MOVE LOW-VALUES             TO CCRDLIAO                              
                                                                                
           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA                       
                                                                                
           MOVE CCDA-TITLE01           TO TITLE01O OF CCRDLIAO                  
           MOVE CCDA-TITLE02           TO TITLE02O OF CCRDLIAO                  
           MOVE LIT-THISTRANID         TO TRNNAMEO OF CCRDLIAO                  
           MOVE LIT-THISPGM            TO PGMNAMEO OF CCRDLIAO                  
                                                                                
           MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA                       
                                                                                
           MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM                         
           MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD                         
           MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY                         
                                                                                
           MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF CCRDLIAO                  
                                                                                
           MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH                         
           MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM                         
           MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS                         
                                                                                
           MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF CCRDLIAO                  
      *    PAGE NUMBER                                                          
      *                                                                         
           MOVE WS-CA-SCREEN-NUM       TO PAGENOO  OF CCRDLIAO                  
                                                                                
           SET WS-NO-INFO-MESSAGE      TO TRUE                                  
           MOVE WS-INFO-MSG            TO INFOMSGO OF CCRDLIAO                  
           MOVE DFHBMDAR               TO INFOMSGC OF CCRDLIAO                  
           .                                                                    
                                                                                
       1100-SCREEN-INIT-EXIT.                                                   
           EXIT                                                                 
           .                                                                    
                                                                                
       1200-SCREEN-ARRAY-INIT.                                                  
      *    USE REDEFINES AND CLEAN UP REPETITIVE CODE !!                        
           IF   WS-EACH-CARD(1)            EQUAL LOW-VALUES                     
              CONTINUE                                                          
           ELSE                                                                 
              MOVE WS-EDIT-SELECT(1)       TO CRDSEL1O OF CCRDLIAO              
              MOVE WS-ROW-ACCTNO(1)        TO ACCTNO1O OF CCRDLIAO              
              MOVE WS-ROW-CARD-NUM(1)      TO CRDNUM1O OF CCRDLIAO              
              MOVE WS-ROW-CARD-STATUS(1)   TO CRDSTS1O OF CCRDLIAO              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(2)        EQUAL LOW-VALUES                         
              CONTINUE                                                          
           ELSE                                                                 
              MOVE WS-EDIT-SELECT(2)       TO CRDSEL2O OF CCRDLIAO              
              MOVE WS-ROW-ACCTNO(2)        TO ACCTNO2O OF CCRDLIAO              
              MOVE WS-ROW-CARD-NUM(2)      TO CRDNUM2O OF CCRDLIAO              
              MOVE WS-ROW-CARD-STATUS(2)   TO CRDSTS2O OF CCRDLIAO              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(3)        EQUAL LOW-VALUES                         
              CONTINUE                                                          
           ELSE                                                                 
              MOVE WS-EDIT-SELECT(3)       TO CRDSEL3O OF CCRDLIAO              
              MOVE WS-ROW-ACCTNO(3)        TO ACCTNO3O OF CCRDLIAO              
              MOVE WS-ROW-CARD-NUM(3)      TO CRDNUM3O OF CCRDLIAO              
              MOVE WS-ROW-CARD-STATUS(3)   TO CRDSTS3O OF CCRDLIAO              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(4)        EQUAL LOW-VALUES                         
              CONTINUE                                                          
           ELSE                                                                 
              MOVE WS-EDIT-SELECT(4)       TO CRDSEL4O OF CCRDLIAO              
              MOVE WS-ROW-ACCTNO(4)        TO ACCTNO4O OF CCRDLIAO              
              MOVE WS-ROW-CARD-NUM(4)      TO CRDNUM4O OF CCRDLIAO              
              MOVE WS-ROW-CARD-STATUS(4)   TO CRDSTS4O OF CCRDLIAO              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(5)        EQUAL LOW-VALUES                         
              CONTINUE                                                          
           ELSE                                                                 
              MOVE WS-EDIT-SELECT(5)       TO CRDSEL5O OF CCRDLIAO              
              MOVE WS-ROW-ACCTNO(5)        TO ACCTNO5O OF CCRDLIAO              
              MOVE WS-ROW-CARD-NUM(5)      TO CRDNUM5O OF CCRDLIAO              
              MOVE WS-ROW-CARD-STATUS(5)   TO CRDSTS5O OF CCRDLIAO              
           END-IF                                                               
                                                                                
                                                                                
           IF   WS-EACH-CARD(6)        EQUAL LOW-VALUES                         
              CONTINUE                                                          
           ELSE                                                                 
              MOVE WS-EDIT-SELECT(6)       TO CRDSEL6O OF CCRDLIAO              
              MOVE WS-ROW-ACCTNO(6)        TO ACCTNO6O OF CCRDLIAO              
              MOVE WS-ROW-CARD-NUM(6)      TO CRDNUM6O OF CCRDLIAO              
              MOVE WS-ROW-CARD-STATUS(6)   TO CRDSTS6O OF CCRDLIAO              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(7)        EQUAL LOW-VALUES                         
              CONTINUE                                                          
           ELSE                                                                 
              MOVE WS-EDIT-SELECT(7)       TO CRDSEL7O OF CCRDLIAO              
              MOVE WS-ROW-ACCTNO(7)        TO ACCTNO7O OF CCRDLIAO              
              MOVE WS-ROW-CARD-NUM(7)      TO CRDNUM7O OF CCRDLIAO              
              MOVE WS-ROW-CARD-STATUS(7)   TO CRDSTS7O OF CCRDLIAO              
           END-IF                                                               
           .                                                                    
                                                                                
       1200-SCREEN-ARRAY-INIT-EXIT.                                             
           EXIT                                                                 
           .                                                                    
       1250-SETUP-ARRAY-ATTRIBS.                                                
      *    USE REDEFINES AND CLEAN UP REPETITIVE CODE !!                        
                                                                                
           IF   WS-EACH-CARD(1)            EQUAL LOW-VALUES                     
           OR   FLG-PROTECT-SELECT-ROWS-YES                                     
              MOVE DFHBMPRF                TO CRDSEL1A OF CCRDLIAI              
           ELSE                                                                 
              IF WS-ROW-CRDSELECT-ERROR(1) = '1'                                
                 MOVE DFHRED               TO CRDSEL1C OF CCRDLIAO              
                 IF WS-EDIT-SELECT(1) = SPACE OR LOW-VALUES                     
                    MOVE '*'               TO CRDSEL1O OF CCRDLIAO              
                 END-IF                                                         
              END-IF                                                            
              MOVE DFHBMFSE                TO CRDSEL1A OF CCRDLIAI              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(2)            EQUAL LOW-VALUES                     
           OR   FLG-PROTECT-SELECT-ROWS-YES                                     
              MOVE DFHBMPRO                TO CRDSEL2A OF CCRDLIAI              
           ELSE                                                                 
              IF WS-ROW-CRDSELECT-ERROR(2) = '1'                                
                 MOVE DFHRED               TO CRDSEL2C OF CCRDLIAO              
                 MOVE -1                   TO CRDSEL2L OF CCRDLIAI              
              END-IF                                                            
              MOVE DFHBMFSE                TO CRDSEL2A OF CCRDLIAI              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(3)            EQUAL LOW-VALUES                     
           OR   FLG-PROTECT-SELECT-ROWS-YES                                     
              MOVE DFHBMPRO                TO CRDSEL3A OF CCRDLIAI              
                                                                                
           ELSE                                                                 
              IF WS-ROW-CRDSELECT-ERROR(3) = '1'                                
                 MOVE DFHRED               TO CRDSEL3C OF CCRDLIAO              
                 MOVE -1                   TO CRDSEL3L OF CCRDLIAI              
              END-IF                                                            
              MOVE DFHBMFSE                TO CRDSEL3A OF CCRDLIAI              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(4)            EQUAL LOW-VALUES                     
           OR   FLG-PROTECT-SELECT-ROWS-YES                                     
              MOVE DFHBMPRO                TO CRDSEL4A OF CCRDLIAI              
              I                                                                 
           ELSE                                                                 
              IF WS-ROW-CRDSELECT-ERROR(4) = '1'                                
                 MOVE DFHRED               TO CRDSEL4C OF CCRDLIAO              
                 MOVE -1                   TO CRDSEL4L OF CCRDLIAI              
              END-IF                                                            
              MOVE DFHBMFSE                TO CRDSEL4A OF CCRDLIAI              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(5)            EQUAL LOW-VALUES                     
           OR   FLG-PROTECT-SELECT-ROWS-YES                                     
              MOVE DFHBMPRO                TO CRDSEL5A OF CCRDLIAI              
           ELSE                                                                 
              IF WS-ROW-CRDSELECT-ERROR(5) = '1'                                
                 MOVE DFHRED               TO CRDSEL5C OF CCRDLIAO              
                 MOVE -1                   TO CRDSEL5L OF CCRDLIAI              
              END-IF                                                            
              MOVE DFHBMFSE                TO CRDSEL5A OF CCRDLIAI              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(6)            EQUAL LOW-VALUES                     
           OR   FLG-PROTECT-SELECT-ROWS-YES                                     
              MOVE DFHBMPRO                TO CRDSEL6A OF CCRDLIAI              
                                                                                
           ELSE                                                                 
              IF WS-ROW-CRDSELECT-ERROR(6) = '1'                                
                 MOVE DFHRED               TO CRDSEL6C OF CCRDLIAO              
                 MOVE -1                   TO CRDSEL6L OF CCRDLIAI              
              END-IF                                                            
              MOVE DFHBMFSE                TO CRDSEL6A OF CCRDLIAI              
           END-IF                                                               
                                                                                
           IF   WS-EACH-CARD(7)            EQUAL LOW-VALUES                     
           OR   FLG-PROTECT-SELECT-ROWS-YES                                     
              MOVE DFHBMPRO                TO CRDSEL7A OF CCRDLIAI              
           ELSE                                                                 
              IF WS-ROW-CRDSELECT-ERROR(7) = '1'                                
                 MOVE DFHRED               TO CRDSEL7C OF CCRDLIAO              
                 MOVE -1                   TO CRDSEL7L OF CCRDLIAI              
              END-IF                                                            
              MOVE DFHBMFSE                TO CRDSEL7A OF CCRDLIAI              
           END-IF                                                               
           .                                                                    
                                                                                
       1250-SETUP-ARRAY-ATTRIBS-EXIT.                                           
           EXIT                                                                 
           .                                                                    
       1300-SETUP-SCREEN-ATTRS.                                                 
      *    INITIALIZE SEARCH CRITERIA                                           
           IF EIBCALEN = 0                                                      
           OR (CDEMO-PGM-ENTER                                                  
           AND CDEMO-FROM-PROGRAM = LIT-MENUPGM)                                
              CONTINUE                                                          
           ELSE                                                                 
              EVALUATE TRUE                                                     
                  WHEN FLG-ACCTFILTER-ISVALID                                   
                  WHEN FLG-ACCTFILTER-NOT-OK                                    
                     MOVE CC-ACCT-ID   TO ACCTSIDO OF CCRDLIAO                  
                     MOVE DFHBMFSE     TO ACCTSIDA OF CCRDLIAI                  
                  WHEN CDEMO-ACCT-ID = 0                                        
                     MOVE LOW-VALUES   TO ACCTSIDO OF CCRDLIAO                  
                  WHEN OTHER                                                    
                    MOVE CDEMO-ACCT-ID TO ACCTSIDO OF CCRDLIAO                  
                    MOVE DFHBMFSE      TO ACCTSIDA OF CCRDLIAI                  
              END-EVALUATE                                                      
                                                                                
              EVALUATE TRUE                                                     
                  WHEN FLG-CARDFILTER-ISVALID                                   
                  WHEN FLG-CARDFILTER-NOT-OK                                    
                     MOVE CC-CARD-NUM  TO CARDSIDO OF CCRDLIAO                  
                     MOVE DFHBMFSE     TO CARDSIDA OF CCRDLIAI                  
                  WHEN CDEMO-CARD-NUM = 0                                       
                     MOVE LOW-VALUES   TO CARDSIDO OF CCRDLIAO                  
                  WHEN OTHER                                                    
                    MOVE CDEMO-CARD-NUM                                         
                                       TO CARDSIDO OF CCRDLIAO                  
                    MOVE DFHBMFSE      TO CARDSIDA OF CCRDLIAI                  
              END-EVALUATE                                                      
           END-IF                                                               
                                                                                
      *    POSITION CURSOR                                                      
                                                                                
           IF FLG-ACCTFILTER-NOT-OK                                             
              MOVE  DFHRED             TO ACCTSIDC OF CCRDLIAO                  
              MOVE  -1                 TO ACCTSIDL OF CCRDLIAI                  
           END-IF                                                               
                                                                                
           IF FLG-CARDFILTER-NOT-OK                                             
              MOVE  DFHRED             TO CARDSIDC OF CCRDLIAO                  
              MOVE  -1                 TO CARDSIDL OF CCRDLIAI                  
           END-IF                                                               
                                                                                
      *    IF NO ERRORS POSITION CURSOR AT ACCTID                               
                                                                                
           IF INPUT-OK                                                          
             MOVE   -1                 TO ACCTSIDL OF CCRDLIAI                  
           END-IF                                                               
                                                                                
                                                                                
           .                                                                    
       1300-SETUP-SCREEN-ATTRS-EXIT.                                            
           EXIT                                                                 
           .                                                                    
                                                                                
                                                                                
       1400-SETUP-MESSAGE.                                                      
      *    SETUP MESSAGE                                                        
           EVALUATE TRUE                                                        
                WHEN FLG-ACCTFILTER-NOT-OK                                      
                WHEN FLG-CARDFILTER-NOT-OK                                      
                  CONTINUE                                                      
                WHEN CCARD-AID-PFK07                                            
                    AND CA-FIRST-PAGE                                           
                  MOVE 'NO PREVIOUS PAGES TO DISPLAY'                           
                  TO WS-ERROR-MSG                                               
                WHEN CCARD-AID-PFK08                                            
                 AND CA-NEXT-PAGE-NOT-EXISTS                                    
                 AND CA-LAST-PAGE-SHOWN                                         
                  MOVE 'NO MORE PAGES TO DISPLAY'                               
                  TO WS-ERROR-MSG                                               
                WHEN CCARD-AID-PFK08                                            
                 AND CA-NEXT-PAGE-NOT-EXISTS                                    
                  SET WS-INFORM-REC-ACTIONS TO TRUE                             
                  IF  CA-LAST-PAGE-NOT-SHOWN                                    
                  AND CA-NEXT-PAGE-NOT-EXISTS                                   
                      SET CA-LAST-PAGE-SHOWN TO TRUE                            
                  END-IF                                                        
                WHEN WS-NO-INFO-MESSAGE                                         
                WHEN CA-NEXT-PAGE-EXISTS                                        
                  SET WS-INFORM-REC-ACTIONS TO TRUE                             
                WHEN OTHER                                                      
                   SET WS-NO-INFO-MESSAGE TO TRUE                               
           END-EVALUATE                                                         
                                                                                
           MOVE WS-ERROR-MSG          TO ERRMSGO OF CCRDLIAO                    
                                                                                
           IF  NOT WS-NO-INFO-MESSAGE                                           
           AND NOT WS-NO-RECORDS-FOUND                                          
              MOVE WS-INFO-MSG        TO INFOMSGO OF CCRDLIAO                   
              MOVE DFHNEUTR           TO INFOMSGC OF CCRDLIAO                   
           END-IF                                                               
                                                                                
           .                                                                    
       1400-SETUP-MESSAGE-EXIT.                                                 
           EXIT                                                                 
           .                                                                    
                                                                                
                                                                                
       1500-SEND-SCREEN.                                                        
           EXEC CICS SEND MAP(LIT-THISMAP)                                      
                          MAPSET(LIT-THISMAPSET)                                
                          FROM(CCRDLIAO)                                        
                          CURSOR                                                
                          ERASE                                                 
                          RESP(WS-RESP-CD)                                      
                          FREEKB                                                
           END-EXEC                                                             
           .                                                                    
       1500-SEND-SCREEN-EXIT.                                                   
           EXIT                                                                 
           .                                                                    
       2000-RECEIVE-MAP.                                                        
           PERFORM 2100-RECEIVE-SCREEN                                          
              THRU 2100-RECEIVE-SCREEN-EXIT                                     
                                                                                
           PERFORM 2200-EDIT-INPUTS                                             
            THRU   2200-EDIT-INPUTS-EXIT                                        
           .                                                                    
                                                                                
       2000-RECEIVE-MAP-EXIT.                                                   
           EXIT                                                                 
           .                                                                    
       2100-RECEIVE-SCREEN.                                                     
           EXEC CICS RECEIVE MAP(LIT-THISMAP)                                   
                          MAPSET(LIT-THISMAPSET)                                
                          INTO(CCRDLIAI)                                        
                          RESP(WS-RESP-CD)                                      
           END-EXEC                                                             
                                                                                
           MOVE ACCTSIDI OF CCRDLIAI  TO CC-ACCT-ID                             
           MOVE CARDSIDI OF CCRDLIAI  TO CC-CARD-NUM                            
                                                                                
           MOVE CRDSEL1I OF CCRDLIAI  TO WS-EDIT-SELECT(1)                      
           MOVE CRDSEL2I OF CCRDLIAI  TO WS-EDIT-SELECT(2)                      
           MOVE CRDSEL3I OF CCRDLIAI  TO WS-EDIT-SELECT(3)                      
           MOVE CRDSEL4I OF CCRDLIAI  TO WS-EDIT-SELECT(4)                      
           MOVE CRDSEL5I OF CCRDLIAI  TO WS-EDIT-SELECT(5)                      
           MOVE CRDSEL6I OF CCRDLIAI  TO WS-EDIT-SELECT(6)                      
           MOVE CRDSEL7I OF CCRDLIAI  TO WS-EDIT-SELECT(7)                      
           .                                                                    
                                                                                
       2100-RECEIVE-SCREEN-EXIT.                                                
           EXIT                                                                 
           .                                                                    
                                                                                
       2200-EDIT-INPUTS.                                                        
           SET INPUT-OK                   TO TRUE                               
           SET FLG-PROTECT-SELECT-ROWS-NO TO TRUE                               
                                                                                
           PERFORM 2210-EDIT-ACCOUNT                                            
              THRU 2210-EDIT-ACCOUNT-EXIT                                       
                                                                                
           PERFORM 2220-EDIT-CARD                                               
              THRU 2220-EDIT-CARD-EXIT                                          
                                                                                
           PERFORM 2250-EDIT-ARRAY                                              
              THRU 2250-EDIT-ARRAY-EXIT                                         
           .                                                                    
                                                                                
       2200-EDIT-INPUTS-EXIT.                                                   
           EXIT                                                                 
           .                                                                    
                                                                                
       2210-EDIT-ACCOUNT.                                                       
           SET FLG-ACCTFILTER-BLANK TO TRUE                                     
                                                                                
      *    Not supplied                                                         
           IF CC-ACCT-ID   EQUAL LOW-VALUES                                     
           OR CC-ACCT-ID   EQUAL SPACES                                         
           OR CC-ACCT-ID-N EQUAL ZEROS                                          
              SET FLG-ACCTFILTER-BLANK  TO TRUE                                 
              MOVE ZEROES       TO CDEMO-ACCT-ID                                
              GO TO  2210-EDIT-ACCOUNT-EXIT                                     
           END-IF                                                               
      *                                                                         
      *    Not numeric                                                          
      *    Not 11 characters                                                    
           IF CC-ACCT-ID  IS NOT NUMERIC                                        
              SET INPUT-ERROR TO TRUE                                           
              SET FLG-ACCTFILTER-NOT-OK TO TRUE                                 
              SET FLG-PROTECT-SELECT-ROWS-YES TO TRUE                           
              MOVE                                                              
              'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'            
                              TO WS-ERROR-MSG                                   
              MOVE ZERO       TO CDEMO-ACCT-ID                                  
              GO TO 2210-EDIT-ACCOUNT-EXIT                                      
           ELSE                                                                 
              MOVE CC-ACCT-ID TO CDEMO-ACCT-ID                                  
              SET FLG-ACCTFILTER-ISVALID TO TRUE                                
           END-IF                                                               
           .                                                                    
                                                                                
       2210-EDIT-ACCOUNT-EXIT.                                                  
           EXIT                                                                 
           .                                                                    
                                                                                
       2220-EDIT-CARD.                                                          
      *    Not numeric                                                          
      *    Not 16 characters                                                    
           SET FLG-CARDFILTER-BLANK TO TRUE                                     
                                                                                
      *    Not supplied                                                         
           IF CC-CARD-NUM   EQUAL LOW-VALUES                                    
           OR CC-CARD-NUM   EQUAL SPACES                                        
           OR CC-CARD-NUM-N EQUAL ZEROS                                         
              SET FLG-CARDFILTER-BLANK  TO TRUE                                 
              MOVE ZEROES       TO CDEMO-CARD-NUM                               
              GO TO  2220-EDIT-CARD-EXIT                                        
           END-IF                                                               
      *                                                                         
      *    Not numeric                                                          
      *    Not 16 characters                                                    
           IF CC-CARD-NUM  IS NOT NUMERIC                                       
              SET INPUT-ERROR TO TRUE                                           
              SET FLG-CARDFILTER-NOT-OK TO TRUE                                 
              SET FLG-PROTECT-SELECT-ROWS-YES TO TRUE                           
              IF WS-ERROR-MSG-OFF                                               
                 MOVE                                                           
              'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'            
                              TO WS-ERROR-MSG                                   
              END-IF                                                            
              MOVE ZERO       TO CDEMO-CARD-NUM                                 
              GO TO 2220-EDIT-CARD-EXIT                                         
           ELSE                                                                 
              MOVE CC-CARD-NUM-N TO CDEMO-CARD-NUM                              
              SET FLG-CARDFILTER-ISVALID TO TRUE                                
           END-IF                                                               
           .                                                                    
                                                                                
       2220-EDIT-CARD-EXIT.                                                     
           EXIT                                                                 
           .                                                                    
                                                                                
       2250-EDIT-ARRAY.                                                         
                                                                                
           IF INPUT-ERROR                                                       
              GO TO 2250-EDIT-ARRAY-EXIT                                        
           END-IF                                                               
                                                                                
           INSPECT  WS-EDIT-SELECT-FLAGS                                        
           TALLYING I                                                           
           FOR ALL 'S'                                                          
               ALL 'U'                                                          
                                                                                
           IF I > +1                                                            
               SET INPUT-ERROR      TO TRUE                                     
               SET WS-MORE-THAN-1-ACTION TO TRUE                                
                                                                                
               MOVE WS-EDIT-SELECT-FLAGS                                        
                                   TO WS-EDIT-SELECT-ERROR-FLAGS                
               INSPECT WS-EDIT-SELECT-ERROR-FLAGS                               
                 REPLACING ALL 'S' BY '1'                                       
                           ALL 'U' BY '1'                                       
                 CHARACTERS        BY '0'                                       
                                                                                
           END-IF                                                               
                                                                                
           MOVE ZERO TO I-SELECTED                                              
                                                                                
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 7                            
               EVALUATE TRUE                                                    
                 WHEN SELECT-OK(I)                                              
                   MOVE I TO I-SELECTED                                         
                   IF WS-MORE-THAN-1-ACTION                                     
                      MOVE '1' TO WS-ROW-CRDSELECT-ERROR(I)                     
                   END-IF                                                       
                 WHEN SELECT-BLANK(I)                                           
                   CONTINUE                                                     
                 WHEN OTHER                                                     
                   SET INPUT-ERROR TO TRUE                                      
                   MOVE '1' TO WS-ROW-CRDSELECT-ERROR(I)                        
                   IF WS-ERROR-MSG-OFF                                          
                      SET WS-INVALID-ACTION-CODE TO TRUE                        
                   END-IF                                                       
              END-EVALUATE                                                      
           END-PERFORM                                                          
                                                                                
           .                                                                    
                                                                                
       2250-EDIT-ARRAY-EXIT.                                                    
           EXIT                                                                 
           .                                                                    
                                                                                
       9000-READ-FORWARD.                                                       
           MOVE LOW-VALUES           TO WS-ALL-ROWS                             
                                                                                
      *****************************************************************         
      *    Start Browse                                                         
      *****************************************************************         
           EXEC CICS STARTBR                                                    
                DATASET(LIT-CARD-FILE)                                          
                RIDFLD(WS-CARD-RID-CARDNUM)                                     
                KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)                        
                GTEQ                                                            
                RESP(WS-RESP-CD)                                                
                RESP2(WS-REAS-CD)                                               
           END-EXEC                                                             
      *****************************************************************         
      *    Loop through records and fetch max screen records                    
      *****************************************************************         
           MOVE ZEROES TO WS-SCRN-COUNTER                                       
           SET CA-NEXT-PAGE-EXISTS    TO TRUE                                   
           SET MORE-RECORDS-TO-READ   TO TRUE                                   
                                                                                
           PERFORM UNTIL READ-LOOP-EXIT                                         
                                                                                
           EXEC CICS READNEXT                                                   
                DATASET(LIT-CARD-FILE)                                          
                INTO (CARD-RECORD)                                              
                LENGTH(LENGTH OF CARD-RECORD)                                   
                RIDFLD(WS-CARD-RID-CARDNUM)                                     
                KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)                        
                RESP(WS-RESP-CD)                                                
                RESP2(WS-REAS-CD)                                               
           END-EXEC                                                             
                                                                                
           EVALUATE WS-RESP-CD                                                  
               WHEN DFHRESP(NORMAL)                                             
               WHEN DFHRESP(DUPREC)                                             
                   PERFORM 9500-FILTER-RECORDS                                  
                      THRU 9500-FILTER-RECORDS-EXIT                             
                                                                                
                   IF WS-DONOT-EXCLUDE-THIS-RECORD                              
                      ADD 1             TO WS-SCRN-COUNTER                      
                                                                                
                      MOVE CARD-NUM     TO WS-ROW-CARD-NUM(                     
                      WS-SCRN-COUNTER)                                          
                      MOVE CARD-ACCT-ID TO                                      
                      WS-ROW-ACCTNO(WS-SCRN-COUNTER)                            
                      MOVE CARD-ACTIVE-STATUS                                   
                                        TO WS-ROW-CARD-STATUS(                  
                                        WS-SCRN-COUNTER)                        
                                                                                
                      IF WS-SCRN-COUNTER = 1                                    
                         MOVE CARD-ACCT-ID                                      
                                        TO WS-CA-FIRST-CARD-ACCT-ID             
                         MOVE CARD-NUM  TO WS-CA-FIRST-CARD-NUM                 
                         IF   WS-CA-SCREEN-NUM = 0                              
                           ADD   +1     TO WS-CA-SCREEN-NUM                     
                         ELSE                                                   
                           CONTINUE                                             
                         END-IF                                                 
                      ELSE                                                      
                         CONTINUE                                               
                      END-IF                                                    
                   ELSE                                                         
                       CONTINUE                                                 
                   END-IF                                                       
      ******************************************************************        
      *            Max Screen size                                              
      ******************************************************************        
                   IF WS-SCRN-COUNTER = WS-MAX-SCREEN-LINES                     
                      SET READ-LOOP-EXIT  TO TRUE                               
                                                                                
                      MOVE CARD-ACCT-ID     TO WS-CA-LAST-CARD-ACCT-ID          
                      MOVE CARD-NUM         TO WS-CA-LAST-CARD-NUM              
                                                                                
                      EXEC CICS READNEXT                                        
                        DATASET(LIT-CARD-FILE)                                  
                        INTO (CARD-RECORD)                                      
                        LENGTH(LENGTH OF CARD-RECORD)                           
                        RIDFLD(WS-CARD-RID-CARDNUM)                             
                        KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)                
                        RESP(WS-RESP-CD)                                        
                        RESP2(WS-REAS-CD)                                       
                      END-EXEC                                                  
                                                                                
                      EVALUATE WS-RESP-CD                                       
                         WHEN DFHRESP(NORMAL)                                   
                         WHEN DFHRESP(DUPREC)                                   
                              SET CA-NEXT-PAGE-EXISTS                           
                                                TO TRUE                         
                              MOVE CARD-ACCT-ID TO                              
                                   WS-CA-LAST-CARD-ACCT-ID                      
                              MOVE CARD-NUM     TO WS-CA-LAST-CARD-NUM          
                        WHEN DFHRESP(ENDFILE)                                   
                            SET CA-NEXT-PAGE-NOT-EXISTS     TO TRUE             
                                                                                
                            IF WS-ERROR-MSG-OFF                                 
                                MOVE 'NO MORE RECORDS TO SHOW'                  
                                                TO WS-ERROR-MSG                 
                            END-IF                                              
                            WHEN OTHER                                          
      *                     This is some kind of error. Change to END BR        
      *                     And exit                                            
                            SET READ-LOOP-EXIT      TO TRUE                     
                            MOVE 'READ'              TO ERROR-OPNAME            
                            MOVE LIT-CARD-FILE       TO ERROR-FILE              
                            MOVE WS-RESP-CD          TO ERROR-RESP              
                            MOVE WS-REAS-CD          TO ERROR-RESP2             
                          MOVE WS-FILE-ERROR-MESSAGE TO WS-ERROR-MSG            
                      END-EVALUATE                                              
                  END-IF                                                        
               WHEN DFHRESP(ENDFILE)                                            
                  SET READ-LOOP-EXIT              TO TRUE                       
                  SET CA-NEXT-PAGE-NOT-EXISTS     TO TRUE                       
                  MOVE CARD-ACCT-ID     TO WS-CA-LAST-CARD-ACCT-ID              
                  MOVE CARD-NUM         TO WS-CA-LAST-CARD-NUM                  
                  IF WS-ERROR-MSG-OFF                                           
                     MOVE 'NO MORE RECORDS TO SHOW'  TO WS-ERROR-MSG            
                  END-IF                                                        
                  IF WS-CA-SCREEN-NUM = 1                                       
                  AND WS-SCRN-COUNTER = 0                                       
      *               MOVE 'NO RECORDS TO SHOW'  TO WS-ERROR-MSG                
                      SET WS-NO-RECORDS-FOUND    TO TRUE                        
                  END-IF                                                        
               WHEN OTHER                                                       
      *           This is some kind of error. Change to END BR                  
      *           And exit                                                      
                  SET READ-LOOP-EXIT             TO TRUE                        
                  MOVE 'READ'                     TO ERROR-OPNAME               
                  MOVE LIT-CARD-FILE              TO ERROR-FILE                 
                  MOVE WS-RESP-CD                 TO ERROR-RESP                 
                  MOVE WS-REAS-CD                 TO ERROR-RESP2                
                  MOVE WS-FILE-ERROR-MESSAGE      TO WS-ERROR-MSG               
           END-EVALUATE                                                         
           END-PERFORM                                                          
                                                                                
           EXEC CICS ENDBR FILE(LIT-CARD-FILE)                                  
           END-EXEC                                                             
           .                                                                    
       9000-READ-FORWARD-EXIT.                                                  
           EXIT                                                                 
           .                                                                    
       9100-READ-BACKWARDS.                                                     
                                                                                
           MOVE LOW-VALUES           TO WS-ALL-ROWS                             
                                                                                
           MOVE WS-CA-FIRST-CARDKEY  TO WS-CA-LAST-CARDKEY                      
                                                                                
      *****************************************************************         
      *    Start Browse                                                         
      *****************************************************************         
           EXEC CICS STARTBR                                                    
                DATASET(LIT-CARD-FILE)                                          
                RIDFLD(WS-CARD-RID-CARDNUM)                                     
                KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)                        
                GTEQ                                                            
                RESP(WS-RESP-CD)                                                
                RESP2(WS-REAS-CD)                                               
           END-EXEC                                                             
      *****************************************************************         
      *    Loop through records and fetch max screen records                    
      *****************************************************************         
           COMPUTE WS-SCRN-COUNTER =                                            
                                   WS-MAX-SCREEN-LINES + 1                      
           END-COMPUTE                                                          
           SET CA-NEXT-PAGE-EXISTS    TO TRUE                                   
           SET MORE-RECORDS-TO-READ   TO TRUE                                   
                                                                                
      *****************************************************************         
      *    Now we show the records from previous set.                           
      *****************************************************************         
                                                                                
           EXEC CICS READPREV                                                   
                DATASET(LIT-CARD-FILE)                                          
                INTO (CARD-RECORD)                                              
                LENGTH(LENGTH OF CARD-RECORD)                                   
                RIDFLD(WS-CARD-RID-CARDNUM)                                     
                KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)                        
                RESP(WS-RESP-CD)                                                
                RESP2(WS-REAS-CD)                                               
           END-EXEC                                                             
                                                                                
           EVALUATE WS-RESP-CD                                                  
               WHEN DFHRESP(NORMAL)                                             
               WHEN DFHRESP(DUPREC)                                             
                   SUBTRACT 1          FROM WS-SCRN-COUNTER                     
               WHEN OTHER                                                       
      *           This is some kind of error. Change to END BR                  
      *           And exit                                                      
                  SET READ-LOOP-EXIT             TO TRUE                        
                  MOVE 'READ'                     TO ERROR-OPNAME               
                  MOVE LIT-CARD-FILE              TO ERROR-FILE                 
                  MOVE WS-RESP-CD                 TO ERROR-RESP                 
                  MOVE WS-REAS-CD                 TO ERROR-RESP2                
                  MOVE WS-FILE-ERROR-MESSAGE      TO WS-ERROR-MSG               
                  GO TO 9100-READ-BACKWARDS-EXIT                                
           END-EVALUATE                                                         
                                                                                
           PERFORM UNTIL READ-LOOP-EXIT                                         
                                                                                
           EXEC CICS READPREV                                                   
                DATASET(LIT-CARD-FILE)                                          
                INTO (CARD-RECORD)                                              
                LENGTH(LENGTH OF CARD-RECORD)                                   
                RIDFLD(WS-CARD-RID-CARDNUM)                                     
                KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)                        
                RESP(WS-RESP-CD)                                                
                RESP2(WS-REAS-CD)                                               
           END-EXEC                                                             
                                                                                
           EVALUATE WS-RESP-CD                                                  
               WHEN DFHRESP(NORMAL)                                             
               WHEN DFHRESP(DUPREC)                                             
                   PERFORM 9500-FILTER-RECORDS                                  
                      THRU 9500-FILTER-RECORDS-EXIT                             
                   IF WS-DONOT-EXCLUDE-THIS-RECORD                              
                      MOVE CARD-NUM                                             
                                  TO WS-ROW-CARD-NUM(WS-SCRN-COUNTER)           
                      MOVE CARD-ACCT-ID                                         
                                  TO WS-ROW-ACCTNO(WS-SCRN-COUNTER)             
                      MOVE CARD-ACTIVE-STATUS                                   
                                  TO                                            
                                  WS-ROW-CARD-STATUS(WS-SCRN-COUNTER)           
                                                                                
                      SUBTRACT 1  FROM WS-SCRN-COUNTER                          
                      IF WS-SCRN-COUNTER = 0                                    
                         SET READ-LOOP-EXIT  TO TRUE                            
                                                                                
                         MOVE CARD-ACCT-ID                                      
                                  TO WS-CA-FIRST-CARD-ACCT-ID                   
                         MOVE CARD-NUM                                          
                                  TO WS-CA-FIRST-CARD-NUM                       
                      ELSE                                                      
                         CONTINUE                                               
                      END-IF                                                    
                   ELSE                                                         
                       CONTINUE                                                 
                   END-IF                                                       
                                                                                
               WHEN OTHER                                                       
      *           This is some kind of error. Change to END BR                  
      *           And exit                                                      
                  SET READ-LOOP-EXIT             TO TRUE                        
                  MOVE 'READ'                     TO ERROR-OPNAME               
                  MOVE LIT-CARD-FILE              TO ERROR-FILE                 
                  MOVE WS-RESP-CD                 TO ERROR-RESP                 
                  MOVE WS-REAS-CD                 TO ERROR-RESP2                
                  MOVE WS-FILE-ERROR-MESSAGE      TO WS-ERROR-MSG               
           END-EVALUATE                                                         
           END-PERFORM                                                          
           .                                                                    
                                                                                
       9100-READ-BACKWARDS-EXIT.                                                
           EXEC CICS                                                            
                ENDBR FILE(LIT-CARD-FILE)                                       
           END-EXEC                                                             
                                                                                
           EXIT                                                                 
           .                                                                    
                                                                                
       9500-FILTER-RECORDS.                                                     
           SET WS-DONOT-EXCLUDE-THIS-RECORD TO TRUE                             
                                                                                
           IF FLG-ACCTFILTER-ISVALID                                            
              IF  CARD-ACCT-ID = CC-ACCT-ID                                     
                  CONTINUE                                                      
              ELSE                                                              
                  SET WS-EXCLUDE-THIS-RECORD  TO TRUE                           
                  GO TO 9500-FILTER-RECORDS-EXIT                                
              END-IF                                                            
           ELSE                                                                 
             CONTINUE                                                           
           END-IF                                                               
                                                                                
           IF FLG-CARDFILTER-ISVALID                                            
              IF  CARD-NUM = CC-CARD-NUM-N                                      
                  CONTINUE                                                      
              ELSE                                                              
                  SET WS-EXCLUDE-THIS-RECORD TO TRUE                            
                  GO TO 9500-FILTER-RECORDS-EXIT                                
              END-IF                                                            
           ELSE                                                                 
             CONTINUE                                                           
           END-IF                                                               
                                                                                
           .                                                                    
                                                                                
       9500-FILTER-RECORDS-EXIT.                                                
           EXIT                                                                 
           .                                                                    
                                                                                
      *****************************************************************
      *Common code to store PFKey                                      
      *****************************************************************
       COPY 'CSSTRPFY'
           .

      *****************************************************************         
      * Plain text exit - Dont use in production                      *         
      *****************************************************************         
       SEND-PLAIN-TEXT.                                                         
           EXEC CICS SEND TEXT                                                  
                     FROM(WS-ERROR-MSG)                                         
                     LENGTH(LENGTH OF WS-ERROR-MSG)                             
                     ERASE                                                      
                     FREEKB                                                     
           END-EXEC                                                             
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC                                                             
           .                                                                    
       SEND-PLAIN-TEXT-EXIT.                                                    
           EXIT                                                                 
           .                                                                    
      *****************************************************************         
      * Display Long text and exit                                    *         
      * This is primarily for debugging and should not be used in     *         
      * regular course                                                *         
      *****************************************************************         
       SEND-LONG-TEXT.                                                          
           EXEC CICS SEND TEXT                                                  
                     FROM(WS-LONG-MSG)                                          
                     LENGTH(LENGTH OF WS-LONG-MSG)                              
                     ERASE                                                      
                     FREEKB                                                     
           END-EXEC                                                             
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC                                                             
           .                                                                    
       SEND-LONG-TEXT-EXIT.                                                     
           EXIT                                                                 
           .                                                                    
                                                                                
                                                                                
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *
