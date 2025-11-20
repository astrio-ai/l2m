      *****************************************************************         
      * Program:     COCRDUPC.CBL                                     *         
      * Layer:       Business logic                                   *         
      * Function:    Accept and process credit card detail request    *         
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
           COCRDUPC.                                                            
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
            07 WS-UCTRANS                          PIC X(4)                     
                                                   VALUE SPACES.                
      ******************************************************************        
      *      Input edits                                                        
      ******************************************************************        
                                                                                
         05  WS-INPUT-FLAG                         PIC X(1).                    
           88  INPUT-OK                            VALUE '0'.                   
           88  INPUT-ERROR                         VALUE '1'.                   
           88  INPUT-PENDING                       VALUE LOW-VALUES.            
         05  WS-EDIT-ACCT-FLAG                     PIC X(1).                    
           88  FLG-ACCTFILTER-NOT-OK               VALUE '0'.                   
           88  FLG-ACCTFILTER-ISVALID              VALUE '1'.                   
           88  FLG-ACCTFILTER-BLANK                VALUE ' '.                   
         05  WS-EDIT-CARD-FLAG                     PIC X(1).                    
           88  FLG-CARDFILTER-NOT-OK               VALUE '0'.                   
           88  FLG-CARDFILTER-ISVALID             VALUE '1'.                    
           88  FLG-CARDFILTER-BLANK                VALUE ' '.                   
         05  WS-EDIT-CARDNAME-FLAG                 PIC X(1).                    
           88  FLG-CARDNAME-NOT-OK                 VALUE '0'.                   
           88  FLG-CARDNAME-ISVALID                VALUE '1'.                   
           88  FLG-CARDNAME-BLANK                  VALUE ' '.                   
         05  WS-EDIT-CARDSTATUS-FLAG              PIC X(1).                     
           88  FLG-CARDSTATUS-NOT-OK               VALUE '0'.                   
           88  FLG-CARDSTATUS-ISVALID              VALUE '1'.                   
           88  FLG-CARDSTATUS-BLANK                VALUE ' '.                   
         05  WS-EDIT-CARDEXPMON-FLAG              PIC X(1).                     
           88  FLG-CARDEXPMON-NOT-OK               VALUE '0'.                   
           88  FLG-CARDEXPMON-ISVALID              VALUE '1'.                   
           88  FLG-CARDEXPMON-BLANK                VALUE ' '.                   
         05  WS-EDIT-CARDEXPYEAR-FLAG             PIC X(1).                     
           88  FLG-CARDEXPYEAR-NOT-OK              VALUE '0'.                   
           88  FLG-CARDEXPYEAR-ISVALID             VALUE '1'.                   
           88  FLG-CARDEXPYEAR-BLANK               VALUE ' '.                   
         05  WS-RETURN-FLAG                        PIC X(1).                    
           88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.            
           88  WS-RETURN-FLAG-ON                   VALUE '1'.                   
         05  WS-PFK-FLAG                           PIC X(1).                    
           88  PFK-VALID                           VALUE '0'.                   
           88  PFK-INVALID                         VALUE '1'.                   
         05  CARD-NAME-CHECK                       PIC X(50)                    
                                                   VALUE LOW-VALUES.            
         05  FLG-YES-NO-CHECK                      PIC X(1)                     
                                                   VALUE 'N'.                   
           88 FLG-YES-NO-VALID                     VALUES 'Y', 'N'.             
         05  CARD-MONTH-CHECK                      PIC X(2).                    
         05  CARD-MONTH-CHECK-N REDEFINES                                       
             CARD-MONTH-CHECK                      PIC 9(2).                    
             88 VALID-MONTH                        VALUES 1 THRU 12.            
         05  CARD-YEAR-CHECK                      PIC X(4).                     
         05  CARD-YEAR-CHECK-N REDEFINES                                        
             CARD-YEAR-CHECK                      PIC 9(4).                     
             88 VALID-YEAR                        VALUES 1950 THRU 2099.        
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
           10  CARD-CARD-NUM-X                     PIC X(16).                   
           10  CARD-CARD-NUM-N REDEFINES  CARD-CARD-NUM-X                       
                                                   PIC 9(16).                   
           10  CARD-NAME-EMBOSSED-X                PIC X(50).                   
           10  CARD-STATUS-X                       PIC X.                       
           10  CARD-EXPIRAION-DATE-X               PIC X(10).                   
           10  FILLER REDEFINES CARD-EXPIRAION-DATE-X.                          
               20 CARD-EXPIRY-YEAR                 PIC X(4).                    
               20 FILLER                           PIC X(1).                    
               20 CARD-EXPIRY-MONTH                PIC X(2).                    
               20 FILLER                           PIC X(1).                    
               20 CARD-EXPIRY-DAY                  PIC X(2).                    
           10  CARD-EXPIRAION-DATE-N REDEFINES                                  
               CARD-EXPIRAION-DATE-X               PIC 9(10).                   
                                                                                
      ******************************************************************        
      *      File and data Handling                                             
      ******************************************************************        
         05  WS-CARD-RID.                                                       
           10  WS-CARD-RID-CARDNUM                 PIC X(16).                   
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).                   
           10  WS-CARD-RID-ACCT-ID-X REDEFINES                                  
                  WS-CARD-RID-ACCT-ID              PIC X(11).                   
         05  WS-FILE-ERROR-MESSAGE.                                             
           10  FILLER                              PIC X(12)                    
                                                   VALUE 'File Error: '.        
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
          10  FILLER                               PIC X(5)                     
                                                   VALUE SPACES.                
      ******************************************************************        
      *      Output Message Construction                                        
      ******************************************************************        
         05  WS-LONG-MSG                           PIC X(500).                  
         05  WS-INFO-MSG                           PIC X(40).                   
           88  WS-NO-INFO-MESSAGE                 VALUES                        
                                                  SPACES LOW-VALUES.            
           88  FOUND-CARDS-FOR-ACCOUNT             VALUE                        
               'Details of selected card shown above'.                          
           88  PROMPT-FOR-SEARCH-KEYS              VALUE                        
               'Please enter Account and Card Number'.                          
           88  PROMPT-FOR-CHANGES                  VALUE                        
               'Update card details presented above.'.                          
           88  PROMPT-FOR-CONFIRMATION             VALUE                        
               'Changes validated.Press F5 to save'.                            
           88  CONFIRM-UPDATE-SUCCESS              VALUE                        
               'Changes committed to database'.                                 
           88  INFORM-FAILURE                      VALUE                        
               'Changes unsuccessful. Please try again'.                        
                                                                                
         05  WS-RETURN-MSG                         PIC X(75).                   
           88  WS-RETURN-MSG-OFF                   VALUE SPACES.                
           88  WS-EXIT-MESSAGE                     VALUE                        
               'PF03 pressed.Exiting              '.                            
           88  WS-PROMPT-FOR-ACCT                  VALUE                        
               'Account number not provided'.                                   
           88  WS-PROMPT-FOR-CARD                  VALUE                        
               'Card number not provided'.                                      
           88  WS-PROMPT-FOR-NAME                  VALUE                        
               'Card name not provided'.                                        
           88  WS-NAME-MUST-BE-ALPHA               VALUE                        
               'Card name can only contain alphabets and spaces'.               
           88  NO-SEARCH-CRITERIA-RECEIVED         VALUE                        
               'No input received'.                                             
           88  NO-CHANGES-DETECTED                 VALUE                        
               'No change detected with respect to values fetched.'.            
           88  SEARCHED-ACCT-ZEROES                VALUE                        
               'Account number must be a non zero 11 digit number'.             
           88  SEARCHED-ACCT-NOT-NUMERIC           VALUE                        
               'Account number must be a non zero 11 digit number'.             
           88  SEARCHED-CARD-NOT-NUMERIC           VALUE                        
               'Card number if supplied must be a 16 digit number'.             
           88  CARD-STATUS-MUST-BE-YES-NO          VALUE                        
               'Card Active Status must be Y or N'.                             
           88  CARD-EXPIRY-MONTH-NOT-VALID          VALUE                       
               'Card expiry month must be between 1 and 12'.                    
           88  CARD-EXPIRY-YEAR-NOT-VALID          VALUE                        
               'Invalid card expiry year'.                                      
           88  DID-NOT-FIND-ACCT-IN-CARDXREF       VALUE                        
               'Did not find this account in cards database'.                   
           88  DID-NOT-FIND-ACCTCARD-COMBO         VALUE                        
               'Did not find cards for this search condition'.                  
           88  COULD-NOT-LOCK-FOR-UPDATE           VALUE                        
               'Could not lock record for update'.                              
           88  DATA-WAS-CHANGED-BEFORE-UPDATE      VALUE                        
               'Record changed by some one else. Please review'.                
           88  LOCKED-BUT-UPDATE-FAILED            VALUE                        
               'Update of record failed'.                                       
           88  XREF-READ-ERROR                     VALUE                        
               'Error reading Card Data File'.                                  
           88  CODING-TO-BE-DONE                   VALUE                        
               'Looks Good.... so far'.                                         
      ******************************************************************        
      *      Literals and Constants                                             
      ******************************************************************        
       01 WS-LITERALS.                                                          
          05 LIT-THISPGM                           PIC X(8)                     
                                                   VALUE 'COCRDUPC'.            
          05 LIT-THISTRANID                        PIC X(4)                     
                                                   VALUE 'CCUP'.                
          05 LIT-THISMAPSET                        PIC X(8)                     
                                                   VALUE 'COCRDUP '.            
          05 LIT-THISMAP                           PIC X(7)                     
                                                   VALUE 'CCRDUPA'.             
          05 LIT-CCLISTPGM                         PIC X(8)                     
                                                   VALUE 'COCRDLIC'.            
          05 LIT-CCLISTTRANID                      PIC X(4)                     
                                                   VALUE 'CCLI'.                
          05 LIT-CCLISTMAPSET                      PIC X(7)                     
                                                   VALUE 'COCRDLI'.             
          05 LIT-CCLISTMAP                         PIC X(7)                     
                                                   VALUE 'CCRDSLA'.             
          05 LIT-MENUPGM                           PIC X(8)                     
                                                   VALUE 'COMEN01C'.            
          05 LIT-MENUTRANID                        PIC X(4)                     
                                                   VALUE 'CM00'.                
          05 LIT-MENUMAPSET                        PIC X(7)                     
                                                   VALUE 'COMEN01'.             
          05 LIT-MENUMAP                           PIC X(7)                     
                                                   VALUE 'COMEN1A'.             
          05  LIT-CARDDTLPGM                       PIC X(8)                     
                                                   VALUE 'COCRDSLC'.            
          05  LIT-CARDDTLTRANID                    PIC X(4)                     
                                                   VALUE 'CCDL'.                
          05  LIT-CARDDTLMAPSET                    PIC X(7)                     
                                                   VALUE 'COCRDSL'.             
          05  LIT-CARDDTLMAP                       PIC X(7)                     
                                                   VALUE 'CCRDSLA'.             
          05 LIT-CARDFILENAME                      PIC X(8)                     
                                                   VALUE 'CARDDAT '.            
          05 LIT-CARDFILENAME-ACCT-PATH            PIC X(8)                     
                                                   VALUE 'CARDAIX '.            
          05 LIT-ALL-ALPHA-FROM                    PIC X(52)                    
             VALUE                                                              
             'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.            
          05 LIT-ALL-SPACES-TO                     PIC X(52)                    
                                                   VALUE SPACES.                
          05 LIT-UPPER                             PIC X(26)                    
                                 VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.            
          05 LIT-LOWER                             PIC X(26)                    
                                 VALUE 'abcdefghijklmnopqrstuvwxyz'.            
                                                                                
      ******************************************************************        
      *Other common working storage Variables                                   
      ******************************************************************        
       COPY CVCRD01Y.                                                           
                                                                                
      ******************************************************************        
      *Application Commmarea Copybook                                           
       COPY COCOM01Y.                                                           
                                                                                
       01 WS-THIS-PROGCOMMAREA.                                                 
          05 CARD-UPDATE-SCREEN-DATA.                                           
             10 CCUP-CHANGE-ACTION                 PIC X(1)                     
                                                   VALUE LOW-VALUES.            
                88 CCUP-DETAILS-NOT-FETCHED        VALUES                       
                                                   LOW-VALUES,                  
                                                   SPACES.                      
                88 CCUP-SHOW-DETAILS               VALUE 'S'.                   
                88 CCUP-CHANGES-MADE               VALUES 'E', 'N'              
                                                        , 'C', 'L'              
                                                        , 'F'.                  
                88 CCUP-CHANGES-NOT-OK             VALUE 'E'.                   
                88 CCUP-CHANGES-OK-NOT-CONFIRMED   VALUE 'N'.                   
                88 CCUP-CHANGES-OKAYED-AND-DONE    VALUE 'C'.                   
                88 CCUP-CHANGES-FAILED             VALUES 'L', 'F'.             
                88 CCUP-CHANGES-OKAYED-LOCK-ERROR  VALUE 'L'.                   
                88 CCUP-CHANGES-OKAYED-BUT-FAILED  VALUE 'F'.                   
          05 CCUP-OLD-DETAILS.                                                  
             10 CCUP-OLD-ACCTID                    PIC X(11).                   
             10 CCUP-OLD-CARDID                    PIC X(16).                   
             10 CCUP-OLD-CVV-CD                    PIC X(3).                    
             10 CCUP-OLD-CARDDATA.                                              
                20 CCUP-OLD-CRDNAME                PIC X(50).                   
                20 CCUP-OLD-EXPIRAION-DATE.                                     
                   25 CCUP-OLD-EXPYEAR             PIC X(4).                    
                   25 CCUP-OLD-EXPMON              PIC X(2).                    
                   25 CCUP-OLD-EXPDAY              PIC X(2).                    
                20 CCUP-OLD-CRDSTCD                PIC X(1).                    
                                                                                
          05 CCUP-NEW-DETAILS.                                                  
             10 CCUP-NEW-ACCTID                    PIC X(11).                   
             10 CCUP-NEW-CARDID                    PIC X(16).                   
             10 CCUP-NEW-CVV-CD                    PIC X(3).                    
             10 CCUP-NEW-CARDDATA.                                              
                20 CCUP-NEW-CRDNAME                PIC X(50).                   
                20 CCUP-NEW-EXPIRAION-DATE.                                     
                   25 CCUP-NEW-EXPYEAR             PIC X(4).                    
                   25 CCUP-NEW-EXPMON              PIC X(2).                    
                   25 CCUP-NEW-EXPDAY              PIC X(2).                    
                20 CCUP-NEW-CRDSTCD                PIC X(1).                    
          05 CARD-UPDATE-RECORD.                                                
             10 CARD-UPDATE-NUM                   PIC X(16).                    
             10 CARD-UPDATE-ACCT-ID               PIC 9(11).                    
             10 CARD-UPDATE-CVV-CD                PIC 9(03).                    
             10 CARD-UPDATE-EMBOSSED-NAME         PIC X(50).                    
             10 CARD-UPDATE-EXPIRAION-DATE        PIC X(10).                    
             10 CARD-UPDATE-ACTIVE-STATUS         PIC X(01).                    
             10 FILLER                            PIC X(59).                    
                                                                                
                                                                                
       01  WS-COMMAREA                             PIC X(2000).                 
                                                                                
      *IBM SUPPLIED COPYBOOKS                                                   
       COPY DFHBMSCA.                                                           
       COPY DFHAID.                                                             
                                                                                
      *COMMON COPYBOOKS                                                         
      *Screen Titles                                                            
       COPY COTTL01Y.                                                           
      *Credit Card Update Screen Layout                                         
       COPY COCRDUP.                                                            
                                                                                
      *Current Date                                                             
       COPY CSDAT01Y.                                                           
                                                                                
      *Common Messages                                                          
       COPY CSMSG01Y.                                                           
                                                                                
      *Abend Variables                                                          
       COPY CSMSG02Y.                                                           
                                                                                
      *Signed on user data                                                      
       COPY CSUSR01Y.                                                           
                                                                                
      *Dataset layouts                                                          
      *ACCOUNT RECORD LAYOUT                                                    
      *COPY CVACT01Y.                                                           
                                                                                
      *CARD RECORD LAYOUT                                                       
       COPY CVACT02Y.                                                           
                                                                                
      *CARD XREF LAYOUT                                                         
      *COPY CVACT03Y.                                                           
                                                                                
      *CUSTOMER LAYOUT                                                          
       COPY CVCUS01Y.                                                           
                                                                                
       LINKAGE SECTION.                                                         
       01  DFHCOMMAREA.                                                         
         05  FILLER                                PIC X(1)                     
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.                     
                                                                                
       PROCEDURE DIVISION.                                                      
       0000-MAIN.                                                               
                                                                                
                                                                                
           EXEC CICS HANDLE ABEND                                               
                     LABEL(ABEND-ROUTINE)                                       
           END-EXEC                                                             
                                                                                
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
           SET WS-RETURN-MSG-OFF  TO TRUE                                       
      *****************************************************************         
      * Store passed data if  any                *                              
      *****************************************************************         
           IF EIBCALEN IS EQUAL TO 0                                            
               OR (CDEMO-FROM-PROGRAM = LIT-MENUPGM                             
               AND NOT CDEMO-PGM-REENTER)                                       
              INITIALIZE CARDDEMO-COMMAREA                                      
                         WS-THIS-PROGCOMMAREA                                   
              SET CDEMO-PGM-ENTER TO TRUE                                       
              SET CCUP-DETAILS-NOT-FETCHED TO TRUE                              
           ELSE                                                                 
              MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA)  TO              
                                CARDDEMO-COMMAREA                               
              MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:                 
                               LENGTH OF WS-THIS-PROGCOMMAREA ) TO              
                                WS-THIS-PROGCOMMAREA                            
           END-IF                                                               
      *****************************************************************         
      * Remap PFkeys as needed.                                                 
      * Store the Mapped PF Key                                                 
      *****************************************************************         
           PERFORM YYYY-STORE-PFKEY                                             
              THRU YYYY-STORE-PFKEY-EXIT                                        
      *****************************************************************         
      * Check the AID to see if its valid at this point               *         
      * F3 - Exit                                                               
      * Enter show screen again                                                 
      *****************************************************************         
           SET PFK-INVALID TO TRUE                                              
           IF CCARD-AID-ENTER OR                                                
              CCARD-AID-PFK03 OR                                                
              (CCARD-AID-PFK05 AND CCUP-CHANGES-OK-NOT-CONFIRMED)               
                              OR                                                
              (CCARD-AID-PFK12 AND NOT CCUP-DETAILS-NOT-FETCHED)                
              SET PFK-VALID TO TRUE                                             
           END-IF                                                               
                                                                                
           IF PFK-INVALID                                                       
              SET CCARD-AID-ENTER TO TRUE                                       
           END-IF                                                               
                                                                                
      *****************************************************************         
      * Decide what to do based on inputs received                              
      *****************************************************************         
           EVALUATE TRUE                                                        
      ******************************************************************        
      *       USER PRESSES PF03 TO EXIT                                         
      *  OR   USER IS DONE WITH UPDATE                                          
      *            XCTL TO CALLING PROGRAM OR MAIN MENU                         
      ******************************************************************        
              WHEN CCARD-AID-PFK03                                              
              WHEN (CCUP-CHANGES-OKAYED-AND-DONE                                
               AND  CDEMO-LAST-MAPSET   EQUAL LIT-CCLISTMAPSET)                 
              WHEN (CCUP-CHANGES-FAILED                                         
               AND  CDEMO-LAST-MAPSET   EQUAL LIT-CCLISTMAPSET)                 
                   SET CCARD-AID-PFK03     TO TRUE                              
                                                                                
                   IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES                     
                   OR CDEMO-FROM-TRANID    EQUAL SPACES                         
                      MOVE LIT-MENUTRANID  TO CDEMO-TO-TRANID                   
                   ELSE                                                         
                      MOVE CDEMO-FROM-TRANID  TO CDEMO-TO-TRANID                
                   END-IF                                                       
                                                                                
                   IF CDEMO-FROM-PROGRAM   EQUAL LOW-VALUES                     
                   OR CDEMO-FROM-PROGRAM   EQUAL SPACES                         
                      MOVE LIT-MENUPGM     TO CDEMO-TO-PROGRAM                  
                   ELSE                                                         
                      MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM               
                   END-IF                                                       
                                                                                
                   MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID                 
                   MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM                
                                                                                
                   IF CDEMO-LAST-MAPSET    EQUAL LIT-CCLISTMAPSET               
                       MOVE ZEROS          TO CDEMO-ACCT-ID                     
                                              CDEMO-CARD-NUM                    
                   END-IF                                                       
                                                                                
                   SET  CDEMO-USRTYP-USER  TO TRUE                              
                   SET  CDEMO-PGM-ENTER    TO TRUE                              
                   MOVE LIT-THISMAPSET     TO CDEMO-LAST-MAPSET                 
                   MOVE LIT-THISMAP        TO CDEMO-LAST-MAP                    
                                                                                
                   EXEC CICS                                                    
                        SYNCPOINT                                               
                   END-EXEC                                                     
      *                                                                         
                   EXEC CICS XCTL                                               
                        PROGRAM (CDEMO-TO-PROGRAM)                              
                        COMMAREA(CARDDEMO-COMMAREA)                             
                   END-EXEC                                                     
      ******************************************************************        
      *       USER CAME FROM CREDIT CARD LIST SCREEN                            
      *            SO WE ALREADY HAVE THE FILTER KEYS                           
      *            FETCH THE ASSSOCIATED CARD DETAILS FOR UPDATE                
      ******************************************************************        
              WHEN CDEMO-PGM-ENTER                                              
               AND CDEMO-FROM-PROGRAM  EQUAL LIT-CCLISTPGM                      
              WHEN CCARD-AID-PFK12                                              
               AND CDEMO-FROM-PROGRAM  EQUAL LIT-CCLISTPGM                      
                   SET CDEMO-PGM-REENTER    TO TRUE                             
                   SET INPUT-OK             TO TRUE                             
                   SET FLG-ACCTFILTER-ISVALID  TO TRUE                          
                   SET FLG-CARDFILTER-ISVALID  TO TRUE                          
                   MOVE CDEMO-ACCT-ID       TO CC-ACCT-ID-N                     
                   MOVE CDEMO-CARD-NUM      TO CC-CARD-NUM-N                    
                   PERFORM 9000-READ-DATA                                       
                      THRU 9000-READ-DATA-EXIT                                  
                   SET CCUP-SHOW-DETAILS TO TRUE                                
                   PERFORM 3000-SEND-MAP                                        
                      THRU 3000-SEND-MAP-EXIT                                   
                   GO TO COMMON-RETURN                                          
      ******************************************************************        
      *       FRESH ENTRY INTO PROGRAM                                          
      *            ASK THE USER FOR THE KEYS TO FETCH CARD TO BE UPDATED        
      ******************************************************************        
              WHEN CCUP-DETAILS-NOT-FETCHED                                     
               AND CDEMO-PGM-ENTER                                              
              WHEN CDEMO-FROM-PROGRAM   EQUAL LIT-MENUPGM                       
               AND NOT CDEMO-PGM-REENTER                                        
                   INITIALIZE WS-THIS-PROGCOMMAREA                              
                   PERFORM 3000-SEND-MAP THRU                                   
                           3000-SEND-MAP-EXIT                                   
                   SET CDEMO-PGM-REENTER        TO TRUE                         
                   SET CCUP-DETAILS-NOT-FETCHED TO TRUE                         
                   GO TO COMMON-RETURN                                          
      ******************************************************************        
      *       CARD DATA CHANGES REVIEWED, OKAYED AND DONE SUCESSFULLY           
      *            RESET THE SEARCH KEYS                                        
      *            ASK THE USER FOR FRESH SEARCH CRITERIA                       
      ******************************************************************        
              WHEN CCUP-CHANGES-OKAYED-AND-DONE                                 
              WHEN CCUP-CHANGES-FAILED                                          
                   INITIALIZE WS-THIS-PROGCOMMAREA                              
                              WS-MISC-STORAGE                                   
                              CDEMO-ACCT-ID                                     
                              CDEMO-CARD-NUM                                    
                   SET CDEMO-PGM-ENTER            TO TRUE                       
                   PERFORM 3000-SEND-MAP THRU                                   
                           3000-SEND-MAP-EXIT                                   
                   SET CDEMO-PGM-REENTER          TO TRUE                       
                   SET CCUP-DETAILS-NOT-FETCHED   TO TRUE                       
                   GO TO COMMON-RETURN                                          
      ******************************************************************        
      *      CARD DATA HAS BEEN PRESENTED TO USER                               
      *            CHECK THE USER INPUTS                                        
      *            DECIDE WHAT TO DO                                            
      *            PRESENT NEXT STEPS TO USER                                   
      ******************************************************************        
              WHEN OTHER                                                        
                   PERFORM 1000-PROCESS-INPUTS                                  
                      THRU 1000-PROCESS-INPUTS-EXIT                             
                   PERFORM 2000-DECIDE-ACTION                                   
                      THRU 2000-DECIDE-ACTION-EXIT                              
                   PERFORM 3000-SEND-MAP                                        
                      THRU 3000-SEND-MAP-EXIT                                   
                   GO TO COMMON-RETURN                                          
           END-EVALUATE                                                         
           .                                                                    
                                                                                
       COMMON-RETURN.                                                           
           MOVE WS-RETURN-MSG     TO CCARD-ERROR-MSG                            
                                                                                
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
                                                                                
       1000-PROCESS-INPUTS.                                                     
           PERFORM 1100-RECEIVE-MAP                                             
              THRU 1100-RECEIVE-MAP-EXIT                                        
           PERFORM 1200-EDIT-MAP-INPUTS                                         
              THRU 1200-EDIT-MAP-INPUTS-EXIT                                    
           MOVE WS-RETURN-MSG  TO CCARD-ERROR-MSG                               
           MOVE LIT-THISPGM    TO CCARD-NEXT-PROG                               
           MOVE LIT-THISMAPSET TO CCARD-NEXT-MAPSET                             
           MOVE LIT-THISMAP    TO CCARD-NEXT-MAP                                
           .                                                                    
                                                                                
       1000-PROCESS-INPUTS-EXIT.                                                
           EXIT                                                                 
           .                                                                    
       1100-RECEIVE-MAP.                                                        
           EXEC CICS RECEIVE MAP(LIT-THISMAP)                                   
                     MAPSET(LIT-THISMAPSET)                                     
                     INTO(CCRDUPAI)                                             
                     RESP(WS-RESP-CD)                                           
                     RESP2(WS-REAS-CD)                                          
           END-EXEC                                                             
                                                                                
           INITIALIZE CCUP-NEW-DETAILS                                          
                                                                                
      *    REPLACE * WITH LOW-VALUES                                            
           IF  ACCTSIDI OF CCRDUPAI = '*'                                       
           OR  ACCTSIDI OF CCRDUPAI = SPACES                                    
               MOVE LOW-VALUES           TO  CC-ACCT-ID                         
                                             CCUP-NEW-ACCTID                    
           ELSE                                                                 
               MOVE ACCTSIDI OF CCRDUPAI TO  CC-ACCT-ID                         
                                             CCUP-NEW-ACCTID                    
           END-IF                                                               
                                                                                
           IF  CARDSIDI OF CCRDUPAI = '*'                                       
           OR  CARDSIDI OF CCRDUPAI = SPACES                                    
               MOVE LOW-VALUES           TO  CC-CARD-NUM                        
                                             CCUP-NEW-CARDID                    
           ELSE                                                                 
               MOVE CARDSIDI OF CCRDUPAI TO  CC-CARD-NUM                        
                                             CCUP-NEW-CARDID                    
           END-IF                                                               
                                                                                
           IF  CRDNAMEI OF CCRDUPAI = '*'                                       
           OR  CRDNAMEI OF CCRDUPAI = SPACES                                    
               MOVE LOW-VALUES           TO  CCUP-NEW-CRDNAME                   
           ELSE                                                                 
               MOVE CRDNAMEI OF CCRDUPAI TO  CCUP-NEW-CRDNAME                   
           END-IF                                                               
                                                                                
           IF  CRDSTCDI OF CCRDUPAI = '*'                                       
           OR  CRDSTCDI OF CCRDUPAI = SPACES                                    
               MOVE LOW-VALUES           TO  CCUP-NEW-CRDSTCD                   
           ELSE                                                                 
               MOVE CRDSTCDI OF CCRDUPAI TO  CCUP-NEW-CRDSTCD                   
           END-IF                                                               
                                                                                
           MOVE EXPDAYI     OF CCRDUPAI  TO  CCUP-NEW-EXPDAY                    
                                                                                
           IF  EXPMONI OF CCRDUPAI = '*'                                        
           OR  EXPMONI OF CCRDUPAI = SPACES                                     
               MOVE LOW-VALUES           TO  CCUP-NEW-EXPMON                    
           ELSE                                                                 
               MOVE EXPMONI OF CCRDUPAI  TO  CCUP-NEW-EXPMON                    
           END-IF                                                               
                                                                                
           IF  EXPYEARI OF CCRDUPAI = '*'                                       
           OR  EXPYEARI OF CCRDUPAI = SPACES                                    
               MOVE LOW-VALUES           TO  CCUP-NEW-EXPYEAR                   
           ELSE                                                                 
               MOVE EXPYEARI OF CCRDUPAI TO  CCUP-NEW-EXPYEAR                   
           END-IF                                                               
           .                                                                    
                                                                                
       1100-RECEIVE-MAP-EXIT.                                                   
           EXIT                                                                 
           .                                                                    
       1200-EDIT-MAP-INPUTS.                                                    
                                                                                
           SET INPUT-OK                  TO TRUE                                
                                                                                
           IF  CCUP-DETAILS-NOT-FETCHED                                         
      *        VALIDATE THE SEARCH KEYS                                         
               PERFORM 1210-EDIT-ACCOUNT                                        
                  THRU 1210-EDIT-ACCOUNT-EXIT                                   
                                                                                
               PERFORM 1220-EDIT-CARD                                           
                  THRU 1220-EDIT-CARD-EXIT                                      
                                                                                
               MOVE LOW-VALUES                 TO CCUP-NEW-CARDDATA             
                                                                                
      *       IF THE SEARCH CONDITIONS HAVE PROBLEMS SKIP OTHER EDITS           
               IF  FLG-ACCTFILTER-BLANK                                         
               AND FLG-CARDFILTER-BLANK                                         
                   SET NO-SEARCH-CRITERIA-RECEIVED TO TRUE                      
               END-IF                                                           
                                                                                
               GO TO 1200-EDIT-MAP-INPUTS-EXIT                                  
                                                                                
           ELSE                                                                 
               CONTINUE                                                         
           END-IF                                                               
                                                                                
      *    SEARCH KEYS ALREADY VALIDATED AND DATA FETCHED                       
           SET FOUND-CARDS-FOR-ACCOUNT TO TRUE                                  
           SET FLG-ACCTFILTER-ISVALID  TO TRUE                                  
           SET FLG-CARDFILTER-ISVALID  TO TRUE                                  
           MOVE CCUP-OLD-ACCTID     TO CDEMO-ACCT-ID                            
           MOVE CCUP-OLD-CARDID     TO CDEMO-CARD-NUM                           
           MOVE CCUP-OLD-CRDNAME    TO CARD-EMBOSSED-NAME                       
           MOVE CCUP-OLD-CRDSTCD    TO CARD-ACTIVE-STATUS                       
           MOVE CCUP-OLD-EXPDAY     TO CARD-EXPIRY-DAY                          
           MOVE CCUP-OLD-EXPMON     TO CARD-EXPIRY-MONTH                        
           MOVE CCUP-OLD-EXPYEAR    TO CARD-EXPIRY-YEAR                         
                                                                                
      *    NEW DATA IS SAME AS OLD DATA                                         
           IF  (FUNCTION UPPER-CASE(CCUP-NEW-CARDDATA) EQUAL                    
                FUNCTION UPPER-CASE(CCUP-OLD-CARDDATA))                         
               SET NO-CHANGES-DETECTED TO TRUE                                  
           END-IF                                                               
                                                                                
           IF  NO-CHANGES-DETECTED                                              
           OR  CCUP-CHANGES-OK-NOT-CONFIRMED                                    
           OR  CCUP-CHANGES-OKAYED-AND-DONE                                     
               SET FLG-CARDNAME-ISVALID    TO TRUE                              
               SET FLG-CARDSTATUS-ISVALID  TO TRUE                              
               SET FLG-CARDEXPMON-ISVALID  TO TRUE                              
               SET FLG-CARDEXPYEAR-ISVALID TO TRUE                              
               GO TO 1200-EDIT-MAP-INPUTS-EXIT                                  
           END-IF                                                               
                                                                                
                                                                                
           SET CCUP-CHANGES-NOT-OK    TO TRUE                                   
                                                                                
           PERFORM 1230-EDIT-NAME                                               
              THRU 1230-EDIT-NAME-EXIT                                          
                                                                                
           PERFORM 1240-EDIT-CARDSTATUS                                         
              THRU 1240-EDIT-CARDSTATUS-EXIT                                    
                                                                                
           PERFORM 1250-EDIT-EXPIRY-MON                                         
              THRU 1250-EDIT-EXPIRY-MON-EXIT                                    
                                                                                
           PERFORM 1260-EDIT-EXPIRY-YEAR                                        
              THRU 1260-EDIT-EXPIRY-YEAR-EXIT                                   
                                                                                
           IF INPUT-ERROR                                                       
              CONTINUE                                                          
           ELSE                                                                 
              SET CCUP-CHANGES-OK-NOT-CONFIRMED TO TRUE                         
           END-IF                                                               
           .                                                                    
                                                                                
       1200-EDIT-MAP-INPUTS-EXIT.                                               
           EXIT                                                                 
           .                                                                    
                                                                                
       1210-EDIT-ACCOUNT.                                                       
           SET FLG-ACCTFILTER-NOT-OK TO TRUE                                    
                                                                                
      *    Not supplied                                                         
           IF CC-ACCT-ID   EQUAL LOW-VALUES                                     
           OR CC-ACCT-ID   EQUAL SPACES                                         
           OR CC-ACCT-ID-N EQUAL ZEROS                                          
              SET INPUT-ERROR           TO TRUE                                 
              SET FLG-ACCTFILTER-BLANK  TO TRUE                                 
              IF WS-RETURN-MSG-OFF                                              
                 SET WS-PROMPT-FOR-ACCT TO TRUE                                 
              END-IF                                                            
              MOVE ZEROES       TO CDEMO-ACCT-ID                                
              MOVE LOW-VALUES   TO CCUP-NEW-ACCTID                              
              GO TO  1210-EDIT-ACCOUNT-EXIT                                     
           END-IF                                                               
      *                                                                         
      *    Not numeric                                                          
      *    Not 11 characters                                                    
           IF CC-ACCT-ID  IS NOT NUMERIC                                        
              SET INPUT-ERROR TO TRUE                                           
              SET FLG-ACCTFILTER-NOT-OK TO TRUE                                 
              IF WS-RETURN-MSG-OFF                                              
                MOVE                                                            
              'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'            
                              TO WS-RETURN-MSG                                  
              END-IF                                                            
              MOVE ZERO       TO CDEMO-ACCT-ID                                  
              MOVE LOW-VALUES TO CCUP-NEW-ACCTID                                
              GO TO 1210-EDIT-ACCOUNT-EXIT                                      
           ELSE                                                                 
              MOVE CC-ACCT-ID TO CDEMO-ACCT-ID                                  
                                 CCUP-NEW-ACCTID                                
              SET FLG-ACCTFILTER-ISVALID TO TRUE                                
           END-IF                                                               
           .                                                                    
                                                                                
       1210-EDIT-ACCOUNT-EXIT.                                                  
           EXIT                                                                 
           .                                                                    
                                                                                
       1220-EDIT-CARD.                                                          
      *    Not numeric                                                          
      *    Not 16 characters                                                    
           SET FLG-CARDFILTER-NOT-OK TO TRUE                                    
                                                                                
      *    Not supplied                                                         
           IF CC-CARD-NUM   EQUAL LOW-VALUES                                    
           OR CC-CARD-NUM   EQUAL SPACES                                        
           OR CC-CARD-NUM-N EQUAL ZEROS                                         
              SET INPUT-ERROR           TO TRUE                                 
              SET FLG-CARDFILTER-BLANK  TO TRUE                                 
              IF WS-RETURN-MSG-OFF                                              
                 SET WS-PROMPT-FOR-CARD TO TRUE                                 
              END-IF                                                            
                                                                                
              MOVE ZEROES        TO CDEMO-CARD-NUM                              
                                   CCUP-NEW-CARDID                              
              GO TO  1220-EDIT-CARD-EXIT                                        
           END-IF                                                               
      *                                                                         
      *    Not numeric                                                          
      *    Not 16 characters                                                    
           IF CC-CARD-NUM  IS NOT NUMERIC                                       
              SET INPUT-ERROR TO TRUE                                           
              SET FLG-CARDFILTER-NOT-OK TO TRUE                                 
              IF WS-RETURN-MSG-OFF                                              
                 MOVE                                                           
              'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'            
                                 TO WS-RETURN-MSG                               
              END-IF                                                            
              MOVE ZERO          TO CDEMO-CARD-NUM                              
              MOVE LOW-VALUES    TO CCUP-NEW-CARDID                             
              GO TO 1220-EDIT-CARD-EXIT                                         
           ELSE                                                                 
              MOVE CC-CARD-NUM-N TO CDEMO-CARD-NUM                              
              MOVE CC-CARD-NUM   TO CCUP-NEW-CARDID                             
              SET FLG-CARDFILTER-ISVALID TO TRUE                                
           END-IF                                                               
           .                                                                    
                                                                                
       1220-EDIT-CARD-EXIT.                                                     
           EXIT                                                                 
           .                                                                    
                                                                                
       1230-EDIT-NAME.                                                          
      *    Not BLANK                                                            
           SET FLG-CARDNAME-NOT-OK      TO TRUE                                 
                                                                                
      *    Not supplied                                                         
           IF CCUP-NEW-CRDNAME   EQUAL LOW-VALUES                               
           OR CCUP-NEW-CRDNAME   EQUAL SPACES                                   
           OR CCUP-NEW-CRDNAME   EQUAL ZEROS                                    
              SET INPUT-ERROR           TO TRUE                                 
              SET FLG-CARDNAME-BLANK  TO TRUE                                   
              IF WS-RETURN-MSG-OFF                                              
                 SET WS-PROMPT-FOR-NAME TO TRUE                                 
              END-IF                                                            
              GO TO  1230-EDIT-NAME-EXIT                                        
           END-IF                                                               
                                                                                
      *    Only Alphabets and space allowed                                     
           MOVE CCUP-NEW-CRDNAME        TO CARD-NAME-CHECK                      
           INSPECT CARD-NAME-CHECK                                              
             CONVERTING LIT-ALL-ALPHA-FROM                                      
                     TO LIT-ALL-SPACES-TO                                       
                                                                                
           IF FUNCTION LENGTH(FUNCTION TRIM(CARD-NAME-CHECK)) = 0               
              CONTINUE                                                          
           ELSE                                                                 
              SET INPUT-ERROR           TO TRUE                                 
              SET FLG-CARDNAME-NOT-OK   TO TRUE                                 
              IF WS-RETURN-MSG-OFF                                              
                 SET WS-NAME-MUST-BE-ALPHA  TO TRUE                             
              END-IF                                                            
              GO TO  1230-EDIT-NAME-EXIT                                        
           END-IF                                                               
                                                                                
           SET FLG-CARDNAME-ISVALID     TO TRUE                                 
           .                                                                    
       1230-EDIT-NAME-EXIT.                                                     
           EXIT                                                                 
           .                                                                    
                                                                                
       1240-EDIT-CARDSTATUS.                                                    
      *    Must be Y or N                                                       
           SET FLG-CARDSTATUS-NOT-OK      TO TRUE                               
                                                                                
      *    Not supplied                                                         
           IF CCUP-NEW-CRDSTCD   EQUAL LOW-VALUES                               
           OR CCUP-NEW-CRDSTCD   EQUAL SPACES                                   
           OR CCUP-NEW-CRDSTCD   EQUAL ZEROS                                    
              SET INPUT-ERROR           TO TRUE                                 
              SET FLG-CARDSTATUS-BLANK  TO TRUE                                 
              IF WS-RETURN-MSG-OFF                                              
                 SET CARD-STATUS-MUST-BE-YES-NO TO TRUE                         
              END-IF                                                            
              GO TO  1240-EDIT-CARDSTATUS-EXIT                                  
           END-IF                                                               
                                                                                
           MOVE CCUP-NEW-CRDSTCD          TO FLG-YES-NO-CHECK                   
                                                                                
           IF FLG-YES-NO-VALID                                                  
              SET FLG-CARDSTATUS-ISVALID  TO TRUE                               
           ELSE                                                                 
              SET INPUT-ERROR             TO TRUE                               
              SET FLG-CARDSTATUS-NOT-OK   TO TRUE                               
              IF WS-RETURN-MSG-OFF                                              
                 SET CARD-STATUS-MUST-BE-YES-NO  TO TRUE                        
              END-IF                                                            
              GO TO  1240-EDIT-CARDSTATUS-EXIT                                  
           END-IF                                                               
           .                                                                    
       1240-EDIT-CARDSTATUS-EXIT.                                               
           EXIT                                                                 
           .                                                                    
       1250-EDIT-EXPIRY-MON.                                                    
                                                                                
                                                                                
           SET FLG-CARDEXPMON-NOT-OK      TO TRUE                               
                                                                                
      *    Not supplied                                                         
           IF CCUP-NEW-EXPMON   EQUAL LOW-VALUES                                
           OR CCUP-NEW-EXPMON   EQUAL SPACES                                    
           OR CCUP-NEW-EXPMON   EQUAL ZEROS                                     
              SET INPUT-ERROR           TO TRUE                                 
              SET FLG-CARDEXPMON-BLANK  TO TRUE                                 
              IF WS-RETURN-MSG-OFF                                              
                 SET CARD-EXPIRY-MONTH-NOT-VALID TO TRUE                        
              END-IF                                                            
              GO TO  1250-EDIT-EXPIRY-MON-EXIT                                  
           END-IF                                                               
                                                                                
      *    Must be numeric                                                      
      *    Must be 1 to 12                                                      
           MOVE CCUP-NEW-EXPMON           TO CARD-MONTH-CHECK                   
                                                                                
           IF VALID-MONTH                                                       
              SET FLG-CARDEXPMON-ISVALID  TO TRUE                               
           ELSE                                                                 
              SET INPUT-ERROR             TO TRUE                               
              SET FLG-CARDEXPMON-NOT-OK   TO TRUE                               
              IF WS-RETURN-MSG-OFF                                              
                 SET CARD-EXPIRY-MONTH-NOT-VALID  TO TRUE                       
              END-IF                                                            
              GO TO  1250-EDIT-EXPIRY-MON-EXIT                                  
           END-IF                                                               
           .                                                                    
                                                                                
       1250-EDIT-EXPIRY-MON-EXIT.                                               
           EXIT                                                                 
           .                                                                    
       1260-EDIT-EXPIRY-YEAR.                                                   
                                                                                
      *    Not supplied                                                         
           IF CCUP-NEW-EXPYEAR   EQUAL LOW-VALUES                               
           OR CCUP-NEW-EXPYEAR   EQUAL SPACES                                   
           OR CCUP-NEW-EXPYEAR   EQUAL ZEROS                                    
              SET INPUT-ERROR           TO TRUE                                 
              SET FLG-CARDEXPYEAR-BLANK  TO TRUE                                
              IF WS-RETURN-MSG-OFF                                              
                 SET CARD-EXPIRY-YEAR-NOT-VALID TO TRUE                         
              END-IF                                                            
              GO TO  1260-EDIT-EXPIRY-YEAR-EXIT                                 
           END-IF                                                               
                                                                                
      *    Must be numeric                                                      
      *    Must be 1 to 12                                                      
                                                                                
           SET FLG-CARDEXPYEAR-NOT-OK      TO TRUE                              
                                                                                
           MOVE CCUP-NEW-EXPYEAR           TO CARD-YEAR-CHECK                   
                                                                                
           IF VALID-YEAR                                                        
              SET FLG-CARDEXPYEAR-ISVALID  TO TRUE                              
           ELSE                                                                 
              SET INPUT-ERROR              TO TRUE                              
              SET FLG-CARDEXPYEAR-NOT-OK   TO TRUE                              
              IF WS-RETURN-MSG-OFF                                              
                 SET CARD-EXPIRY-YEAR-NOT-VALID  TO TRUE                        
              END-IF                                                            
              GO TO  1260-EDIT-EXPIRY-YEAR-EXIT                                 
           END-IF                                                               
           .                                                                    
       1260-EDIT-EXPIRY-YEAR-EXIT.                                              
           EXIT                                                                 
           .                                                                    
       2000-DECIDE-ACTION.                                                      
           EVALUATE TRUE                                                        
      ******************************************************************        
      *       NO DETAILS SHOWN.                                                 
      *       SO GET THEM AND SETUP DETAIL EDIT SCREEN                          
      ******************************************************************        
              WHEN CCUP-DETAILS-NOT-FETCHED                                     
      ******************************************************************        
      *       CHANGES MADE. BUT USER CANCELS                                    
      ******************************************************************        
              WHEN CCARD-AID-PFK12                                              
                 IF  FLG-ACCTFILTER-ISVALID                                     
                 AND FLG-CARDFILTER-ISVALID                                     
                     PERFORM 9000-READ-DATA                                     
                        THRU 9000-READ-DATA-EXIT                                
                     IF FOUND-CARDS-FOR-ACCOUNT                                 
                        SET CCUP-SHOW-DETAILS    TO TRUE                        
                     END-IF                                                     
                 END-IF                                                         
      ******************************************************************        
      *       DETAILS SHOWN                                                     
      *       CHECK CHANGES AND ASK CONFIRMATION IF GOOD                        
      ******************************************************************        
              WHEN CCUP-SHOW-DETAILS                                            
                 IF INPUT-ERROR                                                 
                 OR NO-CHANGES-DETECTED                                         
                    CONTINUE                                                    
                 ELSE                                                           
                    SET CCUP-CHANGES-OK-NOT-CONFIRMED TO TRUE                   
                 END-IF                                                         
      ******************************************************************        
      *       DETAILS SHOWN                                                     
      *       BUT INPUT EDIT ERRORS FOUND                                       
      ******************************************************************        
              WHEN CCUP-CHANGES-NOT-OK                                          
                  CONTINUE                                                      
      ******************************************************************        
      *       DETAILS EDITED , FOUND OK, CONFIRM SAVE REQUESTED                 
      *       CONFIRMATION GIVEN.SO SAVE THE CHANGES                            
      ******************************************************************        
              WHEN CCUP-CHANGES-OK-NOT-CONFIRMED                                
               AND CCARD-AID-PFK05                                              
                 PERFORM 9200-WRITE-PROCESSING                                  
                    THRU 9200-WRITE-PROCESSING-EXIT                             
                 EVALUATE TRUE                                                  
                    WHEN COULD-NOT-LOCK-FOR-UPDATE                              
                         SET CCUP-CHANGES-OKAYED-LOCK-ERROR TO TRUE             
                    WHEN LOCKED-BUT-UPDATE-FAILED                               
                       SET CCUP-CHANGES-OKAYED-BUT-FAILED TO TRUE               
                    WHEN DATA-WAS-CHANGED-BEFORE-UPDATE                         
                        SET CCUP-SHOW-DETAILS            TO TRUE                
                    WHEN OTHER                                                  
                       SET CCUP-CHANGES-OKAYED-AND-DONE   TO TRUE               
                 END-EVALUATE                                                   
      ******************************************************************        
      *       DETAILS EDITED , FOUND OK, CONFIRM SAVE REQUESTED                 
      *       CONFIRMATION NOT GIVEN. SO SHOW DETAILS AGAIN                     
      ******************************************************************        
              WHEN CCUP-CHANGES-OK-NOT-CONFIRMED                                
                  CONTINUE                                                      
      ******************************************************************        
      *       SHOW CONFIRMATION. GO BACK TO SQUARE 1                            
      ******************************************************************        
              WHEN CCUP-CHANGES-OKAYED-AND-DONE                                 
                  SET CCUP-SHOW-DETAILS TO TRUE                                 
                  IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES                      
                  OR CDEMO-FROM-TRANID    EQUAL SPACES                          
                     MOVE ZEROES       TO CDEMO-ACCT-ID                         
                                          CDEMO-CARD-NUM                        
                     MOVE LOW-VALUES   TO CDEMO-ACCT-STATUS                     
                  END-IF                                                        
              WHEN OTHER                                                        
                   MOVE LIT-THISPGM    TO ABEND-CULPRIT                         
                   MOVE '0001'         TO ABEND-CODE                            
                   MOVE SPACES         TO ABEND-REASON                          
                   MOVE 'UNEXPECTED DATA SCENARIO'                              
                                       TO ABEND-MSG                             
                   PERFORM ABEND-ROUTINE                                        
                      THRU ABEND-ROUTINE-EXIT                                   
           END-EVALUATE                                                         
           .                                                                    
       2000-DECIDE-ACTION-EXIT.                                                 
           EXIT                                                                 
           .                                                                    
                                                                                
                                                                                
                                                                                
       3000-SEND-MAP.                                                           
           PERFORM 3100-SCREEN-INIT                                             
              THRU 3100-SCREEN-INIT-EXIT                                        
           PERFORM 3200-SETUP-SCREEN-VARS                                       
              THRU 3200-SETUP-SCREEN-VARS-EXIT                                  
           PERFORM 3250-SETUP-INFOMSG                                           
              THRU 3250-SETUP-INFOMSG-EXIT                                      
           PERFORM 3300-SETUP-SCREEN-ATTRS                                      
              THRU 3300-SETUP-SCREEN-ATTRS-EXIT                                 
           PERFORM 3400-SEND-SCREEN                                             
              THRU 3400-SEND-SCREEN-EXIT                                        
           .                                                                    
                                                                                
       3000-SEND-MAP-EXIT.                                                      
           EXIT                                                                 
           .                                                                    
                                                                                
       3100-SCREEN-INIT.                                                        
           MOVE LOW-VALUES TO CCRDUPAO                                          
                                                                                
           MOVE FUNCTION CURRENT-DATE     TO WS-CURDATE-DATA                    
                                                                                
           MOVE CCDA-TITLE01              TO TITLE01O OF CCRDUPAO               
           MOVE CCDA-TITLE02              TO TITLE02O OF CCRDUPAO               
           MOVE LIT-THISTRANID            TO TRNNAMEO OF CCRDUPAO               
           MOVE LIT-THISPGM               TO PGMNAMEO OF CCRDUPAO               
                                                                                
           MOVE FUNCTION CURRENT-DATE     TO WS-CURDATE-DATA                    
                                                                                
           MOVE WS-CURDATE-MONTH          TO WS-CURDATE-MM                      
           MOVE WS-CURDATE-DAY            TO WS-CURDATE-DD                      
           MOVE WS-CURDATE-YEAR(3:2)      TO WS-CURDATE-YY                      
                                                                                
           MOVE WS-CURDATE-MM-DD-YY       TO CURDATEO OF CCRDUPAO               
                                                                                
           MOVE WS-CURTIME-HOURS          TO WS-CURTIME-HH                      
           MOVE WS-CURTIME-MINUTE         TO WS-CURTIME-MM                      
           MOVE WS-CURTIME-SECOND         TO WS-CURTIME-SS                      
                                                                                
           MOVE WS-CURTIME-HH-MM-SS       TO CURTIMEO OF CCRDUPAO               
                                                                                
           .                                                                    
                                                                                
       3100-SCREEN-INIT-EXIT.                                                   
           EXIT                                                                 
           .                                                                    
                                                                                
       3200-SETUP-SCREEN-VARS.                                                  
      *    INITIALIZE SEARCH CRITERIA                                           
           IF CDEMO-PGM-ENTER                                                   
              CONTINUE                                                          
           ELSE                                                                 
              IF CC-ACCT-ID-N = 0                                               
                 MOVE LOW-VALUES          TO ACCTSIDO OF CCRDUPAO               
              ELSE                                                              
                 MOVE CC-ACCT-ID          TO ACCTSIDO OF CCRDUPAO               
              END-IF                                                            
                                                                                
              IF CC-CARD-NUM-N = 0                                              
                MOVE LOW-VALUES           TO CARDSIDO OF CCRDUPAO               
              ELSE                                                              
                MOVE CC-CARD-NUM          TO CARDSIDO OF CCRDUPAO               
              END-IF                                                            
                                                                                
              EVALUATE TRUE                                                     
                  WHEN CCUP-DETAILS-NOT-FETCHED                                 
                       MOVE LOW-VALUES         TO CRDNAMEO OF CCRDUPAO          
                                                  CRDNAMEO OF CCRDUPAO          
                                                  CRDSTCDO OF CCRDUPAO          
                                                  EXPDAYO  OF CCRDUPAO          
                                                  EXPMONO  OF CCRDUPAO          
                                                  EXPYEARO OF CCRDUPAO          
                  WHEN CCUP-SHOW-DETAILS                                        
                      MOVE CCUP-OLD-CRDNAME    TO CRDNAMEO OF CCRDUPAO          
                      MOVE CCUP-OLD-CRDSTCD    TO CRDSTCDO OF CCRDUPAO          
                      MOVE CCUP-OLD-EXPDAY     TO EXPDAYO  OF CCRDUPAO          
                      MOVE CCUP-OLD-EXPMON     TO EXPMONO  OF CCRDUPAO          
                      MOVE CCUP-OLD-EXPYEAR    TO EXPYEARO OF CCRDUPAO          
                  WHEN CCUP-CHANGES-MADE                                        
                      MOVE CCUP-NEW-CRDNAME    TO CRDNAMEO OF CCRDUPAO          
                      MOVE CCUP-NEW-CRDSTCD    TO CRDSTCDO OF CCRDUPAO          
                      MOVE CCUP-NEW-EXPMON     TO EXPMONO  OF CCRDUPAO          
                      MOVE CCUP-NEW-EXPYEAR    TO EXPYEARO OF CCRDUPAO          
      ******************************************************************        
      *               MOVE OLD VALUES TO NON-DISPLAY FIELDS                     
      *               THAT WE ARE NOT ALLOWING USER TO CHANGE(FOR NOW)          
      *****************************************************************         
      *               MOVE CCUP-NEW-EXPDAY     TO EXPDAYO  OF CCRDUPAO          
                      MOVE CCUP-OLD-EXPDAY     TO EXPDAYO  OF CCRDUPAO          
                  WHEN OTHER                                                    
                      MOVE CCUP-OLD-CRDNAME    TO CRDNAMEO OF CCRDUPAO          
                      MOVE CCUP-OLD-CRDSTCD    TO CRDSTCDO OF CCRDUPAO          
                      MOVE CCUP-OLD-EXPDAY     TO EXPDAYO  OF CCRDUPAO          
                      MOVE CCUP-OLD-EXPMON     TO EXPMONO  OF CCRDUPAO          
                      MOVE CCUP-OLD-EXPYEAR    TO EXPYEARO OF CCRDUPAO          
              END-EVALUATE                                                      
                                                                                
                                                                                
            END-IF                                                              
           .                                                                    
       3200-SETUP-SCREEN-VARS-EXIT.                                             
           EXIT                                                                 
           .                                                                    
       3250-SETUP-INFOMSG.                                                      
      *    SETUP INFORMATION MESSAGE                                            
           EVALUATE TRUE                                                        
               WHEN CDEMO-PGM-ENTER                                             
                    SET  PROMPT-FOR-SEARCH-KEYS TO TRUE                         
               WHEN CCUP-DETAILS-NOT-FETCHED                                    
                   SET PROMPT-FOR-SEARCH-KEYS      TO TRUE                      
               WHEN CCUP-SHOW-DETAILS                                           
                    SET FOUND-CARDS-FOR-ACCOUNT    TO TRUE                      
               WHEN CCUP-CHANGES-NOT-OK                                         
                    SET PROMPT-FOR-CHANGES         TO TRUE                      
               WHEN CCUP-CHANGES-OK-NOT-CONFIRMED                               
                    SET PROMPT-FOR-CONFIRMATION    TO TRUE                      
               WHEN CCUP-CHANGES-OKAYED-AND-DONE                                
                    SET CONFIRM-UPDATE-SUCCESS     TO TRUE                      
               WHEN CCUP-CHANGES-OKAYED-LOCK-ERROR                              
                    SET INFORM-FAILURE             TO TRUE                      
               WHEN CCUP-CHANGES-OKAYED-BUT-FAILED                              
                    SET INFORM-FAILURE             TO TRUE                      
               WHEN WS-NO-INFO-MESSAGE                                          
                   SET PROMPT-FOR-SEARCH-KEYS      TO TRUE                      
           END-EVALUATE                                                         
                                                                                
           MOVE WS-INFO-MSG                    TO INFOMSGO OF CCRDUPAO          
                                                                                
           MOVE WS-RETURN-MSG                  TO ERRMSGO OF CCRDUPAO           
           .                                                                    
       3250-SETUP-INFOMSG-EXIT.                                                 
           EXIT                                                                 
           .                                                                    
       3300-SETUP-SCREEN-ATTRS.                                                 
                                                                                
                                                                                
      *    PROTECT OR UNPROTECT BASED ON CONTEXT                                
           EVALUATE TRUE                                                        
              WHEN CCUP-DETAILS-NOT-FETCHED                                     
                   MOVE DFHBMFSE      TO ACCTSIDA OF CCRDUPAI                   
                                         CARDSIDA OF CCRDUPAI                   
                   MOVE DFHBMPRF      TO CRDNAMEA OF CCRDUPAI                   
                                         CRDSTCDA OF CCRDUPAI                   
      *                                  EXPDAYA  OF CCRDUPAI                   
                                         EXPMONA  OF CCRDUPAI                   
                                         EXPYEARA OF CCRDUPAI                   
              WHEN  CCUP-SHOW-DETAILS                                           
              WHEN  CCUP-CHANGES-NOT-OK                                         
                   MOVE DFHBMPRF      TO ACCTSIDA OF CCRDUPAI                   
                                         CARDSIDA OF CCRDUPAI                   
      *                                  EXPDAYA  OF CCRDUPAI                   
                   MOVE DFHBMFSE      TO CRDNAMEA OF CCRDUPAI                   
                                         CRDSTCDA OF CCRDUPAI                   
                                                                                
                                         EXPMONA  OF CCRDUPAI                   
                                         EXPYEARA OF CCRDUPAI                   
              WHEN CCUP-CHANGES-OK-NOT-CONFIRMED                                
              WHEN CCUP-CHANGES-OKAYED-AND-DONE                                 
                   MOVE DFHBMPRF      TO ACCTSIDA OF CCRDUPAI                   
                                         CARDSIDA OF CCRDUPAI                   
                                         CRDNAMEA OF CCRDUPAI                   
                                         CRDSTCDA OF CCRDUPAI                   
      *                                  EXPDAYA  OF CCRDUPAI                   
                                         EXPMONA  OF CCRDUPAI                   
                                         EXPYEARA OF CCRDUPAI                   
              WHEN OTHER                                                        
                   MOVE DFHBMFSE      TO ACCTSIDA OF CCRDUPAI                   
                                         CARDSIDA OF CCRDUPAI                   
                   MOVE DFHBMPRF      TO CRDNAMEA OF CCRDUPAI                   
                                         CRDSTCDA OF CCRDUPAI                   
      *                                  EXPDAYA  OF CCRDUPAI                   
                                         EXPMONA  OF CCRDUPAI                   
                                         EXPYEARA OF CCRDUPAI                   
           END-EVALUATE                                                         
                                                                                
      *    POSITION CURSOR                                                      
           EVALUATE TRUE                                                        
              WHEN FOUND-CARDS-FOR-ACCOUNT                                      
              WHEN NO-CHANGES-DETECTED                                          
                  MOVE -1              TO CRDNAMEL OF CCRDUPAI                  
              WHEN FLG-ACCTFILTER-NOT-OK                                        
              WHEN FLG-ACCTFILTER-BLANK                                         
                   MOVE -1             TO ACCTSIDL OF CCRDUPAI                  
              WHEN FLG-CARDFILTER-NOT-OK                                        
              WHEN FLG-CARDFILTER-BLANK                                         
                   MOVE -1             TO CARDSIDL OF CCRDUPAI                  
              WHEN FLG-CARDNAME-NOT-OK                                          
              WHEN FLG-CARDNAME-BLANK                                           
                  MOVE -1              TO CRDNAMEL OF  CCRDUPAI                 
              WHEN FLG-CARDSTATUS-NOT-OK                                        
              WHEN FLG-CARDSTATUS-BLANK                                         
                  MOVE -1              TO CRDSTCDL OF  CCRDUPAI                 
              WHEN FLG-CARDEXPMON-NOT-OK                                        
              WHEN FLG-CARDEXPMON-BLANK                                         
                  MOVE -1              TO EXPMONL  OF  CCRDUPAI                 
              WHEN FLG-CARDEXPYEAR-NOT-OK                                       
              WHEN FLG-CARDEXPYEAR-BLANK                                        
                  MOVE -1              TO EXPYEARL OF  CCRDUPAI                 
              WHEN OTHER                                                        
                  MOVE -1              TO ACCTSIDL OF CCRDUPAI                  
           END-EVALUATE                                                         
                                                                                
      *    SETUP COLOR                                                          
           IF CDEMO-LAST-MAPSET   EQUAL LIT-CCLISTMAPSET                        
              MOVE DFHDFCOL            TO ACCTSIDC OF CCRDUPAO                  
              MOVE DFHDFCOL            TO CARDSIDC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF FLG-ACCTFILTER-NOT-OK                                             
              MOVE DFHRED              TO ACCTSIDC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF  FLG-ACCTFILTER-BLANK                                             
           AND CDEMO-PGM-REENTER                                                
               MOVE '*'                TO ACCTSIDO OF CCRDUPAO                  
               MOVE DFHRED             TO ACCTSIDC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF FLG-CARDFILTER-NOT-OK                                             
              MOVE DFHRED              TO CARDSIDC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF  FLG-CARDFILTER-BLANK                                             
           AND CDEMO-PGM-REENTER                                                
               MOVE '*'                TO CARDSIDO OF CCRDUPAO                  
               MOVE DFHRED             TO CARDSIDC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF FLG-CARDNAME-NOT-OK                                               
           AND CCUP-CHANGES-NOT-OK                                              
              MOVE DFHRED              TO CRDNAMEC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF  FLG-CARDNAME-BLANK                                               
           AND CCUP-CHANGES-NOT-OK                                              
               MOVE '*'                TO CRDNAMEO OF CCRDUPAO                  
               MOVE DFHRED             TO CRDNAMEC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF FLG-CARDSTATUS-NOT-OK                                             
           AND CCUP-CHANGES-NOT-OK                                              
              MOVE DFHRED              TO CRDSTCDC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF  FLG-CARDSTATUS-BLANK                                             
           AND CCUP-CHANGES-NOT-OK                                              
               MOVE '*'                TO CRDSTCDO OF CCRDUPAO                  
               MOVE DFHRED             TO CRDSTCDC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           MOVE DFHBMDAR               TO EXPDAYC  OF CCRDUPAO                  
                                                                                
           IF FLG-CARDEXPMON-NOT-OK                                             
           AND CCUP-CHANGES-NOT-OK                                              
              MOVE DFHRED              TO EXPMONC  OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF  FLG-CARDEXPMON-BLANK                                             
           AND CCUP-CHANGES-NOT-OK                                              
               MOVE '*'                TO EXPMONO  OF CCRDUPAO                  
               MOVE DFHRED             TO EXPMONC  OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF  FLG-CARDEXPYEAR-NOT-OK                                           
           AND CCUP-CHANGES-NOT-OK                                              
              MOVE DFHRED              TO EXPYEARC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF  FLG-CARDEXPYEAR-BLANK                                            
           AND CCUP-CHANGES-NOT-OK                                              
               MOVE '*'                TO EXPYEARO OF CCRDUPAO                  
               MOVE DFHRED             TO EXPYEARC OF CCRDUPAO                  
           END-IF                                                               
                                                                                
           IF  WS-NO-INFO-MESSAGE                                               
               MOVE DFHBMDAR           TO INFOMSGA OF CCRDUPAI                  
           ELSE                                                                 
               MOVE DFHBMBRY           TO INFOMSGA OF CCRDUPAI                  
           END-IF                                                               
                                                                                
           IF PROMPT-FOR-CONFIRMATION                                           
              MOVE DFHBMBRY            TO FKEYSCA  OF CCRDUPAI                  
           END-IF                                                               
           .                                                                    
       3300-SETUP-SCREEN-ATTRS-EXIT.                                            
           EXIT                                                                 
           .                                                                    
                                                                                
                                                                                
       3400-SEND-SCREEN.                                                        
                                                                                
           MOVE LIT-THISMAPSET         TO CCARD-NEXT-MAPSET                     
           MOVE LIT-THISMAP            TO CCARD-NEXT-MAP                        
                                                                                
           EXEC CICS SEND MAP(CCARD-NEXT-MAP)                                   
                          MAPSET(CCARD-NEXT-MAPSET)                             
                          FROM(CCRDUPAO)                                        
                          CURSOR                                                
                          ERASE                                                 
                          FREEKB                                                
                          RESP(WS-RESP-CD)                                      
           END-EXEC                                                             
           .                                                                    
       3400-SEND-SCREEN-EXIT.                                                   
           EXIT                                                                 
           .                                                                    
                                                                                
                                                                                
       9000-READ-DATA.                                                          
                                                                                
           INITIALIZE CCUP-OLD-DETAILS                                          
           MOVE CC-ACCT-ID              TO CCUP-OLD-ACCTID                      
           MOVE CC-CARD-NUM             TO CCUP-OLD-CARDID                      
                                                                                
           PERFORM 9100-GETCARD-BYACCTCARD                                      
              THRU 9100-GETCARD-BYACCTCARD-EXIT                                 
                                                                                
           IF FOUND-CARDS-FOR-ACCOUNT                                           
                                                                                
              MOVE CARD-CVV-CD          TO CCUP-OLD-CVV-CD                      
                                                                                
              INSPECT CARD-EMBOSSED-NAME                                        
              CONVERTING LIT-LOWER                                              
                      TO LIT-UPPER                                              
                                                                                
              MOVE CARD-EMBOSSED-NAME   TO CCUP-OLD-CRDNAME                     
              MOVE CARD-EXPIRAION-DATE(1:4)                                     
                                        TO CCUP-OLD-EXPYEAR                     
              MOVE CARD-EXPIRAION-DATE(6:2)                                     
                                        TO CCUP-OLD-EXPMON                      
              MOVE CARD-EXPIRAION-DATE(9:2)                                     
                                        TO CCUP-OLD-EXPDAY                      
              MOVE CARD-ACTIVE-STATUS   TO CCUP-OLD-CRDSTCD                     
                                                                                
           END-IF                                                               
           .                                                                    
                                                                                
       9000-READ-DATA-EXIT.                                                     
           EXIT                                                                 
           .                                                                    
                                                                                
       9100-GETCARD-BYACCTCARD.                                                 
      *    Read the Card file                                                   
      *                                                                         
      *    MOVE CC-ACCT-ID-N            TO WS-CARD-RID-ACCT-ID                  
           MOVE CC-CARD-NUM             TO WS-CARD-RID-CARDNUM                  
                                                                                
           EXEC CICS READ                                                       
                FILE      (LIT-CARDFILENAME)                                    
                RIDFLD    (WS-CARD-RID-CARDNUM)                                 
                KEYLENGTH (LENGTH OF WS-CARD-RID-CARDNUM)                       
                INTO      (CARD-RECORD)                                         
                LENGTH    (LENGTH OF CARD-RECORD)                               
                RESP      (WS-RESP-CD)                                          
                RESP2     (WS-REAS-CD)                                          
           END-EXEC                                                             
                                                                                
           EVALUATE WS-RESP-CD                                                  
               WHEN DFHRESP(NORMAL)                                             
                  SET FOUND-CARDS-FOR-ACCOUNT TO TRUE                           
               WHEN DFHRESP(NOTFND)                                             
                  SET INPUT-ERROR                    TO TRUE                    
                  SET FLG-ACCTFILTER-NOT-OK          TO TRUE                    
                  SET FLG-CARDFILTER-NOT-OK          TO TRUE                    
                  IF  WS-RETURN-MSG-OFF                                         
                     SET DID-NOT-FIND-ACCTCARD-COMBO TO TRUE                    
                  END-IF                                                        
               WHEN OTHER                                                       
                  SET INPUT-ERROR                    TO TRUE                    
                  IF  WS-RETURN-MSG-OFF                                         
                      SET FLG-ACCTFILTER-NOT-OK      TO TRUE                    
                  END-IF                                                        
                  MOVE 'READ'                        TO ERROR-OPNAME            
                  MOVE LIT-CARDFILENAME              TO ERROR-FILE              
                  MOVE WS-RESP-CD                    TO ERROR-RESP              
                  MOVE WS-REAS-CD                    TO ERROR-RESP2             
                  MOVE WS-FILE-ERROR-MESSAGE         TO WS-RETURN-MSG           
           END-EVALUATE                                                         
           .                                                                    
                                                                                
       9100-GETCARD-BYACCTCARD-EXIT.                                            
           EXIT                                                                 
           .                                                                    
                                                                                
                                                                                
       9200-WRITE-PROCESSING.                                                   
                                                                                
      *    Read the Card file                                                   
      *                                                                         
      *    MOVE CC-ACCT-ID-N            TO WS-CARD-RID-ACCT-ID                  
           MOVE CC-CARD-NUM             TO WS-CARD-RID-CARDNUM                  
                                                                                
           EXEC CICS READ                                                       
                FILE      (LIT-CARDFILENAME)                                    
                UPDATE                                                          
                RIDFLD    (WS-CARD-RID-CARDNUM)                                 
                KEYLENGTH (LENGTH OF WS-CARD-RID-CARDNUM)                       
                INTO      (CARD-RECORD)                                         
                LENGTH    (LENGTH OF CARD-RECORD)                               
                RESP      (WS-RESP-CD)                                          
                RESP2     (WS-REAS-CD)                                          
           END-EXEC                                                             
                                                                                
      *****************************************************************         
      *    Could we lock the record ?                                           
      *****************************************************************         
           IF WS-RESP-CD EQUAL TO DFHRESP(NORMAL)                               
              CONTINUE                                                          
           ELSE                                                                 
              SET INPUT-ERROR                    TO TRUE                        
              IF  WS-RETURN-MSG-OFF                                             
                  SET COULD-NOT-LOCK-FOR-UPDATE  TO TRUE                        
              END-IF                                                            
              GO TO 9200-WRITE-PROCESSING-EXIT                                  
           END-IF                                                               
      *****************************************************************         
      *    Did someone change the record while we were out ?                    
      *****************************************************************         
           PERFORM 9300-CHECK-CHANGE-IN-REC                                     
              THRU 9300-CHECK-CHANGE-IN-REC-EXIT                                
           IF DATA-WAS-CHANGED-BEFORE-UPDATE                                    
              GO TO 9200-WRITE-PROCESSING-EXIT                                  
           END-IF                                                               
      *****************************************************************         
      * Prepare the update                                                      
      *****************************************************************         
           INITIALIZE CARD-UPDATE-RECORD                                        
           MOVE CCUP-NEW-CARDID             TO CARD-UPDATE-NUM                  
           MOVE CC-ACCT-ID-N                TO CARD-UPDATE-ACCT-ID              
           MOVE CCUP-NEW-CVV-CD             TO CARD-CVV-CD-X                    
           MOVE CARD-CVV-CD-N               TO CARD-UPDATE-CVV-CD               
           MOVE CCUP-NEW-CRDNAME            TO CARD-UPDATE-EMBOSSED-NAME        
           STRING  CCUP-NEW-EXPYEAR                                             
                   '-'                                                          
                   CCUP-NEW-EXPMON                                              
                   '-'                                                          
                   CCUP-NEW-EXPDAY                                              
                   DELIMITED BY SIZE                                            
              INTO CARD-UPDATE-EXPIRAION-DATE                                   
           END-STRING                                                           
           MOVE CCUP-NEW-CRDSTCD            TO CARD-UPDATE-ACTIVE-STATUS        
                                                                                
           EXEC CICS                                                            
                REWRITE FILE(LIT-CARDFILENAME)                                  
                        FROM(CARD-UPDATE-RECORD)                                
                        LENGTH(LENGTH OF CARD-UPDATE-RECORD)                    
                        RESP      (WS-RESP-CD)                                  
                        RESP2     (WS-REAS-CD)                                  
           END-EXEC.                                                            
                                                                                
      *****************************************************************         
      * Did the update succeed ?  *                                             
      *****************************************************************         
           IF WS-RESP-CD EQUAL TO DFHRESP(NORMAL)                               
             CONTINUE                                                           
           ELSE                                                                 
             SET LOCKED-BUT-UPDATE-FAILED    TO TRUE                            
           END-IF                                                               
           .                                                                    
       9200-WRITE-PROCESSING-EXIT.                                              
           EXIT                                                                 
           .                                                                    
                                                                                
       9300-CHECK-CHANGE-IN-REC.                                                
           INSPECT CARD-EMBOSSED-NAME                                           
           CONVERTING LIT-LOWER                                                 
                   TO LIT-UPPER                                                 
                                                                                
           IF  CARD-CVV-CD              EQUAL  TO CCUP-OLD-CVV-CD               
           AND CARD-EMBOSSED-NAME       EQUAL  TO CCUP-OLD-CRDNAME              
           AND CARD-EXPIRAION-DATE(1:4) EQUAL  TO CCUP-OLD-EXPYEAR              
           AND CARD-EXPIRAION-DATE(6:2) EQUAL  TO CCUP-OLD-EXPMON               
           AND CARD-EXPIRAION-DATE(9:2) EQUAL  TO CCUP-OLD-EXPDAY               
           AND CARD-ACTIVE-STATUS       EQUAL  TO CCUP-OLD-CRDSTCD              
               CONTINUE                                                         
           ELSE                                                                 
              SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE                        
              MOVE CARD-CVV-CD                 TO CCUP-OLD-CVV-CD               
              MOVE CARD-EMBOSSED-NAME          TO CCUP-OLD-CRDNAME              
              MOVE CARD-EXPIRAION-DATE(1:4)    TO CCUP-OLD-EXPYEAR              
              MOVE CARD-EXPIRAION-DATE(6:2)    TO CCUP-OLD-EXPMON               
              MOVE CARD-EXPIRAION-DATE(9:2)    TO CCUP-OLD-EXPDAY               
              MOVE CARD-ACTIVE-STATUS          TO CCUP-OLD-CRDSTCD              
              GO TO 9200-WRITE-PROCESSING-EXIT                                  
           END-IF EXIT                                                          
           .                                                                    
       9300-CHECK-CHANGE-IN-REC-EXIT.                                           
           EXIT                                                                 
           .                                                                    
                                                                                
      ******************************************************************        
      *Common code to store PFKey
      ******************************************************************
       COPY 'CSSTRPFY'
           .                                                           
                                                                        340000
       ABEND-ROUTINE.                                                           
                                                                                
           IF ABEND-MSG EQUAL LOW-VALUES                                        
              MOVE 'UNEXPECTED ABEND OCCURRED.' TO ABEND-MSG                    
           END-IF                                                               
                                                                                
           MOVE LIT-THISPGM       TO ABEND-CULPRIT                              
                                                                                
           EXEC CICS SEND                                                       
                            FROM (ABEND-DATA)                                   
                            LENGTH(LENGTH OF ABEND-DATA)                        
                            NOHANDLE                                            
                            ERASE                                               
           END-EXEC                                                             
                                                                                
           EXEC CICS HANDLE ABEND                                               
                CANCEL                                                          
           END-EXEC                                                             
                                                                                
           EXEC CICS ABEND                                                      
                ABCODE('9999')                                                  
           END-EXEC                                                             
           .                                                                    
       ABEND-ROUTINE-EXIT.                                                      
           EXIT                                                                 
           .                                                                    
                                                                                
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:33 CDT
      *
