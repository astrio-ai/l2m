      ******************************************************************
      * Program     : CBACT04C.CBL                                      
      * Application : CardDemo                                          
      * Type        : BATCH COBOL Program                                
      * Function    : This is a interest calculator program.
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
       PROGRAM-ID.    CBACT04C.                                                 
       AUTHOR.        AWS.                                                      
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT TCATBAL-FILE ASSIGN TO TCATBALF                               
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS SEQUENTIAL                                    
                  RECORD KEY   IS FD-TRAN-CAT-KEY                               
                  FILE STATUS  IS TCATBALF-STATUS.                              
                                                                                
           SELECT XREF-FILE ASSIGN TO   XREFFILE                                
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-XREF-CARD-NUM                              
                  ALTERNATE RECORD KEY IS FD-XREF-ACCT-ID                       
                  FILE STATUS  IS XREFFILE-STATUS.                              
                                                                                
           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE                               
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-ACCT-ID                                    
                  FILE STATUS  IS ACCTFILE-STATUS.                              
                                                                                
           SELECT DISCGRP-FILE ASSIGN TO DISCGRP                                
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-DISCGRP-KEY                                
                  FILE STATUS  IS DISCGRP-STATUS.                               
                                                                                
           SELECT TRANSACT-FILE ASSIGN TO TRANSACT                              
                  ORGANIZATION IS SEQUENTIAL                                    
                  ACCESS MODE  IS SEQUENTIAL                                    
                  FILE STATUS  IS TRANFILE-STATUS.                              
                                                                                
      *                                                                         
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  TCATBAL-FILE.                                                        
       01  FD-TRAN-CAT-BAL-RECORD.                                              
           05 FD-TRAN-CAT-KEY.                                                  
              10 FD-TRANCAT-ACCT-ID             PIC 9(11).                      
              10 FD-TRANCAT-TYPE-CD             PIC X(02).                      
              10 FD-TRANCAT-CD                  PIC 9(04).                      
           05 FD-FD-TRAN-CAT-DATA               PIC X(33).                      
                                                                                
       FD  XREF-FILE.                                                           
       01  FD-XREFFILE-REC.                                                     
           05 FD-XREF-CARD-NUM                  PIC X(16).                      
           05 FD-XREF-CUST-NUM                  PIC 9(09).                      
           05 FD-XREF-ACCT-ID                   PIC 9(11).                      
           05 FD-XREF-FILLER                    PIC X(14).                      
                                                                                
       FD  DISCGRP-FILE.                                                        
       01  FD-DISCGRP-REC.                                                      
           05 FD-DISCGRP-KEY.                                                   
              10 FD-DIS-ACCT-GROUP-ID           PIC X(10).                      
              10 FD-DIS-TRAN-TYPE-CD            PIC X(02).                      
              10 FD-DIS-TRAN-CAT-CD             PIC 9(04).                      
           05 FD-DISCGRP-DATA                   PIC X(34).                      
                                                                                
       FD  ACCOUNT-FILE.                                                        
       01  FD-ACCTFILE-REC.                                                     
           05 FD-ACCT-ID                        PIC 9(11).                      
           05 FD-ACCT-DATA                      PIC X(289).                     
                                                                                
       FD  TRANSACT-FILE.                                                       
       01  FD-TRANFILE-REC.                                                     
           05 FD-TRANS-ID                       PIC X(16).                      
           05 FD-ACCT-DATA                      PIC X(334).                     
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *****************************************************************         
       COPY CVTRA01Y.                                                           
       01  TCATBALF-STATUS.                                                     
           05  TCATBALF-STAT1      PIC X.                                       
           05  TCATBALF-STAT2      PIC X.                                       
                                                                                
       COPY CVACT03Y.                                                           
       01  XREFFILE-STATUS.                                                     
           05  XREFFILE-STAT1      PIC X.                                       
           05  XREFFILE-STAT2      PIC X.                                       
                                                                                
       COPY CVTRA02Y.                                                           
       01  DISCGRP-STATUS.                                                      
           05 DISCGRP-STAT1        PIC X.                                       
           05 DISCGRP-STAT2        PIC X.                                       
                                                                                
       COPY CVACT01Y.                                                           
       01  ACCTFILE-STATUS.                                                     
           05  ACCTFILE-STAT1      PIC X.                                       
           05  ACCTFILE-STAT2      PIC X.                                       
                                                                                
       COPY CVTRA05Y.                                                           
       01  TRANFILE-STATUS.                                                     
           05  TRANFILE-STAT1      PIC X.                                       
           05  TRANFILE-STAT2      PIC X.                                       
                                                                                
       01  IO-STATUS.                                                           
           05  IO-STAT1            PIC X.                                       
           05  IO-STAT2            PIC X.                                       
       01  TWO-BYTES-BINARY        PIC 9(4) BINARY.                             
       01  TWO-BYTES-ALPHA         REDEFINES TWO-BYTES-BINARY.                  
           05  TWO-BYTES-LEFT      PIC X.                                       
           05  TWO-BYTES-RIGHT     PIC X.                                       
       01  IO-STATUS-04.                                                        
           05  IO-STATUS-0401      PIC 9   VALUE 0.                             
           05  IO-STATUS-0403      PIC 999 VALUE 0.                             
                                                                                
       01  APPL-RESULT             PIC S9(9)   COMP.                            
           88  APPL-AOK            VALUE 0.                                     
           88  APPL-EOF            VALUE 16.                                    
                                                                                
       01  END-OF-FILE             PIC X(01)    VALUE 'N'.                      
       01  ABCODE                  PIC S9(9) BINARY.                            
       01  TIMING                  PIC S9(9) BINARY.                            
      * T I M E S T A M P   D B 2  X(26)     EEEE-MM-DD-UU.MM.SS.HH0000         
       01  COBOL-TS.                                                            
           05 COB-YYYY                  PIC X(04).                              
           05 COB-MM                    PIC X(02).                              
           05 COB-DD                    PIC X(02).                              
           05 COB-HH                    PIC X(02).                              
           05 COB-MIN                   PIC X(02).                              
           05 COB-SS                    PIC X(02).                              
           05 COB-MIL                   PIC X(02).                              
           05 COB-REST                  PIC X(05).                              
       01  DB2-FORMAT-TS                PIC X(26).                              
       01  FILLER REDEFINES DB2-FORMAT-TS.                                      
           06 DB2-YYYY                  PIC X(004).                      E      
           06 DB2-STREEP-1              PIC X.                           -      
           06 DB2-MM                    PIC X(002).                      M      
           06 DB2-STREEP-2              PIC X.                           -      
           06 DB2-DD                    PIC X(002).                      D      
           06 DB2-STREEP-3              PIC X.                           -      
           06 DB2-HH                    PIC X(002).                      U      
           06 DB2-DOT-1                 PIC X.                                  
           06 DB2-MIN                   PIC X(002).                             
           06 DB2-DOT-2                 PIC X.                                  
           06 DB2-SS                    PIC X(002).                             
           06 DB2-DOT-3                 PIC X.                                  
           06 DB2-MIL                   PIC 9(002).                             
           06 DB2-REST                  PIC X(04).                              
       01 WS-MISC-VARS.                                                         
           05 WS-LAST-ACCT-NUM          PIC X(11) VALUE SPACES.                 
           05 WS-MONTHLY-INT            PIC S9(09)V99.                          
           05 WS-TOTAL-INT              PIC S9(09)V99.                          
           05 WS-FIRST-TIME             PIC X(01) VALUE 'Y'.                    
       01 WS-COUNTERS.                                                          
           05 WS-RECORD-COUNT           PIC 9(09) VALUE 0.                      
           05 WS-TRANID-SUFFIX          PIC 9(06) VALUE 0.                      
                                                                                
       LINKAGE SECTION.                                                         
       01  EXTERNAL-PARMS.                                                      
           05  PARM-LENGTH         PIC S9(04) COMP.                             
           05  PARM-DATE           PIC X(10).                                   
      *****************************************************************         
       PROCEDURE DIVISION USING EXTERNAL-PARMS.                                 
           DISPLAY 'START OF EXECUTION OF PROGRAM CBACT04C'.                    
           PERFORM 0000-TCATBALF-OPEN.                                          
           PERFORM 0100-XREFFILE-OPEN.                                          
           PERFORM 0200-DISCGRP-OPEN.                                           
           PERFORM 0300-ACCTFILE-OPEN.                                          
           PERFORM 0400-TRANFILE-OPEN.                                          
                                                                                
           PERFORM UNTIL END-OF-FILE = 'Y'                                      
               IF  END-OF-FILE = 'N'                                            
                   PERFORM 1000-TCATBALF-GET-NEXT                               
                   IF  END-OF-FILE = 'N'                                        
                     ADD 1 TO WS-RECORD-COUNT                                   
                     DISPLAY TRAN-CAT-BAL-RECORD                                
                     IF TRANCAT-ACCT-ID NOT= WS-LAST-ACCT-NUM                   
                       IF WS-FIRST-TIME NOT = 'Y'                               
                          PERFORM 1050-UPDATE-ACCOUNT                           
                       ELSE                                                     
                          MOVE 'N' TO WS-FIRST-TIME                             
                       END-IF                                                   
                       MOVE 0 TO WS-TOTAL-INT                                   
                       MOVE TRANCAT-ACCT-ID TO WS-LAST-ACCT-NUM                 
                       MOVE TRANCAT-ACCT-ID TO FD-ACCT-ID                       
                       PERFORM 1100-GET-ACCT-DATA                               
                       MOVE TRANCAT-ACCT-ID TO FD-XREF-ACCT-ID                  
                       PERFORM 1110-GET-XREF-DATA                               
                     END-IF                                                     
      *              DISPLAY 'ACCT-GROUP-ID: ' ACCT-GROUP-ID                    
      *              DISPLAY 'TRANCAT-CD: ' TRANCAT-CD                          
      *              DISPLAY 'TRANCAT-TYPE-CD: ' TRANCAT-TYPE-CD                
                     MOVE ACCT-GROUP-ID TO FD-DIS-ACCT-GROUP-ID                 
                     MOVE TRANCAT-CD TO FD-DIS-TRAN-CAT-CD                      
                     MOVE TRANCAT-TYPE-CD TO FD-DIS-TRAN-TYPE-CD                
                     PERFORM 1200-GET-INTEREST-RATE                             
                     IF DIS-INT-RATE NOT = 0                                    
                       PERFORM 1300-COMPUTE-INTEREST                            
                       PERFORM 1400-COMPUTE-FEES                                
                     END-IF                                                     
                   END-IF                                                       
               ELSE                                                             
                    PERFORM 1050-UPDATE-ACCOUNT                                 
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           PERFORM 9000-TCATBALF-CLOSE.                                         
           PERFORM 9100-XREFFILE-CLOSE.                                         
           PERFORM 9200-DISCGRP-CLOSE.                                          
           PERFORM 9300-ACCTFILE-CLOSE.                                         
           PERFORM 9400-TRANFILE-CLOSE.                                         
                                                                                
           DISPLAY 'END OF EXECUTION OF PROGRAM CBACT04C'.                      
                                                                                
           GOBACK.                                                              
      *---------------------------------------------------------------*         
       0000-TCATBALF-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT TCATBAL-FILE                                              
           IF  TCATBALF-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRANSACTION CATEGORY BALANCE'             
               MOVE TCATBALF-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0100-XREFFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT XREF-FILE                                                 
           IF  XREFFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING CROSS REF FILE'   XREFFILE-STATUS         
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0200-DISCGRP-OPEN.                                                       
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT DISCGRP-FILE                                              
           IF  DISCGRP-STATUS = '00'                                            
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING DALY REJECTS FILE'                        
               MOVE DISCGRP-STATUS TO IO-STATUS                                 
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       0300-ACCTFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN I-O ACCOUNT-FILE                                                
           IF  ACCTFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING ACCOUNT MASTER FILE'                      
               MOVE ACCTFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0400-TRANFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN OUTPUT TRANSACT-FILE                                            
           IF  TRANFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRANSACTION FILE'                         
               MOVE TRANFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1000-TCATBALF-GET-NEXT.                                                  
           READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD.                          
           IF  TCATBALF-STATUS  = '00'                                          
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               IF  TCATBALF-STATUS  = '10'                                      
                   MOVE 16 TO APPL-RESULT                                       
               ELSE                                                             
                   MOVE 12 TO APPL-RESULT                                       
               END-IF                                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               IF  APPL-EOF                                                     
                   MOVE 'Y' TO END-OF-FILE                                      
               ELSE                                                             
                   DISPLAY 'ERROR READING TRANSACTION CATEGORY FILE'            
                   MOVE TCATBALF-STATUS TO IO-STATUS                            
                   PERFORM 9910-DISPLAY-IO-STATUS                               
                   PERFORM 9999-ABEND-PROGRAM                                   
               END-IF                                                           
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1050-UPDATE-ACCOUNT.                                                     
      * Update the balances in account record to reflect posted trans.          
           ADD WS-TOTAL-INT  TO ACCT-CURR-BAL                                   
           MOVE 0 TO ACCT-CURR-CYC-CREDIT                                       
           MOVE 0 TO ACCT-CURR-CYC-DEBIT                                        
                                                                                
           REWRITE FD-ACCTFILE-REC FROM  ACCOUNT-RECORD                         
           IF  ACCTFILE-STATUS  = '00'                                          
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR RE-WRITING ACCOUNT FILE'                          
               MOVE ACCTFILE-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1100-GET-ACCT-DATA.                                                      
           READ ACCOUNT-FILE INTO ACCOUNT-RECORD                                
               INVALID KEY                                                      
                  DISPLAY 'ACCOUNT NOT FOUND: ' FD-ACCT-ID                      
           END-READ                                                             
                                                                                
           IF  ACCTFILE-STATUS  = '00'                                          
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR READING ACCOUNT FILE'                             
               MOVE ACCTFILE-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1110-GET-XREF-DATA.                                                      
           READ XREF-FILE INTO CARD-XREF-RECORD                                 
            KEY IS FD-XREF-ACCT-ID                                              
               INVALID KEY                                                      
                  DISPLAY 'ACCOUNT NOT FOUND: ' FD-XREF-ACCT-ID                 
           END-READ                                                             
                                                                                
           IF  XREFFILE-STATUS   = '00'                                         
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR READING XREF FILE'                                
               MOVE XREFFILE-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1200-GET-INTEREST-RATE.                                                  
           READ DISCGRP-FILE INTO DIS-GROUP-RECORD                              
                INVALID KEY                                                     
                   DISPLAY 'DISCLOSURE GROUP RECORD MISSING'                    
                   DISPLAY 'TRY WITH DEFAULT GROUP CODE'                        
           END-READ.                                                            
                                                                                
           IF  DISCGRP-STATUS  = '00'  OR '23'                                  
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
                                                                                
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR READING DISCLOSURE GROUP FILE'                    
               MOVE DISCGRP-STATUS  TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           IF  DISCGRP-STATUS  = '23'                                           
               MOVE 'DEFAULT' TO FD-DIS-ACCT-GROUP-ID                           
               PERFORM 1200-A-GET-DEFAULT-INT-RATE                              
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       1200-A-GET-DEFAULT-INT-RATE.                                             
           READ DISCGRP-FILE INTO DIS-GROUP-RECORD                              
                                                                                
           IF  DISCGRP-STATUS  = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
                                                                                
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR READING DEFAULT DISCLOSURE GROUP'                 
               MOVE DISCGRP-STATUS  TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1300-COMPUTE-INTEREST.                                                   
                                                                                
           COMPUTE WS-MONTHLY-INT                                               
            = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200                             
                                                                                
           ADD WS-MONTHLY-INT  TO WS-TOTAL-INT                                  
           PERFORM 1300-B-WRITE-TX.                                             
                                                                                
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       1300-B-WRITE-TX.                                                         
           ADD 1 TO WS-TRANID-SUFFIX                                            
                                                                                
           STRING PARM-DATE,                                                    
                  WS-TRANID-SUFFIX                                              
             DELIMITED BY SIZE                                                  
             INTO TRAN-ID                                                       
           END-STRING.                                                          
                                                                                
           MOVE '01'                 TO TRAN-TYPE-CD                            
           MOVE '05'                 TO TRAN-CAT-CD                             
           MOVE 'System'             TO TRAN-SOURCE                             
           STRING 'Int. for a/c ' ,                                             
                  ACCT-ID                                                       
                  DELIMITED BY SIZE                                             
            INTO TRAN-DESC                                                      
           END-STRING                                                           
           MOVE WS-MONTHLY-INT       TO TRAN-AMT                                
           MOVE 0                    TO TRAN-MERCHANT-ID                        
           MOVE SPACES               TO TRAN-MERCHANT-NAME                      
           MOVE SPACES               TO TRAN-MERCHANT-CITY                      
           MOVE SPACES               TO TRAN-MERCHANT-ZIP                       
           MOVE XREF-CARD-NUM        TO TRAN-CARD-NUM                           
           PERFORM Z-GET-DB2-FORMAT-TIMESTAMP                                   
           MOVE DB2-FORMAT-TS        TO TRAN-ORIG-TS                            
           MOVE DB2-FORMAT-TS        TO TRAN-PROC-TS                            
                                                                                
           WRITE FD-TRANFILE-REC FROM TRAN-RECORD                               
           IF  TRANFILE-STATUS   = '00'                                         
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
                                                                                
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TRANSACTION RECORD'                       
               MOVE TRANFILE-STATUS   TO IO-STATUS                              
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       1400-COMPUTE-FEES.                                                       
      * To be implemented                                                       
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9000-TCATBALF-CLOSE.                                                     
           MOVE 8 TO  APPL-RESULT.                                              
           CLOSE TCATBAL-FILE                                                   
           IF  TCATBALF-STATUS = '00'                                           
               MOVE 0 TO  APPL-RESULT                                           
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING TRANSACTION BALANCE FILE'                 
               MOVE TCATBALF-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       9100-XREFFILE-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE XREF-FILE                                                      
           IF  XREFFILE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING CROSS REF FILE'                           
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9200-DISCGRP-CLOSE.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE DISCGRP-FILE                                                   
           IF  DISCGRP-STATUS = '00'                                            
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING DISCLOSURE GROUP FILE'                    
               MOVE DISCGRP-STATUS TO IO-STATUS                                 
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9300-ACCTFILE-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE ACCOUNT-FILE                                                   
           IF  ACCTFILE-STATUS  = '00'                                          
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING ACCOUNT FILE'                             
               MOVE ACCTFILE-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
       9400-TRANFILE-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE TRANSACT-FILE                                                  
           IF  TRANFILE-STATUS  = '00'                                          
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING TRANSACTION FILE'                         
               MOVE TRANFILE-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
       Z-GET-DB2-FORMAT-TIMESTAMP.                                              
           MOVE FUNCTION CURRENT-DATE TO COBOL-TS                               
           MOVE COB-YYYY TO DB2-YYYY                                            
           MOVE COB-MM   TO DB2-MM                                              
           MOVE COB-DD   TO DB2-DD                                              
           MOVE COB-HH   TO DB2-HH                                              
           MOVE COB-MIN  TO DB2-MIN                                             
           MOVE COB-SS   TO DB2-SS                                              
           MOVE COB-MIL  TO DB2-MIL                                             
           MOVE '0000'   TO DB2-REST                                            
           MOVE '-' TO DB2-STREEP-1 DB2-STREEP-2 DB2-STREEP-3                   
           MOVE '.' TO DB2-DOT-1 DB2-DOT-2 DB2-DOT-3                            
      *    DISPLAY 'DB2-TIMESTAMP = ' DB2-FORMAT-TS                             
           EXIT.                                                                
                                                                                
       9999-ABEND-PROGRAM.                                                      
           DISPLAY 'ABENDING PROGRAM'                                           
           MOVE 0 TO TIMING                                                     
           MOVE 999 TO ABCODE                                                   
           CALL 'CEE3ABD'.                                                      
                                                                                
      *****************************************************************         
       9910-DISPLAY-IO-STATUS.                                                  
           IF  IO-STATUS NOT NUMERIC                                            
           OR  IO-STAT1 = '9'                                                   
               MOVE IO-STAT1 TO IO-STATUS-04(1:1)                               
               MOVE 0        TO TWO-BYTES-BINARY                                
               MOVE IO-STAT2 TO TWO-BYTES-RIGHT                                 
               MOVE TWO-BYTES-BINARY TO IO-STATUS-0403                          
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04                      
           ELSE                                                                 
               MOVE '0000' TO IO-STATUS-04                                      
               MOVE IO-STATUS TO IO-STATUS-04(3:2)                              
               DISPLAY 'FILE STATUS IS: NNNN' IO-STATUS-04                      
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:31 CDT
      *
