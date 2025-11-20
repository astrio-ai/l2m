      ******************************************************************
      * Program     : CBTRN02C.CBL                                      
      * Application : CardDemo                                          
      * Type        : BATCH COBOL Program                                
      * Function    : Post the records from daily transaction file.     
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
       PROGRAM-ID.    CBTRN02C.                                                 
       AUTHOR.        AWS.                                                      
                                                                                
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT DALYTRAN-FILE ASSIGN TO DALYTRAN                              
                  ORGANIZATION IS SEQUENTIAL                                    
                  ACCESS MODE  IS SEQUENTIAL                                    
                  FILE STATUS  IS DALYTRAN-STATUS.                              
                                                                                
           SELECT TRANSACT-FILE ASSIGN TO TRANFILE                              
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-TRANS-ID                                   
                  FILE STATUS  IS TRANFILE-STATUS.                              
                                                                                
           SELECT XREF-FILE ASSIGN TO   XREFFILE                                
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-XREF-CARD-NUM                              
                  FILE STATUS  IS XREFFILE-STATUS.                              
                                                                                
           SELECT DALYREJS-FILE ASSIGN TO DALYREJS                              
                  ORGANIZATION IS SEQUENTIAL                                    
                  ACCESS MODE  IS SEQUENTIAL                                    
                  FILE STATUS  IS DALYREJS-STATUS.                              
                                                                                
           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE                               
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-ACCT-ID                                    
                  FILE STATUS  IS ACCTFILE-STATUS.                              
                                                                                
           SELECT TCATBAL-FILE ASSIGN TO TCATBALF                               
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-TRAN-CAT-KEY                               
                  FILE STATUS  IS TCATBALF-STATUS.                              
                                                                                
      *                                                                         
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  DALYTRAN-FILE.                                                       
       01  FD-TRAN-RECORD.                                                      
           05 FD-TRAN-ID                        PIC X(16).                      
           05 FD-CUST-DATA                      PIC X(334).                     
                                                                                
       FD  TRANSACT-FILE.                                                       
       01  FD-TRANFILE-REC.                                                     
           05 FD-TRANS-ID                       PIC X(16).                      
           05 FD-ACCT-DATA                      PIC X(334).                     
                                                                                
       FD  XREF-FILE.                                                           
       01  FD-XREFFILE-REC.                                                     
           05 FD-XREF-CARD-NUM                  PIC X(16).                      
           05 FD-XREF-DATA                      PIC X(34).                      
                                                                                
       FD  DALYREJS-FILE.                                                       
       01  FD-REJS-RECORD.                                                      
           05 FD-REJECT-RECORD                  PIC X(350).                     
           05 FD-VALIDATION-TRAILER             PIC X(80).                      
                                                                                
       FD  ACCOUNT-FILE.                                                        
       01  FD-ACCTFILE-REC.                                                     
           05 FD-ACCT-ID                        PIC 9(11).                      
           05 FD-ACCT-DATA                      PIC X(289).                     
                                                                                
       FD  TCATBAL-FILE.                                                        
       01  FD-TRAN-CAT-BAL-RECORD.                                              
           05 FD-TRAN-CAT-KEY.                                                  
              10 FD-TRANCAT-ACCT-ID             PIC 9(11).                      
              10 FD-TRANCAT-TYPE-CD             PIC X(02).                      
              10 FD-TRANCAT-CD                  PIC 9(04).                      
           05 FD-FD-TRAN-CAT-DATA               PIC X(33).                      
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *****************************************************************         
       COPY CVTRA06Y.                                                           
       01  DALYTRAN-STATUS.                                                     
           05  DALYTRAN-STAT1      PIC X.                                       
           05  DALYTRAN-STAT2      PIC X.                                       
                                                                                
       COPY CVTRA05Y.                                                           
       01  TRANFILE-STATUS.                                                     
           05  TRANFILE-STAT1      PIC X.                                       
           05  TRANFILE-STAT2      PIC X.                                       
                                                                                
       COPY CVACT03Y.                                                           
       01  XREFFILE-STATUS.                                                     
           05  XREFFILE-STAT1      PIC X.                                       
           05  XREFFILE-STAT2      PIC X.                                       
                                                                                
       01  DALYREJS-STATUS.                                                     
           05  DALYREJS-STAT1      PIC X.                                       
           05  DALYREJS-STAT2      PIC X.                                       
                                                                                
       COPY CVACT01Y.                                                           
       01  ACCTFILE-STATUS.                                                     
           05  ACCTFILE-STAT1      PIC X.                                       
           05  ACCTFILE-STAT2      PIC X.                                       
                                                                                
       COPY CVTRA01Y.                                                           
       01  TCATBALF-STATUS.                                                     
           05  TCATBALF-STAT1      PIC X.                                       
           05  TCATBALF-STAT2      PIC X.                                       
                                                                                
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
                                                                                
        01 REJECT-RECORD.                                                       
           05 REJECT-TRAN-DATA          PIC X(350).                             
           05 VALIDATION-TRAILER        PIC X(80).                              
                                                                                
        01 WS-VALIDATION-TRAILER.                                               
           05 WS-VALIDATION-FAIL-REASON      PIC 9(04).                         
           05 WS-VALIDATION-FAIL-REASON-DESC PIC X(76).                         
                                                                                
        01 WS-COUNTERS.                                                         
           05 WS-TRANSACTION-COUNT          PIC 9(09) VALUE 0.                  
           05 WS-REJECT-COUNT               PIC 9(09) VALUE 0.                  
           05 WS-TEMP-BAL                   PIC S9(09)V99.                      
                                                                                
        01 WS-FLAGS.                                                            
           05 WS-CREATE-TRANCAT-REC         PIC X(01) VALUE 'N'.                
                                                                                
      *****************************************************************         
       PROCEDURE DIVISION.                                                      
           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN02C'.                    
           PERFORM 0000-DALYTRAN-OPEN.                                          
           PERFORM 0100-TRANFILE-OPEN.                                          
           PERFORM 0200-XREFFILE-OPEN.                                          
           PERFORM 0300-DALYREJS-OPEN.                                          
           PERFORM 0400-ACCTFILE-OPEN.                                          
           PERFORM 0500-TCATBALF-OPEN.                                          
                                                                                
           PERFORM UNTIL END-OF-FILE = 'Y'                                      
               IF  END-OF-FILE = 'N'                                            
                   PERFORM 1000-DALYTRAN-GET-NEXT                               
                   IF  END-OF-FILE = 'N'                                        
                     ADD 1 TO WS-TRANSACTION-COUNT                              
      *              DISPLAY DALYTRAN-RECORD                                    
                     MOVE 0 TO WS-VALIDATION-FAIL-REASON                        
                     MOVE SPACES TO WS-VALIDATION-FAIL-REASON-DESC              
                     PERFORM 1500-VALIDATE-TRAN                                 
                     IF WS-VALIDATION-FAIL-REASON = 0                           
                       PERFORM 2000-POST-TRANSACTION                            
                     ELSE                                                       
                       ADD 1 TO WS-REJECT-COUNT                                 
                       PERFORM 2500-WRITE-REJECT-REC                            
                     END-IF                                                     
                   END-IF                                                       
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           PERFORM 9000-DALYTRAN-CLOSE.                                         
           PERFORM 9100-TRANFILE-CLOSE.                                         
           PERFORM 9200-XREFFILE-CLOSE.                                         
           PERFORM 9300-DALYREJS-CLOSE.                                         
           PERFORM 9400-ACCTFILE-CLOSE.                                         
           PERFORM 9500-TCATBALF-CLOSE.                                         
           DISPLAY 'TRANSACTIONS PROCESSED :' WS-TRANSACTION-COUNT              
           DISPLAY 'TRANSACTIONS REJECTED  :' WS-REJECT-COUNT                   
           IF WS-REJECT-COUNT > 0                                               
              MOVE 4 TO RETURN-CODE                                             
           END-IF                                                               
           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN02C'.                      
                                                                                
           GOBACK.                                                              
      *---------------------------------------------------------------*         
       0000-DALYTRAN-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT DALYTRAN-FILE                                             
           IF  DALYTRAN-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING DALYTRAN'                                 
               MOVE DALYTRAN-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0100-TRANFILE-OPEN.                                                      
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
       0200-XREFFILE-OPEN.                                                      
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
               DISPLAY 'ERROR OPENING CROSS REF FILE'                           
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0300-DALYREJS-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN OUTPUT DALYREJS-FILE                                            
           IF  DALYREJS-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING DALY REJECTS FILE'                        
               MOVE DALYREJS-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0400-ACCTFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN I-O  ACCOUNT-FILE                                               
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
       0500-TCATBALF-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN I-O  TCATBAL-FILE                                               
           IF  TCATBALF-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRANSACTION BALANCE FILE'                 
               MOVE TCATBALF-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1000-DALYTRAN-GET-NEXT.                                                  
           READ DALYTRAN-FILE INTO DALYTRAN-RECORD.                             
           IF  DALYTRAN-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
      *        DISPLAY DALYTRAN-RECORD                                          
           ELSE                                                                 
               IF  DALYTRAN-STATUS = '10'                                       
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
                   DISPLAY 'ERROR READING DALYTRAN FILE'                        
                   MOVE DALYTRAN-STATUS TO IO-STATUS                            
                   PERFORM 9910-DISPLAY-IO-STATUS                               
                   PERFORM 9999-ABEND-PROGRAM                                   
               END-IF                                                           
           END-IF                                                               
           EXIT.                                                                
       1500-VALIDATE-TRAN.                                                      
           PERFORM 1500-A-LOOKUP-XREF.                                          
           IF WS-VALIDATION-FAIL-REASON = 0                                     
              PERFORM 1500-B-LOOKUP-ACCT                                        
           ELSE                                                                 
              CONTINUE                                                          
           END-IF                                                               
      * ADD MORE VALIDATIONS HERE                                               
           EXIT.                                                                
                                                                                
       1500-A-LOOKUP-XREF.                                                      
      *    DISPLAY 'CARD NUMBER: ' DALYTRAN-CARD-NUM                            
           MOVE DALYTRAN-CARD-NUM TO FD-XREF-CARD-NUM                           
           READ XREF-FILE INTO CARD-XREF-RECORD                                 
              INVALID KEY                                                       
                MOVE 100 TO WS-VALIDATION-FAIL-REASON                           
                MOVE 'INVALID CARD NUMBER FOUND'                                
                  TO WS-VALIDATION-FAIL-REASON-DESC                             
              NOT INVALID KEY                                                   
      *           DISPLAY 'ACCOUNT RECORD FOUND'                                
                  CONTINUE                                                      
           END-READ                                                             
           EXIT.                                                                
       1500-B-LOOKUP-ACCT.                                                      
           MOVE XREF-ACCT-ID TO FD-ACCT-ID                                      
           READ ACCOUNT-FILE INTO ACCOUNT-RECORD                                
              INVALID KEY                                                       
                MOVE 101 TO WS-VALIDATION-FAIL-REASON                           
                MOVE 'ACCOUNT RECORD NOT FOUND'                                 
                  TO WS-VALIDATION-FAIL-REASON-DESC                             
              NOT INVALID KEY                                                   
      *         DISPLAY 'ACCT-CREDIT-LIMIT:' ACCT-CREDIT-LIMIT                  
      *         DISPLAY 'TRAN-AMT         :' DALYTRAN-AMT                       
                COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT                      
                                    - ACCT-CURR-CYC-DEBIT                       
                                    + DALYTRAN-AMT                              
                                                                                
                IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL                             
                  CONTINUE                                                      
                ELSE                                                            
                  MOVE 102 TO WS-VALIDATION-FAIL-REASON                         
                  MOVE 'OVERLIMIT TRANSACTION'                                  
                    TO WS-VALIDATION-FAIL-REASON-DESC                           
                END-IF                                                          
                IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)               
                  CONTINUE                                                      
                ELSE                                                            
                  MOVE 103 TO WS-VALIDATION-FAIL-REASON                         
                  MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'             
                    TO WS-VALIDATION-FAIL-REASON-DESC                           
                END-IF                                                          
           END-READ                                                             
           EXIT.                                                                
      *---------------------------------------------------------------*         
       2000-POST-TRANSACTION.                                                   
           MOVE  DALYTRAN-ID            TO    TRAN-ID                           
           MOVE  DALYTRAN-TYPE-CD       TO    TRAN-TYPE-CD                      
           MOVE  DALYTRAN-CAT-CD        TO    TRAN-CAT-CD                       
           MOVE  DALYTRAN-SOURCE        TO    TRAN-SOURCE                       
           MOVE  DALYTRAN-DESC          TO    TRAN-DESC                         
           MOVE  DALYTRAN-AMT           TO    TRAN-AMT                          
           MOVE  DALYTRAN-MERCHANT-ID   TO    TRAN-MERCHANT-ID                  
           MOVE  DALYTRAN-MERCHANT-NAME TO    TRAN-MERCHANT-NAME                
           MOVE  DALYTRAN-MERCHANT-CITY TO    TRAN-MERCHANT-CITY                
           MOVE  DALYTRAN-MERCHANT-ZIP  TO    TRAN-MERCHANT-ZIP                 
           MOVE  DALYTRAN-CARD-NUM      TO    TRAN-CARD-NUM                     
           MOVE  DALYTRAN-ORIG-TS       TO    TRAN-ORIG-TS                      
           PERFORM Z-GET-DB2-FORMAT-TIMESTAMP                                   
           MOVE  DB2-FORMAT-TS          TO    TRAN-PROC-TS                      
                                                                                
           PERFORM 2700-UPDATE-TCATBAL                                          
           PERFORM 2800-UPDATE-ACCOUNT-REC                                      
           PERFORM 2900-WRITE-TRANSACTION-FILE                                  
                                                                                
           EXIT.                                                                
                                                                                
       2500-WRITE-REJECT-REC.                                                   
           MOVE DALYTRAN-RECORD TO REJECT-TRAN-DATA                             
           MOVE WS-VALIDATION-TRAILER TO VALIDATION-TRAILER                     
      *     DISPLAY '***' REJECT-RECORD                                         
           MOVE 8 TO APPL-RESULT                                                
           WRITE FD-REJS-RECORD FROM REJECT-RECORD                              
           IF DALYREJS-STATUS = '00'                                            
               MOVE 0 TO  APPL-RESULT                                           
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO REJECTS FILE'                          
               MOVE DALYREJS-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       2700-UPDATE-TCATBAL.                                                     
      * Update the balances in transaction balance file.                        
           MOVE XREF-ACCT-ID TO FD-TRANCAT-ACCT-ID                              
           MOVE DALYTRAN-TYPE-CD TO FD-TRANCAT-TYPE-CD                          
           MOVE DALYTRAN-CAT-CD TO FD-TRANCAT-CD                                
                                                                                
           MOVE 'N' TO WS-CREATE-TRANCAT-REC                                    
           READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD                           
              INVALID KEY                                                       
                DISPLAY 'TCATBAL record not found for key : '                   
                   FD-TRAN-CAT-KEY '.. Creating.'                               
                MOVE 'Y' TO WS-CREATE-TRANCAT-REC                               
           END-READ.                                                            
                                                                                
           IF  TCATBALF-STATUS = '00'  OR '23'                                  
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR READING TRANSACTION BALANCE FILE'                 
               MOVE TCATBALF-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
           IF WS-CREATE-TRANCAT-REC = 'Y'                                       
              PERFORM 2700-A-CREATE-TCATBAL-REC                                 
           ELSE                                                                 
              PERFORM 2700-B-UPDATE-TCATBAL-REC                                 
           END-IF                                                               
                                                                                
           EXIT.                                                                
      *---------------------------------------------------------------*         
       2700-A-CREATE-TCATBAL-REC.                                               
           INITIALIZE TRAN-CAT-BAL-RECORD                                       
           MOVE XREF-ACCT-ID TO TRANCAT-ACCT-ID                                 
           MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD                             
           MOVE DALYTRAN-CAT-CD TO TRANCAT-CD                                   
           ADD DALYTRAN-AMT TO TRAN-CAT-BAL                                     
                                                                                
           WRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD                
                                                                                
           IF  TCATBALF-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TRANSACTION BALANCE FILE'                 
               MOVE TCATBALF-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
      *---------------------------------------------------------------*         
       2700-B-UPDATE-TCATBAL-REC.                                               
           ADD DALYTRAN-AMT TO TRAN-CAT-BAL                                     
           REWRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD              
                                                                                
           IF  TCATBALF-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR REWRITING TRANSACTION BALANCE FILE'               
               MOVE TCATBALF-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF.                                                              
                                                                                
      *---------------------------------------------------------------*         
       2800-UPDATE-ACCOUNT-REC.                                                 
      * Update the balances in account record to reflect posted trans.          
           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL                                   
           IF DALYTRAN-AMT >= 0                                                 
              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT                          
           ELSE                                                                 
              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT                           
           END-IF                                                               
                                                                                
           REWRITE FD-ACCTFILE-REC FROM  ACCOUNT-RECORD                         
              INVALID KEY                                                       
                MOVE 109 TO WS-VALIDATION-FAIL-REASON                           
                MOVE 'ACCOUNT RECORD NOT FOUND'                                 
                  TO WS-VALIDATION-FAIL-REASON-DESC                             
           END-REWRITE.                                                         
           EXIT.                                                                
      *---------------------------------------------------------------*         
       2900-WRITE-TRANSACTION-FILE.                                             
           MOVE 8 TO  APPL-RESULT.                                              
           WRITE FD-TRANFILE-REC FROM TRAN-RECORD                               
                                                                                
           IF  TRANFILE-STATUS = '00'                                           
               MOVE 0 TO  APPL-RESULT                                           
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR WRITING TO TRANSACTION FILE'                      
               MOVE TRANFILE-STATUS  TO IO-STATUS                               
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       9000-DALYTRAN-CLOSE.                                                     
           MOVE 8 TO  APPL-RESULT.                                              
           CLOSE DALYTRAN-FILE                                                  
           IF  DALYTRAN-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING DALYTRAN FILE'                            
               MOVE DALYTRAN-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9100-TRANFILE-CLOSE.                                                     
           MOVE 8 TO  APPL-RESULT.                                              
           CLOSE TRANSACT-FILE                                                  
           IF  TRANFILE-STATUS = '00'                                           
               MOVE 0 TO  APPL-RESULT                                           
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
                                                                                
      *---------------------------------------------------------------*         
       9200-XREFFILE-CLOSE.                                                     
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
       9300-DALYREJS-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE DALYREJS-FILE                                                  
           IF  DALYREJS-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING DAILY REJECTS FILE'                       
               MOVE XREFFILE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9400-ACCTFILE-CLOSE.                                                     
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
                                                                                
      *---------------------------------------------------------------*         
       9500-TCATBALF-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE TCATBAL-FILE                                                   
           IF  TCATBALF-STATUS  = '00'                                          
               MOVE 0 TO APPL-RESULT                                            
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
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:32 CDT
      *
