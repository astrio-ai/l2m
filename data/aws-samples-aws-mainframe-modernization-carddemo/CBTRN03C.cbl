      ******************************************************************
      * Program     : CBTRN03C.CBL                                      
      * Application : CardDemo                                          
      * Type        : BATCH COBOL Program                                
      * Function    : Print the transaction detail report.     
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
       PROGRAM-ID.    CBTRN03C.                                                 
       AUTHOR.        AWS.                                                      
                                                                                
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
           SELECT TRANSACT-FILE ASSIGN TO TRANFILE                              
                  ORGANIZATION IS SEQUENTIAL                                    
                  FILE STATUS  IS TRANFILE-STATUS.                              
                                                                                
           SELECT XREF-FILE ASSIGN TO CARDXREF                                  
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-XREF-CARD-NUM                              
                  FILE STATUS  IS CARDXREF-STATUS.                              
                                                                                
           SELECT TRANTYPE-FILE ASSIGN TO TRANTYPE                              
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-TRAN-TYPE                                  
                  FILE STATUS  IS TRANTYPE-STATUS.                              
                                                                                
           SELECT TRANCATG-FILE ASSIGN TO TRANCATG                              
                  ORGANIZATION IS INDEXED                                       
                  ACCESS MODE  IS RANDOM                                        
                  RECORD KEY   IS FD-TRAN-CAT-KEY                               
                  FILE STATUS  IS TRANCATG-STATUS.                              
                                                                                
           SELECT REPORT-FILE ASSIGN TO TRANREPT                                
                  ORGANIZATION IS SEQUENTIAL                                    
                  FILE STATUS  IS TRANREPT-STATUS.                              
                                                                                
           SELECT DATE-PARMS-FILE ASSIGN TO DATEPARM                            
                  ORGANIZATION IS SEQUENTIAL                                    
                  FILE STATUS  IS DATEPARM-STATUS.                              
      *                                                                         
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  TRANSACT-FILE.                                                       
       01 FD-TRANFILE-REC.                                                      
          05 FD-TRANS-DATA      PIC X(304).                                     
          05 FD-TRAN-PROC-TS    PIC X(26).                                      
          05 FD-FILLER          PIC X(20).                                      
                                                                                
       FD  XREF-FILE.                                                           
       01  FD-CARDXREF-REC.                                                     
           05 FD-XREF-CARD-NUM                  PIC X(16).                      
           05 FD-XREF-DATA                      PIC X(34).                      
                                                                                
       FD  TRANTYPE-FILE.                                                       
       01 FD-TRANTYPE-REC.                                                      
          05 FD-TRAN-TYPE       PIC X(02).                                      
          05 FD-TRAN-DATA       PIC X(58).                                      
                                                                                
       FD  TRANCATG-FILE.                                                       
       01 FD-TRAN-CAT-RECORD.                                                   
           05  FD-TRAN-CAT-KEY.                                                 
              10  FD-TRAN-TYPE-CD                         PIC X(02).            
              10  FD-TRAN-CAT-CD                          PIC 9(04).            
           05  FD-TRAN-CAT-DATA                           PIC X(54).            
                                                                                
       FD  REPORT-FILE.                                                         
       01 FD-REPTFILE-REC       PIC X(133).                                     
                                                                                
       FD  DATE-PARMS-FILE.                                                     
       01 FD-DATEPARM-REC       PIC X(80).                                      
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
      *****************************************************************         
       COPY CVTRA05Y.                                                           
       01 TRANFILE-STATUS.                                                      
          05 TRANFILE-STAT1     PIC X.                                          
          05 TRANFILE-STAT2     PIC X.                                          
                                                                                
       COPY CVACT03Y.                                                           
       01  CARDXREF-STATUS.                                                     
           05  CARDXREF-STAT1      PIC X.                                       
           05  CARDXREF-STAT2      PIC X.                                       
                                                                                
       COPY CVTRA03Y.                                                           
       01  TRANTYPE-STATUS.                                                     
           05  TRANTYPE-STAT1      PIC X.                                       
           05  TRANTYPE-STAT2      PIC X.                                       
                                                                                
       COPY CVTRA04Y.                                                           
       01  TRANCATG-STATUS.                                                     
           05  TRANCATG-STAT1      PIC X.                                       
           05  TRANCATG-STAT2      PIC X.                                       
                                                                                
       COPY CVTRA07Y.                                                           
       01 TRANREPT-STATUS.                                                      
           05 REPTFILE-STAT1     PIC X.                                         
           05 REPTFILE-STAT2     PIC X.                                         
                                                                                
       01 DATEPARM-STATUS.                                                      
           05 DATEPARM-STAT1     PIC X.                                         
           05 DATEPARM-STAT2     PIC X.                                         
                                                                                
       01 WS-DATEPARM-RECORD.                                                   
           05 WS-START-DATE      PIC X(10).                                     
           05 FILLER             PIC X(01).                                     
           05 WS-END-DATE        PIC X(10).                                     
                                                                                
       01 WS-REPORT-VARS.                                                       
           05 WS-FIRST-TIME      PIC X      VALUE 'Y'.                          
           05 WS-LINE-COUNTER    PIC 9(09) COMP-3                               
                                            VALUE 0.                            
           05 WS-PAGE-SIZE       PIC 9(03) COMP-3                               
                                            VALUE 20.                           
           05 WS-BLANK-LINE      PIC X(133) VALUE SPACES.                       
           05 WS-PAGE-TOTAL      PIC S9(09)V99 VALUE 0.                         
           05 WS-ACCOUNT-TOTAL   PIC S9(09)V99 VALUE 0.                         
           05 WS-GRAND-TOTAL     PIC S9(09)V99 VALUE 0.                         
           05 WS-CURR-CARD-NUM   PIC X(16) VALUE SPACES.                        
                                                                                
       01 IO-STATUS.                                                            
          05 IO-STAT1           PIC X.                                          
          05 IO-STAT2           PIC X.                                          
       01 TWO-BYTES-BINARY      PIC 9(4) BINARY.                                
       01 TWO-BYTES-ALPHA REDEFINES TWO-BYTES-BINARY.                           
          05 TWO-BYTES-LEFT     PIC X.                                          
          05 TWO-BYTES-RIGHT    PIC X.                                          
       01 IO-STATUS-04.                                                         
          05 IO-STATUS-0401     PIC 9      VALUE 0.                             
          05 IO-STATUS-0403     PIC 999    VALUE 0.                             
                                                                                
       01 APPL-RESULT           PIC S9(9) COMP.                                 
          88 APPL-AOK                      VALUE 0.                             
          88 APPL-EOF                      VALUE 16.                            
                                                                                
       01 END-OF-FILE           PIC X(01)  VALUE 'N'.                           
       01 ABCODE                PIC S9(9) BINARY.                               
       01 TIMING                PIC S9(9) BINARY.                               
                                                                                
      *****************************************************************         
       PROCEDURE DIVISION.                                                      
           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN03C'.                    
           PERFORM 0000-TRANFILE-OPEN.                                          
           PERFORM 0100-REPTFILE-OPEN.                                          
           PERFORM 0200-CARDXREF-OPEN.                                          
           PERFORM 0300-TRANTYPE-OPEN.                                          
           PERFORM 0400-TRANCATG-OPEN.                                          
           PERFORM 0500-DATEPARM-OPEN.                                          
                                                                                
           PERFORM 0550-DATEPARM-READ.                                          
                                                                                
           PERFORM UNTIL END-OF-FILE = 'Y'                                      
             IF END-OF-FILE = 'N'                                               
                PERFORM 1000-TRANFILE-GET-NEXT                                  
                IF TRAN-PROC-TS (1:10) >= WS-START-DATE                         
                   AND TRAN-PROC-TS (1:10) <= WS-END-DATE                       
                   CONTINUE                                                     
                ELSE                                                            
                   NEXT SENTENCE                                                
                END-IF                                                          
                IF END-OF-FILE = 'N'                                            
                   DISPLAY TRAN-RECORD                                          
                   IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM                       
                     IF WS-FIRST-TIME = 'N'                                     
                       PERFORM 1120-WRITE-ACCOUNT-TOTALS                        
                     END-IF                                                     
                     MOVE TRAN-CARD-NUM TO WS-CURR-CARD-NUM                     
                     MOVE TRAN-CARD-NUM TO FD-XREF-CARD-NUM                     
                     PERFORM 1500-A-LOOKUP-XREF                                 
                   END-IF                                                       
                   MOVE TRAN-TYPE-CD OF TRAN-RECORD TO FD-TRAN-TYPE             
                   PERFORM 1500-B-LOOKUP-TRANTYPE                               
                   MOVE TRAN-TYPE-CD OF TRAN-RECORD                             
                     TO FD-TRAN-TYPE-CD OF FD-TRAN-CAT-KEY                      
                   MOVE TRAN-CAT-CD OF TRAN-RECORD                              
                     TO FD-TRAN-CAT-CD OF FD-TRAN-CAT-KEY                       
                   PERFORM 1500-C-LOOKUP-TRANCATG                               
                   PERFORM 1100-WRITE-TRANSACTION-REPORT                        
                ELSE                                                            
                 DISPLAY 'TRAN-AMT ' TRAN-AMT                                   
                 DISPLAY 'WS-PAGE-TOTAL'  WS-PAGE-TOTAL                         
                 ADD TRAN-AMT TO WS-PAGE-TOTAL                                  
                                 WS-ACCOUNT-TOTAL                               
                 PERFORM 1110-WRITE-PAGE-TOTALS                                 
                 PERFORM 1110-WRITE-GRAND-TOTALS                                
                END-IF                                                          
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           PERFORM 9000-TRANFILE-CLOSE.                                         
           PERFORM 9100-REPTFILE-CLOSE.                                         
           PERFORM 9200-CARDXREF-CLOSE.                                         
           PERFORM 9300-TRANTYPE-CLOSE.                                         
           PERFORM 9400-TRANCATG-CLOSE.                                         
           PERFORM 9500-DATEPARM-CLOSE.                                         
                                                                                
           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN03C'.                      
                                                                                
           GOBACK.                                                              
                                                                                
      * Read the date parameter file.                                           
       0550-DATEPARM-READ.                                                      
           READ DATE-PARMS-FILE INTO WS-DATEPARM-RECORD                         
           EVALUATE DATEPARM-STATUS                                             
             WHEN '00'                                                          
                 MOVE 0 TO APPL-RESULT                                          
             WHEN '10'                                                          
                 MOVE 16 TO APPL-RESULT                                         
             WHEN OTHER                                                         
                 MOVE 12 TO APPL-RESULT                                         
           END-EVALUATE                                                         
                                                                                
           IF APPL-AOK                                                          
              DISPLAY 'Reporting from ' WS-START-DATE                           
                 ' to ' WS-END-DATE                                             
           ELSE                                                                 
              IF APPL-EOF                                                       
                 MOVE 'Y' TO END-OF-FILE                                        
              ELSE                                                              
                 DISPLAY 'ERROR READING DATEPARM FILE'                          
                 MOVE DATEPARM-STATUS TO IO-STATUS                              
                 PERFORM 9910-DISPLAY-IO-STATUS                                 
                 PERFORM 9999-ABEND-PROGRAM                                     
              END-IF                                                            
           .                                                                    
                                                                                
      *****************************************************************         
      * I/O ROUTINES TO ACCESS A KSDS, VSAM DATA SET...               *         
      *****************************************************************         
       1000-TRANFILE-GET-NEXT.                                                  
           READ TRANSACT-FILE INTO TRAN-RECORD.                                 
                                                                                
           EVALUATE TRANFILE-STATUS                                             
             WHEN '00'                                                          
                 MOVE 0 TO APPL-RESULT                                          
             WHEN '10'                                                          
                 MOVE 16 TO APPL-RESULT                                         
             WHEN OTHER                                                         
                 MOVE 12 TO APPL-RESULT                                         
           END-EVALUATE                                                         
                                                                                
           IF APPL-AOK                                                          
              CONTINUE                                                          
           ELSE                                                                 
              IF APPL-EOF                                                       
                 MOVE 'Y' TO END-OF-FILE                                        
              ELSE                                                              
                 DISPLAY 'ERROR READING TRANSACTION FILE'                       
                 MOVE TRANFILE-STATUS TO IO-STATUS                              
                 PERFORM 9910-DISPLAY-IO-STATUS                                 
                 PERFORM 9999-ABEND-PROGRAM                                     
              END-IF                                                            
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1100-WRITE-TRANSACTION-REPORT.                                           
           IF WS-FIRST-TIME = 'Y'                                               
              MOVE 'N' TO WS-FIRST-TIME                                         
              MOVE WS-START-DATE TO REPT-START-DATE                             
              MOVE WS-END-DATE TO REPT-END-DATE                                 
              PERFORM 1120-WRITE-HEADERS                                        
           END-IF                                                               
                                                                                
           IF FUNCTION MOD(WS-LINE-COUNTER, WS-PAGE-SIZE) = 0                   
              PERFORM 1110-WRITE-PAGE-TOTALS                                    
              PERFORM 1120-WRITE-HEADERS                                        
           END-IF                                                               
                                                                                
           ADD TRAN-AMT TO WS-PAGE-TOTAL                                        
                           WS-ACCOUNT-TOTAL                                     
           PERFORM 1120-WRITE-DETAIL                                            
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       1110-WRITE-PAGE-TOTALS.                                                  
           MOVE WS-PAGE-TOTAL TO REPT-PAGE-TOTAL                                
           MOVE REPORT-PAGE-TOTALS TO FD-REPTFILE-REC                           
           PERFORM 1111-WRITE-REPORT-REC                                        
           ADD WS-PAGE-TOTAL TO WS-GRAND-TOTAL                                  
           MOVE 0 TO WS-PAGE-TOTAL                                              
           ADD 1 TO WS-LINE-COUNTER                                             
           MOVE TRANSACTION-HEADER-2 TO FD-REPTFILE-REC                         
           PERFORM 1111-WRITE-REPORT-REC                                        
           ADD 1 TO WS-LINE-COUNTER                                             
                                                                                
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1120-WRITE-ACCOUNT-TOTALS.                                               
           MOVE WS-ACCOUNT-TOTAL   TO REPT-ACCOUNT-TOTAL                        
           MOVE REPORT-ACCOUNT-TOTALS TO FD-REPTFILE-REC                        
           PERFORM 1111-WRITE-REPORT-REC                                        
           MOVE 0 TO WS-ACCOUNT-TOTAL                                           
           ADD 1 TO WS-LINE-COUNTER                                             
           MOVE TRANSACTION-HEADER-2 TO FD-REPTFILE-REC                         
           PERFORM 1111-WRITE-REPORT-REC                                        
           ADD 1 TO WS-LINE-COUNTER                                             
                                                                                
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1110-WRITE-GRAND-TOTALS.                                                 
           MOVE WS-GRAND-TOTAL TO REPT-GRAND-TOTAL                              
           MOVE REPORT-GRAND-TOTALS TO FD-REPTFILE-REC                          
           PERFORM 1111-WRITE-REPORT-REC                                        
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1120-WRITE-HEADERS.                                                      
           MOVE REPORT-NAME-HEADER TO FD-REPTFILE-REC                           
           PERFORM 1111-WRITE-REPORT-REC                                        
           ADD 1 TO WS-LINE-COUNTER                                             
                                                                                
           MOVE WS-BLANK-LINE TO FD-REPTFILE-REC                                
           PERFORM 1111-WRITE-REPORT-REC                                        
           ADD 1 TO WS-LINE-COUNTER                                             
                                                                                
           MOVE TRANSACTION-HEADER-1 TO FD-REPTFILE-REC                         
           PERFORM 1111-WRITE-REPORT-REC                                        
           ADD 1 TO WS-LINE-COUNTER                                             
                                                                                
           MOVE TRANSACTION-HEADER-2 TO FD-REPTFILE-REC                         
           PERFORM 1111-WRITE-REPORT-REC                                        
           ADD 1 TO WS-LINE-COUNTER                                             
                                                                                
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1111-WRITE-REPORT-REC.                                                   
                                                                                
           WRITE FD-REPTFILE-REC                                                
           IF TRANREPT-STATUS = '00'                                            
              MOVE 0 TO APPL-RESULT                                             
           ELSE                                                                 
              MOVE 12 TO APPL-RESULT                                            
           END-IF                                                               
           IF APPL-AOK                                                          
              CONTINUE                                                          
           ELSE                                                                 
              DISPLAY 'ERROR WRITING REPTFILE'                                  
              MOVE TRANREPT-STATUS TO IO-STATUS                                 
              PERFORM 9910-DISPLAY-IO-STATUS                                    
              PERFORM 9999-ABEND-PROGRAM                                        
           END-IF                                                               
           EXIT.                                                                
                                                                                
       1120-WRITE-DETAIL.                                                       
           INITIALIZE TRANSACTION-DETAIL-REPORT                                 
           MOVE TRAN-ID TO TRAN-REPORT-TRANS-ID                                 
           MOVE XREF-ACCT-ID TO TRAN-REPORT-ACCOUNT-ID                          
           MOVE TRAN-TYPE-CD OF TRAN-RECORD TO TRAN-REPORT-TYPE-CD              
           MOVE TRAN-TYPE-DESC TO TRAN-REPORT-TYPE-DESC                         
           MOVE TRAN-CAT-CD OF TRAN-RECORD  TO TRAN-REPORT-CAT-CD               
           MOVE TRAN-CAT-TYPE-DESC TO TRAN-REPORT-CAT-DESC                      
           MOVE TRAN-SOURCE TO TRAN-REPORT-SOURCE                               
           MOVE TRAN-AMT TO TRAN-REPORT-AMT                                     
           MOVE TRANSACTION-DETAIL-REPORT TO FD-REPTFILE-REC                    
           PERFORM 1111-WRITE-REPORT-REC                                        
           ADD 1 TO WS-LINE-COUNTER                                             
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0000-TRANFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT TRANSACT-FILE                                             
           IF TRANFILE-STATUS = '00'                                            
              MOVE 0 TO APPL-RESULT                                             
           ELSE                                                                 
              MOVE 12 TO APPL-RESULT                                            
           END-IF                                                               
           IF APPL-AOK                                                          
              CONTINUE                                                          
           ELSE                                                                 
              DISPLAY 'ERROR OPENING TRANFILE'                                  
              MOVE TRANFILE-STATUS TO IO-STATUS                                 
              PERFORM 9910-DISPLAY-IO-STATUS                                    
              PERFORM 9999-ABEND-PROGRAM                                        
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0100-REPTFILE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN OUTPUT REPORT-FILE                                              
           IF TRANREPT-STATUS = '00'                                            
              MOVE 0 TO APPL-RESULT                                             
           ELSE                                                                 
              MOVE 12 TO APPL-RESULT                                            
           END-IF                                                               
           IF APPL-AOK                                                          
              CONTINUE                                                          
           ELSE                                                                 
              DISPLAY 'ERROR OPENING REPTFILE'                                  
              MOVE TRANREPT-STATUS TO IO-STATUS                                 
              PERFORM 9910-DISPLAY-IO-STATUS                                    
              PERFORM 9999-ABEND-PROGRAM                                        
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0200-CARDXREF-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT XREF-FILE                                                 
           IF  CARDXREF-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING CROSS REF FILE'                           
               MOVE CARDXREF-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0300-TRANTYPE-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT TRANTYPE-FILE                                             
           IF  TRANTYPE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRANSACTION TYPE FILE'                    
               MOVE TRANTYPE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0400-TRANCATG-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT TRANCATG-FILE                                             
           IF  TRANCATG-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING TRANSACTION CATG FILE'                    
               MOVE TRANCATG-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       0500-DATEPARM-OPEN.                                                      
           MOVE 8 TO APPL-RESULT.                                               
           OPEN INPUT DATE-PARMS-FILE                                           
           IF  DATEPARM-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR OPENING DATE PARM FILE'                           
               MOVE DATEPARM-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1500-A-LOOKUP-XREF.                                                      
           READ XREF-FILE INTO CARD-XREF-RECORD                                 
              INVALID KEY                                                       
                 DISPLAY 'INVALID CARD NUMBER : '  FD-XREF-CARD-NUM             
                 MOVE 23 TO IO-STATUS                                           
                 PERFORM 9910-DISPLAY-IO-STATUS                                 
                 PERFORM 9999-ABEND-PROGRAM                                     
           END-READ                                                             
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1500-B-LOOKUP-TRANTYPE.                                                  
           READ TRANTYPE-FILE INTO TRAN-TYPE-RECORD                             
              INVALID KEY                                                       
                 DISPLAY 'INVALID TRANSACTION TYPE : '  FD-TRAN-TYPE            
                 MOVE 23 TO IO-STATUS                                           
                 PERFORM 9910-DISPLAY-IO-STATUS                                 
                 PERFORM 9999-ABEND-PROGRAM                                     
           END-READ                                                             
           EXIT.                                                                
      *---------------------------------------------------------------*         
       1500-C-LOOKUP-TRANCATG.                                                  
           READ TRANCATG-FILE INTO TRAN-CAT-RECORD                              
              INVALID KEY                                                       
                 DISPLAY 'INVALID TRAN CATG KEY : '  FD-TRAN-CAT-KEY            
                 MOVE 23 TO IO-STATUS                                           
                 PERFORM 9910-DISPLAY-IO-STATUS                                 
                 PERFORM 9999-ABEND-PROGRAM                                     
           END-READ                                                             
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9000-TRANFILE-CLOSE.                                                     
           ADD 8 TO ZERO GIVING APPL-RESULT.                                    
           CLOSE TRANSACT-FILE                                                  
           IF TRANFILE-STATUS = '00'                                            
              SUBTRACT APPL-RESULT FROM APPL-RESULT                             
           ELSE                                                                 
              ADD 12 TO ZERO GIVING APPL-RESULT                                 
           END-IF                                                               
           IF APPL-AOK                                                          
              CONTINUE                                                          
           ELSE                                                                 
              DISPLAY 'ERROR CLOSING POSTED TRANSACTION FILE'                   
              MOVE TRANFILE-STATUS TO IO-STATUS                                 
              PERFORM 9910-DISPLAY-IO-STATUS                                    
              PERFORM 9999-ABEND-PROGRAM                                        
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9100-REPTFILE-CLOSE.                                                     
           ADD 8 TO ZERO GIVING APPL-RESULT.                                    
           CLOSE REPORT-FILE                                                    
           IF TRANREPT-STATUS = '00'                                            
              SUBTRACT APPL-RESULT FROM APPL-RESULT                             
           ELSE                                                                 
              ADD 12 TO ZERO GIVING APPL-RESULT                                 
           END-IF                                                               
           IF APPL-AOK                                                          
              CONTINUE                                                          
           ELSE                                                                 
              DISPLAY 'ERROR CLOSING REPORT FILE'                               
              MOVE TRANREPT-STATUS TO IO-STATUS                                 
              PERFORM 9910-DISPLAY-IO-STATUS                                    
              PERFORM 9999-ABEND-PROGRAM                                        
           END-IF                                                               
           EXIT.                                                                
                                                                                
      *---------------------------------------------------------------*         
       9200-CARDXREF-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE XREF-FILE                                                      
           IF  CARDXREF-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING CROSS REF FILE'                           
               MOVE CARDXREF-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9300-TRANTYPE-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE TRANTYPE-FILE                                                  
           IF  TRANTYPE-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING TRANSACTION TYPE FILE'                    
               MOVE TRANTYPE-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9400-TRANCATG-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE TRANCATG-FILE                                                  
           IF  TRANCATG-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING TRANSACTION CATG FILE'                    
               MOVE TRANCATG-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
      *---------------------------------------------------------------*         
       9500-DATEPARM-CLOSE.                                                     
           MOVE 8 TO APPL-RESULT.                                               
           CLOSE DATE-PARMS-FILE                                                
           IF  DATEPARM-STATUS = '00'                                           
               MOVE 0 TO APPL-RESULT                                            
           ELSE                                                                 
               MOVE 12 TO APPL-RESULT                                           
           END-IF                                                               
           IF  APPL-AOK                                                         
               CONTINUE                                                         
           ELSE                                                                 
               DISPLAY 'ERROR CLOSING DATE PARM FILE'                           
               MOVE DATEPARM-STATUS TO IO-STATUS                                
               PERFORM 9910-DISPLAY-IO-STATUS                                   
               PERFORM 9999-ABEND-PROGRAM                                       
           END-IF                                                               
           EXIT.                                                                
                                                                                
                                                                                
                                                                                
                                                                                
       9999-ABEND-PROGRAM.                                                      
           DISPLAY 'ABENDING PROGRAM'                                           
           MOVE 0 TO TIMING                                                     
           MOVE 999 TO ABCODE                                                   
           CALL 'CEE3ABD'.                                                      
                                                                                
      *****************************************************************         
       9910-DISPLAY-IO-STATUS.                                                  
           IF IO-STATUS NOT NUMERIC                                             
              OR IO-STAT1 = '9'                                                 
              MOVE IO-STAT1 TO IO-STATUS-04(1:1)                                
              MOVE 0 TO TWO-BYTES-BINARY                                        
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
