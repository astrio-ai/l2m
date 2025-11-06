      ******************************************************************        
      *****       CALL TO CEEDAYS                                *******        
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
       PROGRAM-ID. CSUTLDTC.                                                    
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
                                                                                
      ****  Date passed to CEEDAYS API                                          
         01 WS-DATE-TO-TEST.                                                    
              02  Vstring-length      PIC S9(4) BINARY.                         
              02  Vstring-text.                                                 
                  03  Vstring-char    PIC X                                     
                              OCCURS 0 TO 256 TIMES                             
                              DEPENDING ON Vstring-length                       
                                 of WS-DATE-TO-TEST.                            
      ****  DATE FORMAT PASSED TO CEEDAYS API                                   
         01 WS-DATE-FORMAT.                                                     
              02  Vstring-length      PIC S9(4) BINARY.                         
              02  Vstring-text.                                                 
                  03  Vstring-char    PIC X                                     
                              OCCURS 0 TO 256 TIMES                             
                              DEPENDING ON Vstring-length                       
                                 of WS-DATE-FORMAT.                             
      ****  OUTPUT from CEEDAYS - LILLIAN DATE FORMAT                           
         01 OUTPUT-LILLIAN    PIC S9(9) USAGE IS BINARY.                        
         01 WS-MESSAGE.                                                         
              02 WS-SEVERITY  PIC X(04).                                        
              02 WS-SEVERITY-N REDEFINES WS-SEVERITY PIC 9(4).                  
              02 FILLER       PIC X(11) VALUE 'Mesg Code:'.                     
              02 WS-MSG-NO    PIC X(04).                                        
              02 WS-MSG-NO-N  REDEFINES WS-MSG-NO PIC 9(4).                     
              02 FILLER       PIC X(01) VALUE SPACE.                            
              02 WS-RESULT    PIC X(15).                                        
              02 FILLER       PIC X(01) VALUE SPACE.                            
              02 FILLER       PIC X(09) VALUE 'TstDate:'.                       
              02 WS-DATE      PIC X(10) VALUE SPACES.                           
              02 FILLER       PIC X(01) VALUE SPACE.                            
              02 FILLER       PIC X(10) VALUE 'Mask used:'.                     
              02 WS-DATE-FMT  PIC X(10).                                        
              02 FILLER       PIC X(01) VALUE SPACE.                            
              02 FILLER       PIC X(03) VALUE SPACES.                           
                                                                                
      * CEEDAYS API FEEDBACK CODE                                               
          01 FEEDBACK-CODE.                                                     
           02  FEEDBACK-TOKEN-VALUE. 
             88  FC-INVALID-DATE       VALUE X'0000000000000000'.
             88  FC-INSUFFICIENT-DATA  VALUE X'000309CB59C3C5C5'.
             88  FC-BAD-DATE-VALUE     VALUE X'000309CC59C3C5C5'.
             88  FC-INVALID-ERA        VALUE X'000309CD59C3C5C5'.
             88  FC-UNSUPP-RANGE       VALUE X'000309D159C3C5C5'.
             88  FC-INVALID-MONTH      VALUE X'000309D559C3C5C5'.
             88  FC-BAD-PIC-STRING     VALUE X'000309D659C3C5C5'.
             88  FC-NON-NUMERIC-DATA   VALUE X'000309D859C3C5C5'.
             88  FC-YEAR-IN-ERA-ZERO   VALUE X'000309D959C3C5C5'.
               03  CASE-1-CONDITION-ID.                                         
                   04  SEVERITY        PIC S9(4) BINARY.                        
                   04  MSG-NO          PIC S9(4) BINARY.                        
               03  CASE-2-CONDITION-ID                                          
                         REDEFINES CASE-1-CONDITION-ID.                         
                   04  CLASS-CODE      PIC S9(4) BINARY.                        
                   04  CAUSE-CODE      PIC S9(4) BINARY.                        
               03  CASE-SEV-CTL    PIC X.                                       
               03  FACILITY-ID     PIC XXX.                                     
           02  I-S-INFO        PIC S9(9) BINARY.                                
                                                                                
                                                                                
       LINKAGE SECTION.                                                         
          01 LS-DATE         PIC X(10).                                         
          01 LS-DATE-FORMAT  PIC X(10).                                         
          01 LS-RESULT       PIC X(80).                                         
                                                                                
       PROCEDURE DIVISION USING LS-DATE, LS-DATE-FORMAT, LS-RESULT.             
           
           INITIALIZE WS-MESSAGE
           MOVE SPACES TO WS-DATE
                                                                        
           PERFORM A000-MAIN                                                    
              THRU A000-MAIN-EXIT                                               

      *    DISPLAY WS-MESSAGE                                                   
           MOVE WS-MESSAGE                 TO LS-RESULT 
           MOVE WS-SEVERITY-N              TO RETURN-CODE          
                                                                                
           EXIT PROGRAM                                                         
      *    GOBACK                                                               
           .                                                                    
       A000-MAIN.                                                               
                                                                                
           MOVE LENGTH OF LS-DATE                                               
                        TO VSTRING-LENGTH  OF WS-DATE-TO-TEST                   
           MOVE LS-DATE TO VSTRING-TEXT    OF WS-DATE-TO-TEST
                           WS-DATE                  
           MOVE LENGTH OF LS-DATE-FORMAT                                        
                         TO VSTRING-LENGTH OF WS-DATE-FORMAT                    
           MOVE LS-DATE-FORMAT                                                  
                         TO VSTRING-TEXT   OF WS-DATE-FORMAT   
                            WS-DATE-FMT  
           MOVE 0        TO OUTPUT-LILLIAN                              
                                                                        
           CALL "CEEDAYS" USING                                                 
                  WS-DATE-TO-TEST,                                              
                  WS-DATE-FORMAT,                                               
                  OUTPUT-LILLIAN,                                               
                  FEEDBACK-CODE                                                 
                                                                                
           MOVE WS-DATE-TO-TEST            TO WS-DATE                           
           MOVE SEVERITY OF FEEDBACK-CODE  TO WS-SEVERITY-N                     
           MOVE MSG-NO OF FEEDBACK-CODE    TO WS-MSG-NO-N                       
                                                                 
      *    WS-RESULT IS 15 CHARACTERS                                           
      *                123456789012345'                                         
           EVALUATE TRUE                                                        
              WHEN FC-INVALID-DATE                                   
                 MOVE 'Date is valid'      TO WS-RESULT              
              WHEN FC-INSUFFICIENT-DATA                              
                 MOVE 'Insufficient'       TO WS-RESULT              
              WHEN FC-BAD-DATE-VALUE                                 
                 MOVE 'Datevalue error'    TO WS-RESULT              
              WHEN FC-INVALID-ERA                                    
                 MOVE 'Invalid Era    '    TO WS-RESULT              
              WHEN FC-UNSUPP-RANGE                                   
                 MOVE 'Unsupp. Range  '    TO WS-RESULT              
              WHEN FC-INVALID-MONTH                                  
                 MOVE 'Invalid month  '    TO WS-RESULT              
              WHEN FC-BAD-PIC-STRING                                 
                 MOVE 'Bad Pic String '    TO WS-RESULT              
              WHEN FC-NON-NUMERIC-DATA                               
                 MOVE 'Nonnumeric data'    TO WS-RESULT              
              WHEN FC-YEAR-IN-ERA-ZERO                               
                 MOVE 'YearInEra is 0 '    TO WS-RESULT              
              WHEN OTHER                                             
                 MOVE 'Date is invalid'    TO WS-RESULT 
           END-EVALUATE                                                         
                                                                                
           .                                                                    
       A000-MAIN-EXIT.                                                          
           EXIT                                                                 
           .                                                                    
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:12:35 CDT
      *
