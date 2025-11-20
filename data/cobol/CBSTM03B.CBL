       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBSTM03B.
       AUTHOR.        AWS.
      ******************************************************************
      * Program     : CBSTM03B.CBL
      * Application : CardDemo
      * Type        : BATCH COBOL Subroutine
      * Function    : Does file processing related to Transact Report
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
      * This program is to called by the statement create program
      * It does file handling
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRNX-FILE ASSIGN TO TRNXFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-TRNXS-ID
                  FILE STATUS  IS TRNXFILE-STATUS.

           SELECT XREF-FILE ASSIGN TO   XREFFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS FD-XREF-CARD-NUM
                  FILE STATUS  IS XREFFILE-STATUS.

           SELECT CUST-FILE ASSIGN TO CUSTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-CUST-ID
                  FILE STATUS  IS CUSTFILE-STATUS.

           SELECT ACCT-FILE ASSIGN TO ACCTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS FD-ACCT-ID
                  FILE STATUS  IS ACCTFILE-STATUS.

      *
       DATA DIVISION.
       FILE SECTION.
       FD  TRNX-FILE.
       01  FD-TRNXFILE-REC.
           05 FD-TRNXS-ID.
              10  FD-TRNX-CARD                  PIC X(16).
              10  FD-TRNX-ID                    PIC X(16).
           05 FD-ACCT-DATA                      PIC X(318).

       FD  XREF-FILE.
       01  FD-XREFFILE-REC.
           05 FD-XREF-CARD-NUM                  PIC X(16).
           05 FD-XREF-DATA                      PIC X(34).

       FD  CUST-FILE.
       01  FD-CUSTFILE-REC.
           05 FD-CUST-ID                        PIC X(09).
           05 FD-CUST-DATA                      PIC X(491).

       FD  ACCT-FILE.
       01  FD-ACCTFILE-REC.
           05 FD-ACCT-ID                        PIC 9(11).
           05 FD-ACCT-DATA                      PIC X(289).

       WORKING-STORAGE SECTION.

      *****************************************************************
       01  TRNXFILE-STATUS.
           05  TRNXFILE-STAT1      PIC X.
           05  TRNXFILE-STAT2      PIC X.

       01  XREFFILE-STATUS.
           05  XREFFILE-STAT1      PIC X.
           05  XREFFILE-STAT2      PIC X.

       01  CUSTFILE-STATUS.
           05  CUSTFILE-STAT1      PIC X.
           05  CUSTFILE-STAT2      PIC X.

       01  ACCTFILE-STATUS.
           05  ACCTFILE-STAT1      PIC X.
           05  ACCTFILE-STAT2      PIC X.

       LINKAGE SECTION.
       01  LK-M03B-AREA.
           05  LK-M03B-DD          PIC X(08).
           05  LK-M03B-OPER        PIC X(01).
             88  M03B-OPEN       VALUE 'O'.
             88  M03B-CLOSE      VALUE 'C'.
             88  M03B-READ       VALUE 'R'.
             88  M03B-READ-K     VALUE 'K'.
             88  M03B-WRITE      VALUE 'W'.
             88  M03B-REWRITE    VALUE 'Z'.
           05  LK-M03B-RC          PIC X(02).
           05  LK-M03B-KEY         PIC X(25).
           05  LK-M03B-KEY-LN      PIC S9(4).
           05  LK-M03B-FLDT        PIC X(1000).

       PROCEDURE DIVISION USING LK-M03B-AREA.

       0000-START.

           EVALUATE LK-M03B-DD
             WHEN 'TRNXFILE'
               PERFORM 1000-TRNXFILE-PROC THRU 1999-EXIT
             WHEN 'XREFFILE'
               PERFORM 2000-XREFFILE-PROC THRU 2999-EXIT
             WHEN 'CUSTFILE'
               PERFORM 3000-CUSTFILE-PROC THRU 3999-EXIT
             WHEN 'ACCTFILE'
               PERFORM 4000-ACCTFILE-PROC THRU 4999-EXIT
             WHEN OTHER
               GO TO 9999-GOBACK.

       9999-GOBACK.
           GOBACK.

       1000-TRNXFILE-PROC.

           IF M03B-OPEN
               OPEN INPUT TRNX-FILE
               GO TO 1900-EXIT
           END-IF.

           IF M03B-READ
               READ TRNX-FILE INTO LK-M03B-FLDT
               END-READ
               GO TO 1900-EXIT
           END-IF.

           IF M03B-CLOSE
               CLOSE TRNX-FILE
               GO TO 1900-EXIT
           END-IF.

       1900-EXIT.
           MOVE TRNXFILE-STATUS TO LK-M03B-RC.

       1999-EXIT.
           EXIT.

       2000-XREFFILE-PROC.

           IF M03B-OPEN
               OPEN INPUT XREF-FILE
               GO TO 2900-EXIT
           END-IF.

           IF M03B-READ
               READ XREF-FILE INTO LK-M03B-FLDT
               END-READ
               GO TO 2900-EXIT
           END-IF.

           IF M03B-CLOSE
               CLOSE XREF-FILE
               GO TO 2900-EXIT
           END-IF.

       2900-EXIT.
           MOVE XREFFILE-STATUS TO LK-M03B-RC.

       2999-EXIT.
           EXIT.

       3000-CUSTFILE-PROC.

           IF M03B-OPEN
               OPEN INPUT CUST-FILE
               GO TO 3900-EXIT
           END-IF.

           IF M03B-READ-K
               MOVE LK-M03B-KEY (1:LK-M03B-KEY-LN) TO FD-CUST-ID
               READ CUST-FILE INTO LK-M03B-FLDT
               END-READ
               GO TO 3900-EXIT
           END-IF.

           IF M03B-CLOSE
               CLOSE CUST-FILE
               GO TO 3900-EXIT
           END-IF.

       3900-EXIT.
           MOVE CUSTFILE-STATUS TO LK-M03B-RC.

       3999-EXIT.
           EXIT.

       4000-ACCTFILE-PROC.

           IF M03B-OPEN
               OPEN INPUT ACCT-FILE
               GO TO 4900-EXIT
           END-IF.

           IF M03B-READ-K
               MOVE LK-M03B-KEY (1:LK-M03B-KEY-LN) TO FD-ACCT-ID
               READ ACCT-FILE INTO LK-M03B-FLDT
               END-READ
               GO TO 4900-EXIT
           END-IF.

           IF M03B-CLOSE
               CLOSE ACCT-FILE
               GO TO 4900-EXIT
           END-IF.

       4900-EXIT.
           MOVE ACCTFILE-STATUS TO LK-M03B-RC.

       4999-EXIT.
           EXIT.

