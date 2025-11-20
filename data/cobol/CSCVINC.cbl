       PROCESS CICS
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSCVINC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *  Container names
       01 WS-STORAGE.
          05 Input-Length       PIC S9(8) COMP-4.
          05 Channel-Name       PIC X(16) VALUE SPACES.
          05 Output-String      PIC X(72) VALUE SPACES.
          05 Container-Name     PIC X(16).
          05 Token              PIC S9(8) COMP-5 SYNC.
          05 Message-to-Write   PIC X(90).
          05 CSMT-Output-Area   PIC X(121).
          05 Abs-Time           PIC S9(15) COMP-3.
          05 Current-Date       PIC X(8).
          05 Current-Time       PIC X(8).
          05 EIBRespCode        PIC S9(8) COMP.
           COPY CSCCREQ .
           COPY CSCCRESP.

       PROCEDURE DIVISION.
       MAIN-PROCESSING SECTION.
           INITIALIZE Request-Container.
           INITIALIZE Response-Container.
      *  Save current CICS userid
           EXEC CICS ASSIGN USERID(USERID of Response-Container)
                            END-EXEC.

      *  Obtain name of channel
           EXEC CICS ASSIGN CHANNEL(Channel-Name)
                            END-EXEC.

      *  If no channel passed in, terminate with abend code NOCN
           IF Channel-Name = SPACES THEN
               EXEC CICS ABEND ABCODE('NOCN') NODUMP
                            END-EXEC
           END-IF.

      *  Start browsing the channel (to locate the first container)
           EXEC CICS STARTBROWSE CONTAINER
                     CHANNEL(Channel-Name)
                     BROWSETOKEN(Token)
                     RESP(CEIBRESP)
                     RESP2(CEIBRESP2)
                     END-EXEC.
           If CEIBRESP NOT = DFHRESP(NORMAL)
               Move 'Error invoking CICS STARTBROWSE command '
                   TO Message-to-Write
               Perform Write-to-CSMT-Queue
               Perform Return-to-CICS
           End-If.

      *  Obtain the name of the container in the channel
           EXEC CICS GETNEXT CONTAINER(Container-Name)
                     BROWSETOKEN(Token)
                     RESP(CEIBRESP)
                     RESP2(CEIBRESP)
                     END-EXEC.
           If CEIBRESP NOT = DFHRESP(NORMAL)
               Move 'Error invoking EXEC GETNEXT CONTAINER '
                   TO Message-to-Write
               Perform Write-to-CSMT-Queue
               Perform Return-to-CICS
           End-If.

           EXEC CICS GET CONTAINER(Container-Name)
                            CHANNEL(Channel-Name)
                            NODATA FLENGTH(Input-Length)
                            NOCONVERT
                            RESP(CEIBRESP)
                            RESP2(CEIBRESP2)
                            END-EXEC.

      *  Check response code
           EVALUATE CEIBRESP

      *  Container not passed in
             WHEN DFHRESP(CONTAINERERR)
               STRING Container-Name
                            DELIMITED BY SPACE
                      ' container was not passed to the program'
                            DELIMITED BY SIZE
                   INTO Message-to-Write END-STRING
               Perform Write-to-CSMT-Queue
               Perform Return-to-CICS

      *  Container is BIT not CHAR
             WHEN DFHRESP(CCSIDERR)
               IF CEIBRESP = 3
                 STRING 'Container '
                            DELIMITED BY SIZE
                        Container-Name
                            DELIMITED BY SPACE
                        ' type is BIT, not CHAR'
                            DELIMITED BY SIZE
                         INTO Message-to-Write END-STRING
                         Perform Write-to-CSMT-Queue
                         Perform Return-to-CICS
               ELSE
                 STRING 'The GET CONTAINER command returned an',
                        ' unexpected CCSIDERR condition'
                            DELIMITED BY SIZE
                            INTO Message-to-Write END-STRING
                            Perform Write-to-CSMT-Queue
                            Perform Return-to-CICS
               END-IF

      *  Read from container OK
             WHEN DFHRESP(NORMAL)
               STRING 'CSCVINC Read from '
                            DELIMITED BY SIZE
                      Container-Name
                            DELIMITED BY SPACE
                      ' container successfully'
                            DELIMITED BY SIZE
                            INTO Message-to-Write END-STRING
      *                     Perform Write-to-CSMT-Queue

      *  Other response code
             WHEN OTHER
               STRING 'The GET CONTAINER command returned an',
                      ' unexpected response code'
                            DELIMITED BY SIZE
                            INTO Message-to-Write END-STRING
                            Perform Write-to-CSMT-Queue
                            Perform Return-to-CICS

           END-EVALUATE.

      *  Read contents of request container
           EXEC CICS GET CONTAINER(Container-Name)
                            CHANNEL(Channel-Name)
                            FLENGTH(Input-Length)
                            INTO(Request-Container)
                            NOCONVERT
                            RESP(CEIBRESP)
                            RESP2(CEIBRESP2)
                            END-EXEC.
           If CEIBRESP NOT = DFHRESP(NORMAL)
               Move 'Error accessing container data'
                   to Message-to-Write
               Perform Write-to-CSMT-Queue
               Perform Return-to-CICS
           End-If.

      *  Read record from FILEA using NUMB as key
           MOVE NUMB OF Request-Container to NUMB of Response-Container.
           MOVE ACTION OF Request-Container to
                                           ACTION of Response-Container.
           EVALUATE ACTION of Request-Container

              WHEN 'D'
                EXEC CICS DELETE FILE('FILEA')
                            KEYLENGTH(LENGTH OF NUMB
                                      OF Request-Container)
                            RIDFLD(NUMB OF Request-Container)
                            RESP(CEIBRESP)
                            RESP2(CEIBRESP2)
                            END-EXEC
                If CEIBRESP NOT = DFHRESP(NORMAL)
                  Move 'Error deleting record ' to Message-to-Write
                  Perform Write-to-CSMT-Queue
                End-If
                MOVE CORRESPONDING FileA-Area of Request-Container to
                       FileA-Area of Response-Container

              WHEN 'I'
                EXEC CICS WRITE FILE('FILEA')
                            FROM(FileA-Area of Request-Container)
                            LENGTH(LENGTH OF FileA-Area
                                     OF Request-Container)
                            RIDFLD(NUMB OF Request-Container)
                            RESP(CEIBRESP)
                            RESP2(CEIBRESP2)
                            END-EXEC
                If CEIBRESP NOT = DFHRESP(NORMAL)
                  Move 'Error deleting record ' to Message-to-Write
                  Perform Write-to-CSMT-Queue
                End-If
                MOVE CORRESPONDING FileA-Area of Request-Container to
                       FileA-Area of Response-Container

              WHEN 'U'
                EXEC CICS READ FILE('FILEA') INTO(FileA-Area of
                                   Response-Container)
                            LENGTH(LENGTH OF FileA-Area
                                OF Response-Container)
                            UPDATE
                            RIDFLD(NUMB OF Request-Container)
                            RESP(CEIBRESP)
                            RESP2(CEIBRESP2)
                            END-EXEC
                If CEIBRESP NOT = DFHRESP(NORMAL)
                  Move 'Error reading a record for update ' to
                            Message-to-Write
                  Perform Write-to-CSMT-Queue
                End-If
                EXEC CICS REWRITE FILE('FILEA')
                            FROM(FileA-Area of Request-Container)
                            LENGTH(LENGTH OF FileA-Area
                                     of Request-Container)
                            RESP(CEIBRESP)
                            RESP2(CEIBRESP2)
                            END-EXEC
                If CEIBRESP NOT = DFHRESP(NORMAL)
                  Move 'Error rewriting record ' to Message-to-Write
                  Perform Write-to-CSMT-Queue
                End-If
                MOVE CORRESPONDING FileA-Area of Request-Container to
                       FileA-Area of Response-Container

           WHEN OTHER
                EXEC CICS READ FILE('FILEA') INTO(FileA-Area
                                   OF Response-Container)
                            LENGTH(LENGTH OF FileA-Area
                                   OF Response-Container)
                            RIDFLD(NUMB OF Request-Container)
                            RESP(CEIBRESP)
                            RESP2(CEIBRESP2)
                            END-EXEC
               If CEIBRESP NOT = DFHRESP(NORMAL)
                 Move 'Error reading record '
                   to Message-to-Write
                 Perform Write-to-CSMT-Queue
               End-If
           END-EVALUATE.

      *  Put response container into channel
           EXEC CICS PUT CONTAINER(Container-Name)
                            FROM(Response-Container)
                            FLENGTH(LENGTH OF Response-Container)
                            RESP(CEIBRESP)
                            END-EXEC.

      *  Check return code
           IF CEIBRESP NOT = DFHRESP(NORMAL)
               Move 'Error writing reponse container '
                   to Message-to-Write
               Perform Write-to-CSMT-Queue
               Perform Return-to-CICS
           END-IF.

      *  Finish
           EXEC CICS RETURN END-EXEC.

       Return-to-CICS.
           EXEC CICS RETURN END-EXEC.

       Write-to-CSMT-Queue.
           PERFORM Get-and-Format-Current-Time.
           String 'CSCVINC '
               Current-Date ' ' Current-Time ' '
               Message-To-Write Delimited by Size
            Into CSMT-Output-Area.
           EXEC CICS WRITEQ TD QUEUE('CSMT')
               FROM (CSMT-Output-Area)
               LENGTH(LENGTH OF CSMT-Output-Area)
               RESP(EIBRespCode)
               END-EXEC.

       Get-and-Format-Current-Time.
           EXEC CICS ASKTIME ABSTIME(Abs-Time) END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(Abs-Time)
                         DATE(Current-Date) DATESEP('/')
                         TIME(Current-Time) TIMESEP(':')
                         END-EXEC.

           EXIT.

