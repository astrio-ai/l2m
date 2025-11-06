      *****************************************************************
      *    LOANCICS                                                   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOANCICS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COMMAREA-BUFFER.
           10 name pic X(20).
           10 creditScore pic 9(18).
           10 yearlyIncome pic 9(18).
           10 age pic 9(10).
           10 amount pic  9(18).
           10 approved pic X.
               88 BoolValue value 'T'.
           10 effectDate pic X(8).
           10 yearlyInterestRate pic S9(5).
           10 yearlyRepayment pic 9(18).
           10 uid             pic X(8).
           10 messages-Num pic 9(9).
           10 messages pic X(60) occurs 10 times.
        01 MESSAGE-TO-SEND PIC X(80) VALUE SPACES.

        COPY MINIMAP.
        COPY DFHAID.

        LINKAGE SECTION.

        01  DFHCOMMAREA                   PIC X(725).

        PROCEDURE DIVISION.

      ***********************************************************
      * Main Program loop starts here....                       *
      ***********************************************************

        MAIN-PROCESS SECTION.

      ***********************************************************
      * FIRST TIME PROCESSING                                   *
      ***********************************************************
            IF EIBCALEN = 0
               EXEC CICS SEND MAP('MINIMAP')
                              MAPSET('MINIMAP')
                              MAPONLY
                              FREEKB
                              ERASE
                              END-EXEC

               EXEC CICS RETURN TRANSID('LOAN')
                                COMMAREA (COMMAREA-BUFFER)
                                LENGTH(LENGTH of COMMAREA-BUFFER)
                                END-EXEC
            END-IF.

      ***********************************************************
      * END OF FIRST TIME PROCESSING                            *
      ***********************************************************

            MOVE DFHCOMMAREA TO COMMAREA-BUFFER.

               EXEC CICS RECEIVE MAP('MINIMAP')
                           MAPSET('MINIMAP')
                           NOHANDLE
                           END-EXEC
               EVALUATE EIBAID
                   WHEN DFHPF3   PERFORM EXIT-TRANSACTION
                   WHEN DFHPF12  PERFORM EXIT-TRANSACTION
                   WHEN DFHCLEAR CONTINUE
                   WHEN DFHENTER
                     MOVE  AGEI      to age of COMMAREA-BUFFER
                     MOVE  CRDSCREI  to creditScore of COMMAREA-BUFFER
                     MOVE  NAMEI     to name of COMMAREA-BUFFER
                     MOVE  INCOMEI   to yearlyIncome of COMMAREA-BUFFER
                     MOVE  AMOUNTI   to amount       of COMMAREA-BUFFER
                     MOVE  'F'       to approved     of COMMAREA-BUFFER
                     MOVE  EFDATEI   to effectDate   of COMMAREA-BUFFER
                     MOVE  0         to messages-Num of COMMAREA-BUFFER
                     MOVE  INTERESTI
                               TO yearlyInterestRate  of COMMAREA-BUFFER
                     MOVE  YRPAYMNTI
                               To yearlyRepayment of COMMAREA-BUFFER
                     EXEC CICS LINK PROGRAM('MINICICS')
                        COMMAREA(COMMAREA-BUFFER)
                     END-EXEC
                     MOVE name of COMMAREA-BUFFER to NAMEO
                     MOVE uid  of COMMAREA-BUFFER to UIDO
                     IF approved = 'T'
                        MOVE 'Loan approved' to approvedo
                     ELSE
                        MOVE 'Loan not approved' to approvedo
                     END-IF
                     MOVE messages(1) to MSG1O
                     MOVE messages(2) to MSG2O
                     MOVE messages(3) to MSG3O
                     MOVE messages(4) to MSG4O
                     MOVE messages(5) to MSG5O
                     MOVE messages(6) to MSG6O
                     MOVE messages(7) to MSG7O
                     MOVE messages(8) to MSG8O
                     MOVE messages(9) to MSG9O
               END-EVALUATE
            EXEC CICS SEND CONTROL ERASE END-EXEC
            EXEC CICS SEND MAP('MINIMAP')
                 MAPSET('MINIMAP')
                 FREEKB ERASE END-EXEC
            EXEC CICS RETURN TRANSID ('LOAN')
                 COMMAREA (COMMAREA-BUFFER) END-EXEC.

         MAIN-PROCESS-EXIT.
            EXEC CICS RETURN END-EXEC.
            EXIT.
      ***********************************************************
      * Main Program loop ENDS here....                         *
      ***********************************************************

        EXIT-TRANSACTION SECTION.
      *********************************************************
      * End of this transaction, put out message and die...
      *********************************************************

            EXEC CICS SEND CONTROL ERASE END-EXEC
            MOVE 'LOANCICS Session Over' to MESSAGE-TO-SEND.
            EXEC CICS SEND TEXT FROM(MESSAGE-TO-SEND)
                 ERASE
                 FREEKB
            END-EXEC.
            EXEC CICS RETURN END-EXEC.

        EXIT-TRANSACTION-EXIT.
           EXIT.
