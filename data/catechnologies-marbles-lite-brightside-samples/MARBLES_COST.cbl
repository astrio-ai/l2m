      * ===============================================================
      * The user invokes this transaction (called MRBC) via:
      *   MRBC <VERB>
      *
      * Where:
      *  <VERB> = CRE|UPD|DEL
      *
      * Verb Functions:
      * CREate
      *  Invoked via MRBC CRE <COLOR> <INVENTORY> <COST>
      *
      *  Where:
      *   <COLOR> Exists in the EVENT.COLOR table.
      *   <INVENTORY> An integer value.
      *   <COST> An integer value.
      *
      *  Errors:
      *   MRBC002E - When a marble of <COLOR> already exists in the table
      *
      *  Example:
      *   MRBC CRE BLUE 10 4
      *
      * UPDate
      *  Invoked via MRBC UPD <COLOR> <INVENTORY> <COST>
      *
      *  Where:
      *   <COLOR> Is a color of an existing marble.
      *   <INVENTORY> An integer value.
      *   <COST> An integer value.
      *
      *  Errors:
      *   MRBC001E - When a marble of <COLOR> doesn't exist in the table
      *
      *  Example:
      *   MRBC UPD BLUE 1 5
      *
      * DELete
      *  Invoked via MRBC DEL <COLOR>
      *
      *  Where:
      *   <COLOR> Is a color of an existing marble.
      *
      *  Errors:
      *   MRBC001E - When a marble of <COLOR> doesn't exist in the table
      *
      *  Example:
      *   MRBC DEL BLUE
      *
      * Build via
      *  gulp build --source marbles.cbl && gulp refresh --source marbles.cbl
      * ===============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MARBLESC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      * ===============================================================
      * Map input / output areas
      * ===============================================================
       WORKING-STORAGE SECTION.
       01 BOOLEAN.
          02 BOOLEAN-FALSE PIC 9 VALUE 0.
          02 BOOLEAN-TRUE PIC 9 VALUE 1.
       01 WS-WORK.
          02 WS-WORK-INV PIC S9(4) COMP VALUE 0.
          02 WS-WORK-COST PIC S9(4) COMP VALUE 0.
          02 WS-WORK-COLOR PIC X(10).
          02 WS-WORK-ROW-COUNT PIC S9(4) COMP-3 VALUE 0.
       01 WS-RESULT.
          02 WS-RESULT-COLOR-FOUND PIC 9 VALUE 0.
          02 WS-RESULT-OPERATION-SUCCESS PIC 9 VALUE 0.
          02 WS-RESULT-VERB.
             03 WS-RESULT-VERB-CREATE PIC 9 VALUE 0.
             03 WS-RESULT-VERB-UPDATE PIC 9 VALUE 0.
             03 WS-RESULT-VERB-DELETE PIC 9 VALUE 0.
       01 WS-CONST.
          02 WS-CONST-CREATE PIC X(3) VALUE 'CRE'.
          02 WS-CONST-UPDATE PIC X(3) VALUE 'UPD'.
          02 WS-CONST-DELETE PIC X(3) VALUE 'DEL'.
          02 WS-CONST-SUCCESS PIC X(7) VALUE 'SUCCESS'.
       01 WS-ERROR-CODES.
          02 WS-ERROR-MARBLE-DNE PIC X(8) VALUE 'MRBC001E'.
          02 WS-ERROR-MARBLE-EXISTS PIC X(8) VALUE 'MRBC002E'.
       01 WS-CICS-INPUT PIC X(74) VALUE SPACES.
       01 WS-INPUT.
          05 WS-INPUT-TRAN-ID PIC X(4).
          05 WS-INPUT-VERB PIC X(3) VALUE SPACES.
          05 WS-INPUT-COLOR PIC X(10) VALUE SPACES.
          05 WS-INPUT-INV PIC 9(4) VALUE 0.
          05 WS-INPUT-COST PIC 9(4) VALUE 0.
       01 WS-OUTPUT PIC X(78).
       01 WS-OUTPUT-SUCCESS REDEFINES WS-OUTPUT.
          05 WS-OUTPUT-SUCCESS-TEXT PIC X(7).
          05 WS-OUTPUT-SUCCESS-FILLER PIC X(71).
       01 WS-OUTPUT-ERROR REDEFINES WS-OUTPUT.
          05 WS-OUTPUT-ERROR-CODE PIC X(8).
          05 WS-OUTPUT-ERROR-SPACE PIC X(1).
          05 WS-OUTPUT-ERROR-MESSAGE PIC X(69).
       01 WS-MSG-LENGTH PIC S9(4) COMP.
      * ===============================================================
      * Map SQL table for this transaction
      * ===============================================================
           EXEC SQL DECLARE EVENT.MARBLE TABLE
           ( COLOR                          VARCHAR(10) NOT NULL,
             INVENTORY                      INTEGER NOT NULL,
             COST                           INTEGER NOT NULL
           ) END-EXEC.
           EXEC SQL INCLUDE SQLCA END-EXEC.
      * ===============================================================
      * MRBL transaction
      * ===============================================================
       PROCEDURE DIVISION.
      *
      *     Initial working storage to known values
      *
            PERFORM INIT-WORK-AREAS.
      *
      *     Receive user input (e.g. ADD BLUE)
      *
            PERFORM GET-TRANS-INPUT.
      *
      *     Parse the user input into corresponding fields
      *
            PERFORM PARSE-CICS-INPUT.
      *
      *     Verify known input verb
      *
            PERFORM VERIFY-VERB.
      *
      *     Route to specific verb processing routine
      *
            IF WS-RESULT-VERB-CREATE = BOOLEAN-TRUE THEN
                PERFORM CHECK-IF-COLOR-FOUND
                IF WS-RESULT-COLOR-FOUND = BOOLEAN-FALSE THEN
                    PERFORM INSERT-COLOR
                    IF WS-RESULT-OPERATION-SUCCESS = BOOLEAN-TRUE THEN
                        PERFORM OUTPUT-SUCCESS
                    END-IF
                ELSE
                    PERFORM OUTPUT-MARBLE-ALREADY-EXISTS
                END-IF
            ELSE IF WS-RESULT-VERB-UPDATE = BOOLEAN-TRUE THEN
                PERFORM CHECK-IF-COLOR-FOUND
                IF WS-RESULT-COLOR-FOUND = BOOLEAN-TRUE THEN
                    PERFORM UPDATE-COLOR
                    IF WS-RESULT-OPERATION-SUCCESS = BOOLEAN-TRUE THEN
                        PERFORM OUTPUT-SUCCESS
                    END-IF
                ELSE
                    PERFORM OUTPUT-MARBLE-DOES-NOT-EXIST
                END-IF
            ELSE IF WS-RESULT-VERB-DELETE = BOOLEAN-TRUE THEN
                PERFORM CHECK-IF-COLOR-FOUND
                IF WS-RESULT-COLOR-FOUND = BOOLEAN-TRUE THEN
                    PERFORM DELETE-COLOR
                    IF WS-RESULT-OPERATION-SUCCESS = BOOLEAN-TRUE THEN
                        PERFORM OUTPUT-SUCCESS
                    END-IF
                ELSE
                    PERFORM OUTPUT-MARBLE-DOES-NOT-EXIST
                END-IF
            END-IF.
            PERFORM WRITE-OUTPUT
            GOBACK.
      * ===============================================================
      * Initialize working areas
      * ===============================================================
       INIT-WORK-AREAS.
      *
      *     Set work areas to known values
      *
            INITIALIZE SQLCA.
            MOVE 74 TO WS-MSG-LENGTH.
            MOVE SPACES TO WS-INPUT.
            MOVE SPACES TO WS-OUTPUT.
      * ===============================================================
      * Get transaction input
      * ===============================================================
       GET-TRANS-INPUT.
      *
      *     Receive input from user
      *
            EXEC CICS RECEIVE
                        INTO(WS-CICS-INPUT)
                        LENGTH(WS-MSG-LENGTH)
            END-EXEC.
      * ===============================================================
      * Parse the transaction input
      * ===============================================================
       PARSE-CICS-INPUT.
            UNSTRING WS-CICS-INPUT DELIMITED BY SPACE
                INTO WS-INPUT-TRAN-ID, WS-INPUT-VERB, WS-INPUT-COLOR,
                     WS-INPUT-INV, WS-INPUT-COST
            END-UNSTRING.
      * ===============================================================
      * Set indicator if verb is invalid
      * ===============================================================
       VERIFY-VERB.
      *
      *     Get count of rows on input color
      *
            IF WS-CONST-CREATE = WS-INPUT-VERB THEN
                MOVE BOOLEAN-TRUE TO WS-RESULT-VERB-CREATE
            ELSE IF WS-CONST-UPDATE = WS-INPUT-VERB THEN
                MOVE BOOLEAN-TRUE TO WS-RESULT-VERB-UPDATE
            ELSE IF WS-CONST-DELETE = WS-INPUT-VERB THEN
                MOVE BOOLEAN-TRUE TO WS-RESULT-VERB-DELETE
            ELSE
                MOVE 41 TO WS-MSG-LENGTH
                MOVE 'USE CRE|UPD|DEL' TO WS-OUTPUT
            END-IF.
      * ===============================================================
      * Write transaction response to user
      * ===============================================================
       WRITE-OUTPUT.
      *
      *     Send response to terminal
      *
            EXEC CICS SEND
                        FROM(WS-OUTPUT)
                        LENGTH(WS-MSG-LENGTH)
                        ERASE
            END-EXEC.
      * ===============================================================
      * Set indicator if input color is found
      * ===============================================================
       CHECK-IF-COLOR-FOUND.
      *
      *     Get count of rows on input color
      *
            EXEC SQL
                SELECT COUNT(*) INTO :WS-WORK-ROW-COUNT
                FROM EVENT.MARBLE
                WHERE COLOR = :WS-INPUT-COLOR
            END-EXEC.
      *
      *     If positive row count, mark "found" indicator
      *
            IF WS-WORK-ROW-COUNT > 0 THEN
                MOVE BOOLEAN-TRUE TO WS-RESULT-COLOR-FOUND
            ELSE
                MOVE BOOLEAN-FALSE TO WS-RESULT-COLOR-FOUND
            END-IF.
      * ===============================================================
      * Move the marble doesn't exist message into the buffer
      * ===============================================================
       OUTPUT-MARBLE-DOES-NOT-EXIST.
            MOVE 33 TO WS-MSG-LENGTH
            MOVE WS-ERROR-MARBLE-DNE TO WS-OUTPUT-ERROR-CODE
            MOVE 'UNKNOWN COLOR, CREate IT' TO WS-OUTPUT-ERROR-MESSAGE.
      * ===============================================================
      * Move the marble already exists message into the buffer
      * ===============================================================
       OUTPUT-MARBLE-ALREADY-EXISTS.
            MOVE 51 TO WS-MSG-LENGTH
            MOVE WS-ERROR-MARBLE-EXISTS TO WS-OUTPUT-ERROR-CODE
            MOVE 'MARBLE ALREADY EXISTS, UPDate or DELete IT'
                TO WS-OUTPUT-ERROR-MESSAGE.
      * ===============================================================
      * Move the success message into the buffer
      * ===============================================================
       OUTPUT-SUCCESS.
            MOVE 7 TO WS-MSG-LENGTH
            MOVE WS-CONST-SUCCESS TO WS-OUTPUT-SUCCESS-TEXT.
      * ===============================================================
      * Insert color
      * ===============================================================
       INSERT-COLOR.
      *
      *    Set current inventory into WS-WORK-INV
      *
           MOVE WS-INPUT-INV TO WS-WORK-INV
           MOVE WS-INPUT-COST TO WS-WORK-COST
           EXEC SQL
               INSERT INTO EVENT.MARBLE
               (COLOR,INVENTORY,COST)
               VALUES (
                     :WS-INPUT-COLOR,
                     :WS-WORK-INV,
                     :WS-WORK-COST)
           END-EXEC
           MOVE BOOLEAN-TRUE TO WS-RESULT-OPERATION-SUCCESS.
      * ===============================================================
      * Update current color
      * ===============================================================
       UPDATE-COLOR.
      *
      *    Set current inventory into WS-WORK-INV
      *
           MOVE WS-INPUT-INV TO WS-WORK-INV
           MOVE WS-INPUT-COST TO WS-WORK-COST
           EXEC SQL
               UPDATE EVENT.MARBLE
               SET INVENTORY = :WS-WORK-INV,
                   COST = :WS-WORK-COST
               WHERE COLOR = :WS-INPUT-COLOR
           END-EXEC
           MOVE BOOLEAN-TRUE TO WS-RESULT-OPERATION-SUCCESS.
      * ===============================================================
      * Delete color
      * ===============================================================
       DELETE-COLOR.
           EXEC SQL
               DELETE FROM EVENT.MARBLE
               WHERE COLOR = :WS-INPUT-COLOR
           END-EXEC
           MOVE BOOLEAN-TRUE TO WS-RESULT-OPERATION-SUCCESS.