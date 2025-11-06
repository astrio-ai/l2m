       Identification division.
       Program-id. "ATSPTKTC" recursive.
       Environment division.
       Configuration section.
       Data Division.
       Working-Storage section.
      *----------------------------------------------------------------*
      * Common defintions                                              *
      *----------------------------------------------------------------*
       01  functionCode            pic 9(9) BINARY.
       01  envVariableNameLength   pic 9(9) BINARY.
       01  envVariableName         pic X(255).
       01  envVariableLength       pic 9(9) BINARY.
       01  envVariablePointer POINTER.
       01  envVariableValue        pic X(255).
       01  ws-length               pic 9(3).
       01  feedbackCode.
           02  CONDITION-TOKEN-VALUE.
           COPY  CEEIGZCT SUPPRESS.
               03  CASE-1-CONDITION-ID.
                  04  SEVERITY     PIC S9(4) BINARY.
                  04  MSG-NO       PIC S9(4) BINARY.
               03  CASE-SEV-CTL    PIC X.
               03  FACILITY-ID     PIC XXX.
           02  I-S-INFO            PIC S9(9) BINARY.
       01  rc                      pic s9(9) comp-5.
       01  W-IRRSPK00              pic X(8) VALUE 'IRRSPK00'.
       01  IRR-AREA.
           10  irr-workarea        pic X(1024).
           10  irr-safrc           pic 9(08) COMP.
           10  irr-racfrc          pic 9(08) COMP.
           10  irr-racfrsn         pic 9(08) COMP.
           10  IRR-ALET            pic 9(08) COMP.
           10  irr-functionCode    pic X(2).
           10  irr-optionWord      pic 9(08) COMP.
           10  irr-ticketOptions   pic X(04).
           10  irr-ticketOptions-ptr USAGE POINTER.
       01  IRR-APPLID.
           10  applid-length       pic 9(04) COMP.
           10  applid              pic X(8).
       01  IRR-IDENTITY.
           10  identity-length     pic 9(04) COMP.
           10  identity            pic X(240).
       01  IRR-PASSTICKET.
           10  passTicket-length   pic 9(04) COMP.
           07  passTicket          pic X(8).
       Local-Storage Section.
       Linkage section .
       01 envVariable              pic x(5000).
       Procedure division.
      *----------------------------------------------------------------*
      * Get the BAQUSERNAME environment variable                       *
      *----------------------------------------------------------------*
           INITIALIZE IRR-AREA.
           MOVE "BAQUSERNAME" to envVariableName.
           PERFORM CALL-GET-CEEENV THRU CALL-GET-CEEENV-END
           IF envVariableLength NOT = 0 THEN
              MOVE envVariable(1:envVariableLength) to identity
           ELSE
              DISPLAY envVariableName(1:envVariableNameLength)
               " NOT FOUND " envVariableNameLength
           END-IF.
      *----------------------------------------------------------------*
      * Get the ATSAPPLID environment variable                         *
      *----------------------------------------------------------------*
           MOVE "ATSAPPLID" to envVariableName.
           PERFORM CALL-GET-CEEENV THRU CALL-GET-CEEENV-END
           IF envVariableLength NOT = 0 THEN
              MOVE envVariable(1:envVariableLength) to applid
           ELSE
              DISPLAY envVariableName(1:envVariableNameLength)
               " NOT FOUND " envVariableNameLength
           END-IF.
           DISPLAY "ATSPTKTC-BAQUSERNAME: " identity(1:8).
           DISPLAY "ATSPTKTC-ATSAPPLID:   " applid.
      *----------------------------------------------------------------*
      * Build IRRSPK00 parameters                                      *
      *----------------------------------------------------------------*
           MOVE 0 to ws-length
           MOVE LENGTH OF identity to identity-length.
           INSPECT FUNCTION REVERSE (identity)
                  TALLYING ws-length FOR ALL SPACES.
           SUBTRACT ws-length FROM identity-length.
           MOVE 0 to ws-length
           MOVE LENGTH OF applid to applid-length.
           INSPECT FUNCTION REVERSE (applid)
                  TALLYING ws-length FOR ALL SPACES.
           SUBTRACT ws-length FROM applid-length.
           MOVE 8 to passTicket-length.
           MOVE ' ' to passTicket.
           MOVE X'0003' to irr-functionCode.
           MOVE X'00000001' to irr-ticketOptions.
           SET irr-ticketOptions-ptr to ADDRESS OF irr-ticketOptions.
      *----------------------------------------------------------------*
      * Call RACF service IRRSPK00 to obtain a pass ticket based       *
      *      on identity and applid                                    *
      *----------------------------------------------------------------*
           PERFORM CALL-RACF.
           IF irr-safrc NOT = zero then
              DISPLAY "SAF_return_code:     " irr-safrc
              DISPLAY "RACF_return_code:    " irr-racfrc
              DISPLAY "RACF_reason_code:    " irr-racfrsn
           End-if
           DISPLAY "ATSPTKTC-BAQPASSWORD: " passTicket
      *----------------------------------------------------------------*
      * Set the BAQPASSWORD environment variable                       *
      *----------------------------------------------------------------*
           MOVE "BAQPASSWORD" to envVariableName.
           MOVE 11 to envVariableNameLength.
           MOVE passTicket to envVariableValue.
           MOVE 8 to envVariableLength.
           PERFORM CALL-SET-CEEENV THRU CALL-SET-CEEENV-END
           Goback.
      *----------------------------------------------------------------*
      * Set environment variable                                       *
      *----------------------------------------------------------------*
       CALL-SET-CEEENV.
           MOVE 5 to functionCode.
           SET envVariablePointer to address of envVariableValue
           CALL "CEEENV" USING functionCode,
                                envVariableNameLength,
                                envVariableName,
                                envVariableLength,
                                envVariablePointer,
                                feedbackCode.
       CALL-SET-CEEENV-END.
      *----------------------------------------------------------------*
      * Get environment variable                                       *
      *----------------------------------------------------------------*
       CALL-GET-CEEENV.
            MOVE 1 to functionCode.
            MOVE ZERO to ws-length.
            INSPECT FUNCTION REVERSE (envVariableName)
                  TALLYING ws-length FOR LEADING SPACES.
            COMPUTE envVariableNameLength =
                 LENGTH OF envVariableName - ws-length.
            MOVE SPACES to envVariableValue.
            MOVE ZERO to envVariableLength.
            CALL "CEEENV" USING functionCode,
                                 envVariableNameLength,
                                 envVariableName,
                                 envVariableLength,
                                 envVariablePointer,
                                 feedbackCode.
            IF envVariableLength NOT = ZERO THEN
                 SET ADDRESS OF envVariable to envVariablePointer .
       CALL-GET-CEEENV-END.
      *----------------------------------------------------------------*
      * Call IRRSPK00 requesting a pass ticket                         *
      *----------------------------------------------------------------*
       CALL-RACF.
            CALL W-IRRSPK00 USING irr-workarea,
                  IRR-ALET, irr-safrc,
                  IRR-ALET, irr-racfrc,
                  IRR-ALET, irr-racfrsn,
                  IRR-ALET, irr-functionCode,
                  irr-optionWord,
                  IRR-PASSTICKET,
                  irr-ticketOptions-ptr,
                  IRR-IDENTITY,
                  IRR-APPLID.
       CALL-RACF-END.
       End program "ATSPTKTC".
