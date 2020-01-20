       IDENTIFICATION DIVISION.
        PROGRAM-ID. CALLER.
      ******************************************************************
      * This program calls a subprogram installed as a shareable image.*
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 A-GROUP. 01 B-GROUP.
           02 FIELD1. 02 FIELD1.
            03 A PIC X. 03 A PIC X.
            03 B PIC 9. 03 C PIC XX.
            03 C PIC XX. 03 E PIC XXX.
            03 D PIC 99.
            03 E PIC XXX.
        PROCEDURE DIVISION.
        0.
             MOVE CORRESPONDING
                A-GROUP TO B-GROUP.
                CALL "SUBSHR1"
                      ON EXCEPTION
                           DISPLAY "First CALL failed. Program aborted."
                END-CALL.
                STOP RUN.
        END PROGRAM CALLER.
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SUBSHR1.
      ******************************************************************
      * This program is linked as a shareable image. When it is called,*
      * it calls another program installed as a shareable image. *
      ******************************************************************
        PROCEDURE DIVISION.
        0.
                DISPLAY "Call to SUBSHR1 successful. Calling SUBSHR2.".
                CALL "SUBSHR2"
                      ON EXCEPTION
               DISPLAY "Second call failed. Control returned to CALL"
               END-CALL.
        END PROGRAM SUBSHR1.
        IDENTIFICATION DIVISION.
        PROGRAM-ID. SUBSHR2.
      ****************************************************************
      * This program is linked as a shareable image and is called by *
      * another shareable image. *
      ****************************************************************
        PROCEDURE DIVISION.
        0.
                DISPLAY "Call to SUBSHR2 successful!".
        END PROGRAM SUBSHR2.
