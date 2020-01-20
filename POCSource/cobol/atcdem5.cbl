       IDENTIFICATION DIVISION.
      ******************************************************
      *  PROGRAM NAME: DTDEM5  VERSION TDM                 *
      *                                                    *
      *  OBJECTIVES OF TESTCASE:                           *
      *                                                    *
      *       DEMO INTERACTIVE DEBUG TOOL                  *
      *                                                    *
      ******************************************************
       PROGRAM-ID.             ATCDEM5.
       AUTHOR.                 TIM MAGEE.
           DATE-WRITTEN.       03/22/05.
           DATE-COMPILED.      CURRENT-DATE.
           INSTALLATION.       IBM LEXINGTON.
           REMARKS.
              PURPOSE.
              APPLICATION TESTING COLLECTION AND DEBUG TOOL FUNCTONS
      *    SKIP3
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        IBM-370.
       OBJECT-COMPUTER.        IBM-370.
      *    EJECT

       DATA DIVISION.

      **********************************************************
      * WORKING STORAGE                                        *
      **********************************************************
       WORKING-STORAGE SECTION.
       01 TDPARM1      PIC 99 VALUE 5.
       01 TDPARM2      PIC 99 VALUE 2.
       01 P1PARM1      PIC 99 VALUE 0.
       01 BASE-YEAR    PIC 99 VALUE 99.
       01 BASE-CC      PIC 99 VALUE 20.

       01 LOOPCT       PIC 9999999 VALUE 0.

       01  INPUT-RECORD-1              PIC X(2048).
       01  INPUT-RECORD-2              PIC X(2048).

      **********************************************************
      *  LINKAGE FOR PASSED PROGRAM PARAMITER                  *
      **********************************************************
       LINKAGE SECTION.
        01  PARM-VAL.
           05  DEMO-TYPE               PIC X(2).
           05  PARM-YEAR               PIC 9(2).

      **********************************************************
      * IN LINE LOGIC                                          *
      **********************************************************
       PROCEDURE DIVISION USING PARM-VAL.

       PROGD.
           PERFORM WITH TEST BEFORE UNTIL TDPARM1 > 0
             SUBTRACT 1 FROM TDPARM1
           END-PERFORM


           IF TDPARM2 = 0
             PERFORM PROCD
           END-IF

           PERFORM WITH TEST BEFORE UNTIL TDPARM2 > 0
             SUBTRACT 1 FROM TDPARM2
           END-PERFORM

           IF DEMO-TYPE = 'AM'
             PERFORM PROCE
           END-IF

           GOBACK
           .


       PROCD.
           MOVE 10 TO P1PARM1
           GOBACK
           .

        PROCE.
            MOVE 1000000 TO LOOPCT.

            PERFORM WITH TEST BEFORE UNTIL LOOPCT < 2
              MOVE INPUT-RECORD-1 TO INPUT-RECORD-2
              SUBTRACT 1 FROM LOOPCT
            END-PERFORM

            GOBACK
            .

