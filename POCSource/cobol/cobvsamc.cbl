       IDENTIFICATION DIVISION.
      ******************************************************
      *  PROGRAM NAME: COBVSAM                             *
      *                                                    *
      *  PROGRAMMER: TIMOTHY DAVID MAGEE                   *
      *              LEXINGTON, KENTUCKY                   *
      *                                                    *
      *  DATE: 07/04/2005                                  *
      *                                                    *
      *  OBJECTIVES OF TESTCASE:                           *
      *                                                    *
      *       DEMO OF THE APA TOOL CAPTURING THE LE        *
      *       CALLS TO THE RANDOM NUMBER GERNERATION       *
      *       SERVICE AND USING THE RANDOM NUMBER RETURNED *
      *       TO READ THE VSAM INDEXED FILE                *
      *                                                    *
      *  FUNCTION OF THIS COMPILE UNIT:                    *
      *                                                    *
      *       THIS IS THE MAIN DRIVER, COBVSAM THAT WILL   *
      *       CALL THE LE API CEERAND0 TO RETURN A RANDOM  *
      *       NUMBER.  THIS NUMBER WILL BE USED TO READ    *
      *       THE VSAM INDEXED FILE TMVSAM.  THE NUMBER OF *
      *       CALL TO LE AND READS FOR OF FILE IS DEFINED  *
      *       BY A PASSED PROGRAM PARMITER.                *
      *                                                    *
      ******************************************************
       PROGRAM-ID. COBVSAM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT COMPANY-FILE     ASSIGN TO TMVSAM
                   ORGANIZATION IS  INDEXED
                   ACCESS       IS  RANDOM
                   RECORD KEY   IS  WS-VSAM-KEY1
                   FILE STATUS  IS  COMP-FILE-STATUS.

       DATA DIVISION.

      **********************************************************
      *  FILE SECTION FOR VSAM FILE TO BE READ                 *
      **********************************************************
       FILE SECTION.
       FD  COMPANY-FILE.

       COPY RECBUF1.

      **********************************************************
      *  WORKING STORAGE SECTION FOR COBVSAM                   *
      **********************************************************
       WORKING-STORAGE SECTION.

       01 COMP-FILE-STATUS  PIC X(2)  VALUE SPACES.
       01 SEED PIC S9(9) BINARY.
       01 RANDNUM COMP-2.
       01 TEST-NUM PIC 9(3) COMP-3.
       01 TEST-PIC PIC X(3).
       01  RAND-STR.
           05  LEAD-VALUE             PIC X(1) VALUE '1'.
           05  RAND-NUM               PIC X(3).
       01 TEST-PIC2 PIC X(4).
       01 RETURN-VALUE PIC X(2) VALUE SPACES.
       01 FC.
          02 Condition-Token-Value.
          COPY CEEIGZCT.
              03 Case-1-Condition-ID.
                 04 Severity PIC S9(4) BINARY.
                 04 Msg-No PIC S9(4) BINARY.
              03 Case-2-Condition-ID
                       REDEFINES Case-1-Condition-ID.
                 04 Class-Code PIC S9(4) BINARY.
                 04 Cause-Code PIC S9(4) BINARY.
              03 Case-Sev-Ctl PIC X.
              03 Facility-ID PIC XXX.
           02 I-S-Info PIC S9(9) BINARY.

      **********************************************************
      *  LINKAGE FOR PASSED PROGRAM PARAMITER OF VSAM READ     *
      *  COUNT                                                 *
      **********************************************************
       LINKAGE SECTION.
        01  APA-RUN-PARM.
           05  PARM-LENGTH             PIC X(2).
           05  READ-COUNT              PIC 9(4).

      **********************************************************
      *  PROCEDURE DEVISION USING THE PASS CALL COUNT          *
      **********************************************************
       PROCEDURE DIVISION USING APA-RUN-PARM.

       PARA-CBLRAN0.

      ************************************************
      * OPEN VSAM FILE
      ************************************************
           OPEN INPUT COMPANY-FILE.

      *****************************************************
      * FOR THE NUMBER OF TIMES PROVIED IN THE INVOCATION
      * PARM, CALL THE LE API TO GET THE RANDOM NUMBER
      * TO BE USED AS A KEY, THEN READ THE VSAM FILE
      * WITH THE KEY.
      *****************************************************
            PERFORM WITH TEST BEFORE UNTIL READ-COUNT = 1
              PERFORM READ-VSAM-UP
              SUBTRACT 1 FROM READ-COUNT
            END-PERFORM

            GOBACK.

       READ-VSAM-UP SECTION.
      ************************************************
      * GET A RANDOM NUMBER AND USE THE NUMBER TO
      * READ THE VSAM FILE
      ************************************************

              PERFORM RANDOM-NUMS
              PERFORM READ-VSAMFILE
              GO TO READ-VSAM-UP-EXIT
            .
       READ-VSAM-UP-EXIT.
           EXIT.


       RANDOM-NUMS SECTION.
      ************************************************
      * RANDOM-NUMS PARAGRAPH:
      *
      *   FUNTION:  WHEN CALL THIS PARAGRAPH WILL
      *             RETURN A RANDOM NUMBER TO BE
      *             USED AS A VSAM KEY. THE NUMBER
      *             WILL BE AQUIRED BY A LE RUNTIME
      *             FUNCTION CALL CEERAN0
      ************************************************

      ************************************************
      * SPECIFY 0 FOR SEED,SO THE SEED WILL BE
      * DERIVED FROM THE CURRENT GREENWICH MEAN TIme
      ************************************************
           MOVE 0 TO SEED.

      ************************************************
      * CALL CEERAN0 TO RETURN RANDOM NUMBER BETWEen
      * 0.0 AND 1.0
      ************************************************
           CALL "CEERAN0" USING SEED, RANDNUM, FC.
           MOVE SEED TO TEST-NUM.
           MOVE SEED TO TEST-PIC.
           MOVE TEST-PIC TO RAND-NUM.

      ************************************************
      * IF CEERAN0 RUNS SUCCESSFULLY,DISPLAY RESULT.
      ************************************************
           IF CEE000 of FC THEN
               MOVE 0 TO RETURN-VALUE
           ELSE
               DISPLAY "CEERAN0 failed with msg "
                   Msg-No of FC UPON CONSOLE
               STOP RUN
           END-IF
           .
       RANDOM-NUMS-EXIT.
           EXIT.

       READ-VSAMFILE SECTION.
      ************************************************
      * READ-VSAMFILE PARAGRAPH:
      *
      *   FUNTION:  THIS PARAGRAPH WILL READ THE
      *             VSAM FILE WITH THE KEY RETURNED
      *             BY THE RANDON NUMBER CALL
      ************************************************
           MOVE RAND-STR TO WS-VSAM-KEY1.
           READ    COMPANY-FILE

           EVALUATE COMP-FILE-STATUS
              WHEN ZERO
                   MOVE 0 TO RETURN-VALUE
              WHEN 23
                   MOVE 23 TO RETURN-VALUE
              WHEN OTHER
                   DISPLAY 'FILE STATUS ' COMP-FILE-STATUS
                   DISPLAY 'RECORD KEY  ' WS-VSAM-KEY1
                   DISPLAY 'ERRORS ON START OF COMPANY NAME FILE'
                   MOVE 99 TO RETURN-VALUE
           END-EVALUATE
           .
       READ-VSAMFILE-EXIT.
           EXIT.

