       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRKSFINL.
      *    CONTRACT REDEMPTION ANALYSIS - A YEAR 2000 EXAMPLE PROGRAM.
      *    IN INITIALISE-AND-GET-PARAMETERS, SET THE P-START-YEAR TO
      *    START ANALYSIS OF YEARS AFTER 1994 AND THE GENERATED REPORT
      *    GIVES THE WRONG FIGURES.  PRIOR TO THAT DATE THEY ARE OK AS:
      *
      *         CONTRACT REDEMPTION ANALYSIS RUN ON 23/09/97
      *                      1991  1992  1993  1994
      *    04 YEARS EARLY       0     1     0     2
      *    03 YEARS EARLY       1     0     2     1
      *    02 YEARS EARLY       0     2     1     0
      *    1 YEAR EARLY         2     1     0     0
      *    AS CONTRACTED        1     0     0     0
      *
      *    TOTAL REFUNDABLE     0
      *
      *    ALPHA VERSION 0.4 - MW  - 25/9/97
      *    BETA VERSION  0.5 - WLT - 5/12/97 - FIXED FOR MVS COBOL II

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
                     IBM-370.
       OBJECT-COMPUTER.
                     IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT YEAR-FILE ASSIGN YEARFILE
               ORGANIZATION SEQUENTIAL
               FILE STATUS FILE-STATUS.
           SELECT REDEMPTION-FILE ASSIGN REDEEDAT
               ORGANIZATION INDEXED
               ACCESS MODE DYNAMIC
               RECORD KEY IS RD-KEY
               FILE STATUS FILE-STATUS.
           SELECT PRINT-FILE ASSIGN REDEEPRT.
       DATA DIVISION.
       FILE SECTION.

       FD  YEAR-FILE.
      *    SUMMARY OF CONTRACTS AT YEAR END.
      *    THE LAST RECORD ON THE FILE IS A TRAILER WITH A KEY OF ALL 9S
      *    FORMATS FOR: YR-START-DATE   IS YYMMDD
      *                 YR-END-DATE     IS DDMMYY
      *                 YR-PAYMENT-DATE IS DDMMYY
       01  YEAR-REC.
        03 YR-KEY.
           05  YR-START-DATE               PIC 9(6).
           05  YR-CONTRACT-NO              PIC X(20).
        03 YR-END-DATE                     PIC 9(6) COMP-3.
        03 YR-PAYMENT-DATE                 PIC 9(6) COMP.
        03 YR-PAYMENT-AMOUNT               PIC 9(6)V99 COMP.
        03 YR-PAYMENT-REF                  PIC X(15).
        03 YR-CONTRACT-TERM                PIC 99   COMP.
        03 YR-CONTRACT-STATUS              PIC XX.
           88  CONTRACT-ENDED              VALUE 'PY' 'CL' 'FL' 'XX'.

       01  YEAR-TRAILER.
        03 YR-KEY.
           05  YR-START-DATE               PIC 9(6).
           05  YR-CONTRACT-NO              PIC X(20).
        03 YR-RECORD-CNT                   PIC 9(9).
        03 YR-TOTAL-AMOUNT                 PIC 9(8)V99.

       FD  REDEMPTION-FILE.
       01  REDEMPTION-REC.
        03 RD-KEY.
           05  RD-END-DATE-N               PIC 9(6).
           05  RD-END-DATE REDEFINES RD-END-DATE-N.
            07  RD-END-YY                  PIC 99.
            07  RD-END-MM                  PIC 99.
            07  RD-END-DD                  PIC 99.
           05   RD-CONTRACT-NO             PIC X(20).
        03 RD-START-DATE-N                 PIC 9(6).
        03 RD-START-DATE REDEFINES RD-START-DATE-N.
           05  RD-START-YY                 PIC 99.
           05  RD-START-MM                 PIC 99.
           05  RD-START-DD                 PIC 99.
        03 RD-CONTRACT-TERM                PIC 99.
        03 RD-CONTRACT-STATUS              PIC XX.

       FD  PRINT-FILE.
       01  PRINT-REC                       PIC X(80).
       01  PRINT-REC2.
        03 PR-TEXT                         PIC X(16).
        03 PR-YEAR-ENTRY OCCURS 10.
           05  PR-NUM                      PIC Z(5)9.
           05  FILLER REDEFINES PR-NUM.
            07 FILLER                      PIC X(2).
            07 PR-YEAR                     PIC 9(4).

       WORKING-STORAGE SECTION.
       77  REDEEDAT                        PIC X(12) VALUE 'REDEEM.DAT'.
       77  REDEEPRT                        PIC X(12) VALUE 'REDEEM.PRT'.
       77  SUB1                            PIC 99.
       77  SUB2                            PIC 99.
       77  FILE-STATUS                     PIC XX.
       77  TEST-STATUS                     PIC X VALUE 'Y'.
        88 TEST-MODE                       VALUE 'Y'.

       01  WS-REPORT-TABLE.
        03 WS-RT-REFUNDABLE                PIC 9(6).
        03 WS-RT-YEAR-COUNT                PIC 99.
        03 WS-RT-YEAR-TO-REPORT-ENTRY      OCCURS 10.
           05  WS-RT-YEAR                  PIC 99.
           05  WS-RT-YEARS-EARLY-ENTRY     OCCURS 25.
            07 WS-RT-COUNT                 PIC 9(6).

       01  PARAM-RECORD.
        03 P-START-YEAR                    PIC 99.
        03 P-YEARS-TO-REPORT               PIC 99.
        03 P-CONTRACT-LENGTH               PIC 99.

       01  YEAR-FILENAME.
        03 FILLER                          PIC X(4) VALUE 'YEAR'.
        03 YEAR-FILENAME-YY                PIC 99.
        03 FILLER                          PIC X(4) VALUE '.DAT'.
       01  YEARFILE REDEFINES YEAR-FILENAME
                                           PIC X(10).

       01  DDMMYY-N                        PIC 9(6).
       01  DDMMYY REDEFINES DDMMYY-N.
        03 DD                              PIC XX.
        03 MM                              PIC XX.
        03 YY                              PIC XX.
       01  YYMMDD-N                        PIC 9(6).
       01  YYMMDD REDEFINES YYMMDD-N.
        03 YY                              PIC 99.
        03 MM                              PIC 99.
        03 DD                              PIC 99.

       77  TMP-YY                          PIC 9(2) COMP.
       77  TMP-END-YY                      PIC 9(4) COMP.
       77  TMP-YYYY                        PIC 9(6) COMP.
       77  YEAR-EOF                        PIC 9.
       77  REDEMPTION-EOF                  PIC 9.
       77  TODAYS-DATE                     PIC 9(6).
       77  TMP-99                          PIC 99.
       77  MONTHS-ELAPSED-SINCE-JAN-1900   PIC S9(5) COMP-3.
       77  MMMM-CONTRACT-TERM-TO-DATE      PIC S9(5) COMP-3.

       01  PRINT-HEAD.
        03 FILLER                          PIC X(5) VALUE SPACES.
        03 FILLER                          PIC X(36) VALUE
                   'CONTRACT REDEMPTION ANALYSIS RUN ON '.
        03 HEAD-DATE                       PIC XX/XX/XX.

       PROCEDURE DIVISION.
       TOP-LEVEL SECTION.
           PERFORM INITIALIZE-AND-GET-PARAMETERS
           PERFORM BUILD-REDEMPTION-FILE
           PERFORM PROCESS-REDEMPTION-FILE
           PERFORM PRINT-REPORT

           STOP RUN.

       INITIALIZE-AND-GET-PARAMETERS SECTION.
           IF TEST-MODE
               MOVE 02 TO P-START-YEAR
      *             02 SHOULD MEAN THE YEAR 2002 OF COURSE
               MOVE 98 TO P-START-YEAR
               MOVE 96 TO P-START-YEAR
               MOVE 91 TO P-START-YEAR
               MOVE 4  TO P-YEARS-TO-REPORT
               MOVE 5  TO P-CONTRACT-LENGTH
               PERFORM BUILD-TEST-FILES
           ELSE
               ACCEPT PARAM-RECORD
           END-IF
           IF P-YEARS-TO-REPORT = 0 OR > 10 OR
                       P-CONTRACT-LENGTH = 0 OR > 25
               DISPLAY 'ERROR - YEARS TO REPORT = 0 OR > 10'
               DISPLAY '      - OR CONTRACT LENGTH = 0 OR > 25'
               STOP RUN
           END-IF
           INITIALIZE WS-REPORT-TABLE
           MOVE P-START-YEAR TO TMP-YY
           PERFORM P-YEARS-TO-REPORT TIMES
               ADD 1 TO WS-RT-YEAR-COUNT
               MOVE TMP-YY TO WS-RT-YEAR(WS-RT-YEAR-COUNT)
               ADD 1 TO TMP-YY
           END-PERFORM
           ACCEPT YYMMDD FROM DATE
           MOVE CORRESPONDING YYMMDD TO DDMMYY
           MOVE DDMMYY TO TODAYS-DATE
           MOVE TODAYS-DATE TO HEAD-DATE
           COMPUTE MONTHS-ELAPSED-SINCE-JAN-1900 =
                       (YY OF YYMMDD * 12) + MM OF YYMMDD
           .

       BUILD-REDEMPTION-FILE SECTION.
      *    *> FIRST CREATE A NEW, EMPTY FILE.
           OPEN OUTPUT REDEMPTION-FILE
           CLOSE REDEMPTION-FILE
      *    *> NOW OPEN I-O SO WE CAN DELETE RECORDS.
           OPEN I-O REDEMPTION-FILE
           PERFORM OPEN-CHECK
           ADD P-START-YEAR P-YEARS-TO-REPORT -1 GIVING TMP-END-YY
           PERFORM VARYING TMP-YY FROM P-START-YEAR BY 1 UNTIL
                           TMP-YY > TMP-END-YY
               MOVE TMP-YY TO YEAR-FILENAME-YY
               OPEN INPUT YEAR-FILE
               PERFORM OPEN-CHECK
               MOVE 0 TO YEAR-EOF
               PERFORM PROCESS-YEAR-RECORD UNTIL YEAR-EOF = 1
               CLOSE YEAR-FILE
           END-PERFORM
           CLOSE REDEMPTION-FILE
           .

       PROCESS-YEAR-RECORD SECTION.
           READ YEAR-FILE AT END
               MOVE 1 TO YEAR-EOF
           NOT AT END
               IF YR-START-DATE OF YEAR-TRAILER = '999999'
      *            *> TREAT TRAILER RECORD AS EOF
                   MOVE 1 TO YEAR-EOF
               ELSE
                   MOVE YR-END-DATE TO DDMMYY-N
                   MOVE CORRESPONDING DDMMYY TO YYMMDD
                   MOVE YYMMDD TO RD-END-DATE
                   MOVE YR-CONTRACT-NO   OF YEAR-REC TO RD-CONTRACT-NO
                   MOVE YR-START-DATE    OF YEAR-REC TO RD-START-DATE-N
                   MOVE YR-CONTRACT-TERM OF YEAR-REC TO RD-CONTRACT-TERM
                   MOVE YR-CONTRACT-STATUS TO RD-CONTRACT-STATUS
                   WRITE REDEMPTION-REC
                   IF FILE-STATUS = '22'
      *                *> WE ALREADY HAVE A RECORD HERE FROM AN EARLIER
      *                *> YEAR, BUT THE LATER ONE IS KING
                       DELETE REDEMPTION-FILE
                       WRITE REDEMPTION-REC
                   END-IF
               END-IF
           END-READ
           .

       PROCESS-REDEMPTION-FILE SECTION.
           OPEN INPUT REDEMPTION-FILE
           PERFORM OPEN-CHECK
           MOVE 0 TO REDEMPTION-EOF
           PERFORM PROCESS-REDEMPTION-RECORD UNTIL REDEMPTION-EOF = 1
           CLOSE REDEMPTION-FILE
           .

       PROCESS-REDEMPTION-RECORD SECTION.
           READ REDEMPTION-FILE NEXT AT END
               MOVE 1 TO REDEMPTION-EOF
           NOT AT END
               EVALUATE RD-END-DATE
               WHEN '930801'
               WHEN '910221'
                   IF RD-CONTRACT-STATUS = 'XX'
                       GO TO PDR-EXIT
                   END-IF
               END-EVALUATE
               IF RD-END-YY < P-START-YEAR OR
                           > (P-START-YEAR + P-YEARS-TO-REPORT - 1)
                   CONTINUE
               ELSE
                   COMPUTE YYMMDD-N =
                           RD-START-DATE-N + (RD-CONTRACT-TERM * 10000)
                   IF YYMMDD <= RD-END-DATE
      *                *> CONTRACT WAS FULFILLED OK
                       GO TO PDR-EXIT
                   ELSE
      *                *> EARLY REDEMPTION OF THE CONTRACT
                       COMPUTE MMMM-CONTRACT-TERM-TO-DATE =
                               (YY OF YYMMDD * 12) + MM OF YYMMDD
                       SUBTRACT MMMM-CONTRACT-TERM-TO-DATE FROM
                               MONTHS-ELAPSED-SINCE-JAN-1900 GIVING
                               MMMM-CONTRACT-TERM-TO-DATE
                       IF MMMM-CONTRACT-TERM-TO-DATE <= 6
      *                    *> CANCELLED WITH NO PENALTY IF STARTED
      *                    *> WITHIN THE LAST 6 MONTHS
                           ADD 1 TO WS-RT-REFUNDABLE
                       END-IF

                       SUBTRACT P-START-YEAR -1 FROM RD-END-YY
                                               GIVING SUB1
                       SUBTRACT RD-END-DATE-N FROM YYMMDD-N
                       COMPUTE SUB2 = (YYMMDD-N + 9999) / 10000
                       ADD 1 TO WS-RT-COUNT(SUB1 SUB2)
                   END-IF
               END-IF
           END-READ
           .
       PDR-EXIT.
           EXIT.

       PRINT-REPORT SECTION.
           OPEN OUTPUT PRINT-FILE
           MOVE PRINT-HEAD TO PRINT-REC
           PERFORM PRINT-LINE
           ADD 1900 P-START-YEAR GIVING TMP-YYYY
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL
                               SUB1 > P-YEARS-TO-REPORT
               MOVE TMP-YYYY TO PR-YEAR(SUB1)
               ADD 1 TO TMP-YYYY
           END-PERFORM
           PERFORM PRINT-LINE
           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL
                               SUB2 > P-CONTRACT-LENGTH
               SUBTRACT SUB2 FROM P-CONTRACT-LENGTH GIVING TMP-99
               EVALUATE TMP-99
               WHEN 0
                   MOVE 'AS CONTRACTED' TO PR-TEXT
               WHEN 1
                   MOVE '1 YEAR EARLY' TO PR-TEXT
               WHEN OTHER
               STRING TMP-99 ' YEARS EARLY' DELIMITED BY SIZE INTO
                                       PR-TEXT
               END-EVALUATE
               PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL
                                   SUB1 > P-YEARS-TO-REPORT
                   MOVE WS-RT-COUNT(SUB1 SUB2) TO PR-NUM(SUB1)
               END-PERFORM
               PERFORM PRINT-LINE
           END-PERFORM
           PERFORM PRINT-LINE
           MOVE 'TOTAL REFUNDABLE' TO PR-TEXT
           MOVE WS-RT-REFUNDABLE TO PR-NUM(1)
           PERFORM PRINT-LINE
           CLOSE PRINT-FILE
           .

       PRINT-LINE SECTION.
           IF TEST-MODE
               DISPLAY PRINT-REC
           ELSE
               WRITE PRINT-REC AFTER 1
           END-IF
           MOVE SPACE TO PRINT-REC
           .

       BUILD-TEST-FILES SECTION.
      *    HERE WE CREATE SMALL TEST FILES TO TEST FOR YEAR2000 PROBLEMS
      *    WE WANT THE REPORT TO LOOK THE SAME WHATEVER THE SELECTED
      *    START DATE, SO WE HAVE TO FIDDLE TO GET THE YEAR RIGHT.
           MOVE P-START-YEAR TO TMP-END-YY
           PERFORM P-YEARS-TO-REPORT TIMES
               MULTIPLY P-START-YEAR BY 10000 GIVING TMP-YYYY
               MOVE TMP-END-YY TO YEAR-FILENAME-YY
               OPEN OUTPUT YEAR-FILE
               PERFORM OPEN-CHECK
      *        MOVE 890716 TO YR-START-DATE OF YEAR-REC
               MOVE 0716 TO YR-START-DATE OF YEAR-REC
               ADD -20000 TMP-YYYY TO YR-START-DATE OF YEAR-REC
               MOVE ALL 'A' TO YR-CONTRACT-NO  OF YEAR-REC
               MOVE 210200 TO YR-END-DATE
               ADD TMP-END-YY TO YR-END-DATE
               MOVE 5 TO YR-CONTRACT-TERM
               MOVE 'PY' TO YR-CONTRACT-STATUS
               WRITE YEAR-REC
      *        MOVE 910420 TO YR-START-DATE  OF YEAR-REC
               MOVE 0420 TO YR-START-DATE OF YEAR-REC
               ADD 0 TMP-YYYY TO YR-START-DATE OF YEAR-REC
               MOVE ALL 'B' TO YR-CONTRACT-NO  OF YEAR-REC
               MOVE 161100 TO YR-END-DATE
               ADD TMP-END-YY TO YR-END-DATE
               MOVE 5 TO YR-CONTRACT-TERM
               MOVE 'PY' TO YR-CONTRACT-STATUS
               WRITE YEAR-REC
      *        MOVE 880130 TO YR-START-DATE  OF YEAR-REC
               MOVE 0130 TO YR-START-DATE OF YEAR-REC
               ADD -30000 TMP-YYYY TO YR-START-DATE OF YEAR-REC
               MOVE ALL 'C' TO YR-CONTRACT-NO OF YEAR-REC
               MOVE 070900 TO YR-END-DATE
               ADD TMP-END-YY TO YR-END-DATE
               MOVE 5 TO YR-CONTRACT-TERM
               MOVE 'PY' TO YR-CONTRACT-STATUS
               WRITE YEAR-REC
      *        MOVE 891030 TO YR-START-DATE  OF YEAR-REC
               MOVE 1030 TO YR-START-DATE OF YEAR-REC
      *        ADD -20000 TMP-YYYY TO YR-START-DATE OF YEAR-REC
               ADD TMP-YYYY TO YR-START-DATE OF YEAR-REC
               SUBTRACT 20000 FROM YR-START-DATE OF YEAR-REC
               MOVE ALL 'D' TO YR-CONTRACT-NO OF YEAR-REC
               MOVE 070900 TO YR-END-DATE
               ADD TMP-END-YY TO YR-END-DATE
               MOVE 5 TO YR-CONTRACT-TERM
               MOVE 'PY' TO YR-CONTRACT-STATUS
               WRITE YEAR-REC
      *        MOVE 850828 TO YR-START-DATE  OF YEAR-REC
               MOVE 0828 TO YR-START-DATE OF YEAR-REC
               ADD -60000 TMP-YYYY TO YR-START-DATE OF YEAR-REC
               MOVE ALL 'E' TO YR-CONTRACT-NO  OF YEAR-REC
               MOVE 280800 TO YR-END-DATE
               ADD TMP-END-YY TO YR-END-DATE
               MOVE 5 TO YR-CONTRACT-TERM
               MOVE 'FL' TO YR-CONTRACT-STATUS
               WRITE YEAR-REC
               INITIALIZE YEAR-TRAILER
               MOVE ALL '9' TO YR-KEY OF YEAR-TRAILER
               WRITE YEAR-REC
               CLOSE YEAR-FILE
               ADD 1 TO TMP-END-YY
           END-PERFORM
           .

       OPEN-CHECK SECTION.
           IF FILE-STATUS = '00' OR '05'
               CONTINUE
           ELSE
               DISPLAY 'FILE OPEN ERROR STATUS = ' FILE-STATUS
               STOP RUN
           END-IF
           .

