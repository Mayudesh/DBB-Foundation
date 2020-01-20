000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.   test1.
003200*
000300* ***************************************************
000300* ***************************************************
000400* *** test1.cbl
000500* ***
000500* ***
001000* ***     The Program produces a report that shows
001100* ***     the Number of courses and credits that
001200* ***     a student has taken.  The input file has
001300* ***     two types of records, a student record showing
001400* ***     information about a student, followed by
001500* ***     a number of course records for that student.
001600* ***     Each course record shows the information
001700* ***     for a course taken by the student.
003000* ***
003100* ***************************************************
      * **************************************************
003300 INSTALLATION.  IBM.
003400 DATE-WRITTEN.  01-01-2009.
003500 DATE-COMPILED. 01-01-2009.
003600 SECURITY.   NONE.
003700 ENVIRONMENT DIVISION.
003800 CONFIGURATION SECTION.
003900 SOURCE-COMPUTER.   IBM.
004000 OBJECT-COMPUTER.   IBM.
004100 INPUT-OUTPUT SECTION.
004200 FILE-CONTROL.
004300     SELECT STUDENT-FILE   ASSIGN TO UT-S-STDNTFL
004300            ORGANIZATION IS SEQUENTIAL.
004400     SELECT CREDITS-REPORT ASSIGN TO UT-S-REPORTFL
004300            ORGANIZATION IS SEQUENTIAL.
004500 DATA DIVISION.
004600 FILE SECTION.
004700 FD  STUDENT-FILE
004800     LABEL RECORDS ARE STANDARD.
004900 01  STUDENT-RECORD.
005000     05  SR-NAME                 PIC X(19).
005100     05  FILLER                  PIC X(5).
005200     05  SR-ADDRESS              PIC X(20).
005300     05  FILLER                  PIC XXXXX.
005400     05  SR-PHONE                PIC X(7).
005500     05  FILLER                  PIC XXX.
005600     05  SR-BIRTH-DATE           PIC X(6).
005700     05  FILLER                  PIC XXXX.
005800     05  SR-RECORD-TYPE          PIC X.
005800     05  FILLER                  PIC XX.
005900 01  COURSE-RECORD.
006000     05  CR-NAME                 PIC X(21).
006100     05  FILLER                  PIC X(5).
006200     05  CR-COURSE-NUMBER        PIC X(5).
006300     05  FILLER                  PIC X(5).
006400     05  CR-CREDITS              PIC 9.
006500     05  FILLER                  PIC X(34).
006600     05  FILLER                  PIC XXXX.
006700 FD  CREDITS-REPORT
006800     LABEL RECORDS ARE STANDARD.
006900 01  REPORT-LINE-OUT             PIC X(60).
007000 WORKING-STORAGE SECTION.
007100 01  SWITCHES-IN-PROGRAM.
007200     05  SW-END-OF-DATA          PIC X VALUE 'N'.
007300         88  END-OF-DATA               VALUE 'Y'.
007400 01  ACCUMS-AND-COUNTERS.
007500     05  ACCUM-CREDITS           PIC 999 VALUE 0.
007600     05  CTR-COURSES             PIC 999 VALUE 0.
007700     05  CTR-STUDENTS            PIC S9(5) VALUE +1.
007800     05  CTR-LINES               PIC 99 VALUE 0.
007900 01  SAVE-AREAS.
008000     05  SAVE-NAME               PIC X(19).
008100 01  GRAND-TOTAL-LINE.
008200     05  FILLER                  PIC X(30)
008300              VALUE ' TOTAL STUDENTS PROCESSED IS: '.
           EXEC SQL INCLUDE CUST1 END-EXEC.
008400     05  GTL-STUDENT-COUNT       PIC ZZZZZ.
008500 01  DETAIL-LINE.
008600     05  FILLER                  PIC X(5) VALUE SPACE.
008700     05  DL-NAME                 PIC X(19).
008800     05  FILLER                  PIC X(8) VALUE SPACE.
008900     05  DL-COURSES              PIC ZZZ.
009000     05  FILLER                  PIC X(10) VALUE SPACE.
009100     05  DL-CREDITS              PIC ZZZZ.
009200 01  HEADING-1.
009300     05  FILLER                  PIC X(10) VALUE SPACE.
009400     05  FILLER                  PIC X(80) VALUE
009500         'S T U D E N T   C R E D I T S   R E P O R T'.
009600 01  HEADING-2.
009700     05  FILLER                  PIC X(5)  VALUE SPACE.
009800     05  FILLER                  PIC X(25) VALUE 'STUDENT NAME'.
009900     05  FILLER                  PIC X(15) VALUE 'COURSES'.
010000     05  FILLER                  PIC X(7)  VALUE 'CREDITS'.
      *
010100 PROCEDURE DIVISION.
010200 000-TOP-LEVEL.
010300     PERFORM 100-INITIALIZATION.
010400     PERFORM 200-PROCESS-RECORDS UNTIL END-OF-DATA.
010500     PERFORM 300-WRAP-UP.
010600     GOBACK.
010700 100-INITIALIZATION.
010800     OPEN INPUT  STUDENT-FILE.
010900     OPEN OUTPUT CREDITS-REPORT.
011000     PERFORM 211-PAGE-CHANGE-RTN.
011100     PERFORM 230-READ-A-RECORD.
011200     MOVE SR-NAME TO SAVE-NAME.
011300     ADD 1 TO CTR-STUDENTS.
011400 200-PROCESS-RECORDS.
011500     IF SR-NAME  IS EQUAL TO '7'
011600     THEN
011700         DISPLAY STUDENT-RECORD
011700         PERFORM 210-PROCESS-1-RECORDS
011800         MOVE SR-NAME TO SAVE-NAME
011900         ADD 1 TO CTR-STUDENTS
012000     ELSE
012100         PERFORM 220-PROCESS-2-RECORDS.
012200     PERFORM 230-READ-A-RECORD.
012300 210-PROCESS-1-RECORDS.
012400     IF CTR-LINES IS GREATER THAN 50
012500     THEN
012600         PERFORM 211-PAGE-CHANGE-RTN.
012700     PERFORM 212-BUILD-DETAIL-LINE.
012800     WRITE REPORT-LINE-OUT FROM DETAIL-LINE
012900         AFTER ADVANCING 1.
013000     MOVE ZERO TO CTR-COURSES.
013200 211-PAGE-CHANGE-RTN.
013300     WRITE REPORT-LINE-OUT FROM HEADING-1
013400         AFTER ADVANCING PAGE.
013500     WRITE REPORT-LINE-OUT FROM HEADING-2
013600         AFTER ADVANCING 2.
013700     MOVE ZERO TO CTR-LINES.
013800 212-BUILD-DETAIL-LINE.
013900     MOVE SAVE-NAME TO DL-NAME.
014000     MOVE CTR-COURSES TO DL-COURSES.
014100     MOVE ACCUM-CREDITS TO DL-CREDITS.
014200 220-PROCESS-2-RECORDS.
014300     ADD CR-CREDITS TO ACCUM-CREDITS.
014400     ADD 1 TO CTR-COURSES.
014500 230-READ-A-RECORD.
014600     READ STUDENT-FILE
014700         AT END MOVE 'Y' TO SW-END-OF-DATA.
014800 300-WRAP-UP.
014900     MOVE CTR-STUDENTS TO GTL-STUDENT-COUNT.
015000     WRITE REPORT-LINE-OUT FROM GRAND-TOTAL-LINE
015100         AFTER ADVANCING 2.
015200     CLOSE CREDITS-REPORT  STUDENT-FILE.
       310-File-Close.
