000100 IDENTIFICATION DIVISION.                                         00010001
000200 PROGRAM-ID.  BNCHS602.                                           00020001
000300 AUTHOR. JON SAYLES.                                              00030001
000400 INSTALLATION. COBOL DEV Center.                                  00040001
000500 DATE-WRITTEN. 01/23/94.                                          00050002
000600 DATE-COMPILED. 01/23/88.                                         00060001
000700 SECURITY. CONFIDENTIAL PATIENT DATA.                             00070001
000800                                                                  00080001
000900******************************************************************00090001
001000*          LAST UPDATE DATE: 06/01/2010                           00100001
000100*                                                                 00110002
001200*          THIS PROGRAM EDITS A DAILY TREATMENT TRANSACTION FILE  00120001
001300*          PRODUCED BY DATA ENTRY OPERATORS FROM CICS SCREENS     00130001
001400*                                                                 00140001
001500*          IT CONTAINS EVERY TREATMENT FOR EVERY PATIENT IN THE   00150001
001600*          HOSPITAL.                                              00160001
001700*                                                                 00170001
001800*          THE PROGRAM EDITS EACH RECORD AGAINST A NUMBER OF      00180001
001900*          CRITERIA, BALANCES FINAL TOTALS AND WRITES GOOD        00190001
002000*          RECORDS TO AN OUTPUT FILE                              00200001
002100*                                                                 00210001
002200******************************************************************00220001
0KSSS                                                                   00230002
002400         INPUT FILE              -   DDS0001.TRMTDATA             00240001
002500                                                                  00250001
002600         VSAM MASTER FILE        -   DDS0001.PATMASTR             00260001
002700                                                                  00270001
002800         INPUT ERROR FILE        -   DDS0001.TRMTERR              00280001
002900                                                                  00290001
003000         OUTPUT FILE PRODUCED    -   DDS001.TRMTEDIT              00300001
003100                                                                  00310001
003200         DUMP FILE               -   SYSOUT                       00320001
003300                                                                  00330001
003400******************************************************************00340001
003500                                                                  00350001
003600 ENVIRONMENT DIVISION.                                            00360001
003700 CONFIGURATION SECTION.                                           00370001
003800 SOURCE-COMPUTER. IBM-390.                                        00380001
003900 OBJECT-COMPUTER. IBM-390.                                        00390001
004000 INPUT-OUTPUT SECTION.                                            00400001
004100 FILE-CONTROL.                                                    00410001
004200     SELECT SYSOUT                                                00420001
004300     ASSIGN TO UT-S-SYSOUT                                        00430001
004400       ORGANIZATION IS SEQUENTIAL.                                00440001
004500                                                                  00450001
004600     SELECT TRMTDATA                                              00460001
004700     ASSIGN TO UT-S-TRMTDATA                                      00470001
004800       ACCESS MODE IS SEQUENTIAL                                  00480001
004900       FILE STATUS IS OFCODE.                                     00490001
005000                                                                  00500001
005100     SELECT TRMTEDIT                                              00510001
005200     ASSIGN TO UT-S-TRMTEDIT                                      00520001
005300       ACCESS MODE IS SEQUENTIAL                                  00530001
005400       FILE STATUS IS OFCODE.                                     00540001
005500                                                                  00550001
005600     SELECT TRMTERR                                               00560001
005700     ASSIGN TO UT-S-TRMTERR                                       00570001
005800       ACCESS MODE IS SEQUENTIAL                                  00580001
005900       FILE STATUS IS OFCODE.                                     00590001

           SELECT CUSTOMER-FILE-OUT ASSIGN TO CUSTOUT                   00004200
               FILE STATUS  IS  WS-CUSTOUT-STATUS.
                                                                        00600001
006100     SELECT PATMSTR                                               00610001
006200            ASSIGN       to PATMSTR                               00620001
006300            ORGANIZATION is INDEXED                               00630001
006400            ACCESS MODE  is RANDOM                                00640001
006500            RECORD KEY   is PATIENT-KEY                           00650001
006600            FILE STATUS  is PATMSTR-STATUS.                       00660001
006700                                                                  00670001
006800 DATA DIVISION.                                                   00680001
006900 FILE SECTION.                                                    00690001
007000 FD  SYSOUT                                                       00700001
007100     RECORDING MODE IS F                                          00710001
007200     LABEL RECORDS ARE STANDARD                                   00720001
007300     RECORD CONTAINS 130 CHARACTERS                               00730001
007400     BLOCK CONTAINS 0 RECORDS                                     00740001
007500     DATA RECORD IS SYSOUT-Rec.                                   00750001
007600 01  SYSOUT-REC  PIC X(130).                                      00760001
007700                                                                  00770001
007800****** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM    00780001
007900****** IT CONSISTS OF ALL PATIENT TREATMENTS ENTERED              00790001
008000****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS     00800001
008100****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND    00810001
008200 FD  TRMTDATA                                                     00820001
008300     RECORDING MODE IS F                                          00830001
008400     LABEL RECORDS ARE STANDARD                                   00840001
008500     RECORD CONTAINS 1101 CHARACTERS                              00850001
008600     BLOCK CONTAINS 0 RECORDS                                     00860001
008700     DATA RECORD IS INPATIENT-TREATMENT-REC-DATA.                 00870001
008800 01  INPATIENT-TREATMENT-REC-DATA PIC X(1101).                    00880001
008900                                                                  00890001
009000****** THIS FILE IS WRITTEN FOR ALL TREATMENT RECORDS THAT PASS   00900001
009100****** THE PROGRAM'S EDIT ROUTINES                                00910001
009200****** THE TRAILER RECORD SHOULD ONLY CARRY THE NUMBER OF         00920001
009300****** RECORDS IN THE FILE ON TO THE NEXT JOB STEP                00930001
009400 FD  TRMTEDIT                                                     00940001
009500     RECORDING MODE IS F                                          00950001
009600     LABEL RECORDS ARE STANDARD                                   00960001
009700     BLOCK CONTAINS 0 RECORDS                                     00970001
009800     RECORD CONTAINS 1101 CHARACTERS                              00980001
009900     DATA RECORD IS INPATIENT-TREATMENT-REC-EDIT.                 00990001
010000 01  INPATIENT-TREATMENT-REC-EDIT PIC X(1101).                    01000001
010100                                                                  01010001
010200 FD  TRMTERR                                                      01020001
010300     RECORDING MODE IS F                                          01030001
010400     LABEL RECORDS ARE STANDARD                                   01040001
010500     RECORD CONTAINS 1141 CHARACTERS                              01050001
010600     BLOCK CONTAINS 0 RECORDS                                     01060001
010700     DATA RECORD IS INPATIENT-TREATMENT-REC-ERR.                  01070001
010800 01  INPATIENT-TREATMENT-REC-ERR.                                 01080001
010900     05  ERR-MSG                     PIC X(40).                   01090001
011000     05  REST-OF-REC                 PIC X(1101).                 01100001

       FD  CUSTOMER-FILE-OUT                                            00006100
           RECORDING MODE IS V                                          00006200
           BLOCK CONTAINS 0 RECORDS                                     00006300
           RECORD IS VARYING FROM 20 TO 596 CHARACTERS.                 00006400
           COPY CUSTCOPY REPLACING ==:TAG:== BY ==CSTOUT==.

011100                                                                  01110001
011200 FD  PATMSTR                                                      01120001
011300     RECORD CONTAINS 2964 CHARACTERS                              01130001
011400     DATA RECORD IS PATIENT-MASTER-REC.                           01140001
011500 01  PATMSTR-REC.                                                 01150001
011600     05 PATIENT-KEY      PIC X(06).                               01160001
011700     05 FILLER           PIC X(2958).                             01170001
011800                                                                  01180001
011900** QSAM FILE                                                      01190001
012000 WORKING-STORAGE SECTION.                                         01200001
012100 01  FILLER                     PIC X(42) VALUE ' '.              01210001
012100 01  FILLER                     PIC X(37) VALUE                   01210004
012200        '* WORKING STORAGE BEGINS HERE *'.                        01220001
012300                                                                  01230001
012400 01  FILLER                     PIC X(32) VALUE                   01240001
012500             '****** DUMP MSG ****************'.                  01250001
012600***************************************************************** 01260001
012700*    DUMP POINTER AREA                                            01270001
012800*        PARA POINTER- MOVE PARAGRAPH NUMBER TO THIS POINTER    * 01280001
012900*                      AS EACH PARAGRAPH IS ENTERED. DO NOT     * 01290001
013000*                      MOVE PARAGRAPH NUMBERS OF COMMON           01300001
013100*                      PARAGRAPHS (USE COMM POINTER).             01310001
013200*                                                               * 01320001
013300*        COMM POINTER - EACH COMMON PARAGRAPH SHOULD MOVE       * 01330001
013400*                       ITS PARAGRAPH NUMBER TO THIS POINTER    * 01340001
013500*                       AT IT INCEPTION.                          01350001
013600*                                                               * 01360001
013700***************************************************************** 01370001
013800 01  DUMP-LOCATOR.                                                01380001
013900     05 FILLER             PIC X(32)                              01390001
014000         VALUE '>>>>>>> WS DUMP POINTERS >>>>>>>'.                01400001
014100     05 FILLER             PIC X(16)   VALUE 'Z PARA POINTER'.    01410001
014200     05 PARA-POINTER       PIC X(8)    VALUE SPACES.              01420001
014300     05 FILLER             PIC X(8)    VALUE '       Z'.          01430001
014400     05 FILLER             PIC X(16)   VALUE 'Z COMM POINTER'.    01440001
014500     05 COMM-POINTER       PIC X(8)    VALUE SPACES.              01450001
014600     05 FILLER             PIC X(35)                              01460001
014700             VALUE '<<<<<<< WS DUMP POINTERS <<<<<<<'.            01470001
014800                                                                  01480001
014900 01  DUMP-DISPLAY.                                                01490001
015000     05 DUMP-STATUS               PIC X(3)  VALUE SPACES.         01500001
015100     05 DUMP-MESSAGE              PIC X(61) VALUE 'NO MSG'.       01510001
015200                                                                  01520001
015300 01  FILE-STATUS-CODES.                                           01530001
015400     05  PATMSTR-STATUS          PIC X(2).                        01540001
015500         88 RECORD-FOUND         VALUE "00".                      01550001
015600         88 PATMSTR-NOT-FOUND    VALUE "23".                      01560001
015700     05  OFCODE                  PIC X(2).                        01570001
015800         88 CODE-WRITE    VALUE SPACES.                           01580001
       01  CUST-REC.                                                    0000900
           05  CUST-KEY.                                                00009100
               10  CUST-ID             PIC X(5).                        00009200
               10  CUST-REC-TYPE       PIC X.                           00009300
           05  CUST-NAME               PIC X(17).                       00009400
           05  CUST-ACCT-BALANCE       PIC S9(7)V99  COMP-3.            00009500
           05  CUST-ORDERS-YTD         PIC S9(5)     COMP.              00009600
           05  CUST-ADDR               PIC X(20).                       00009700
           05  CUST-CITY               PIC X(14).                       00009800
           05  CUST-STATE              PIC X(02).                       00009900
           05  CUST-COUNTRY            PIC X(11).                       00010000
           05  CUST-MONTH              PIC S9(7)V99 COMP-3 OCCURS 12.   00010100
           05  CUST-OCCUPATION         PIC X(30).                       00010200
           05  CUST-NOTES              PIC X(120).                      00010300
           05  CUST-DATA-1             PIC X(05).                       00010400
           05  CUST-DATA-2             PIC X(40).

015900                                                                  01590001
016000 COPY TREATMNT.                                                   01600001
016100                                                                  01610001
016200 COPY TRAILER.                                                    01620001
016300                                                                  01630001
016400 01  WS-OUTPUT-REC.                                               01640001
016500     05  PATIENT-NBR-O           PIC 9(6).                        01650001
016600     05  FILLER                  PIC X(2) VALUE SPACES.           01660001
016700     05  PATIENT-NAME-O          PIC X(20).                       01670001
016800     05  PATIENT-PHONE-O         PIC X(10).                       01680001
016900     05  FILLER                  PIC X(2) VALUE SPACES.           01690001
017000     05  PATIENT-TYPE-O          PIC X(2).                        01700001
017100     05  BED-IDENTITY-O          PIC ZZZ9.                        01710001
017200     05  FILLER                  PIC X(2) VALUE SPACES.           01720001
017300     05  CURR-DATE-O             PIC X(6).                        01730001
017400     05  FILLER                  PIC X(2) VALUE SPACES.           01740001
017500     05  PATIENT-AMT-PER-DAY-O   PIC $$,$$9.99.                   01750001
017600     05  FILLER                  PIC X(2) VALUE SPACES.           01760001
017700     05  INS-COVERAGE-PERC-O     PIC 999.                         01770001
017800     05  FILLER                  PIC X(2) VALUE SPACES.           01780001
017900     05  INS-TYPE-O              PIC X(4).                        01790001
018000     05  HOSPITAL-STAY-LTH-O     PIC 999.                         01800001
018100     05  FILLER                  PIC X(7) VALUE SPACES.           01810001
018200                                                                  01820001
018300 COPY PATMSTR.                                                    01830001
018400** VSAM FILE                                                      01840001
018500                                                                  01850001
018600 01  WS-SYSOUT-REC.                                               01860001
018700     05  MSG                     PIC X(80).                       01870001
018800                                                                  01880001
018900 77  WS-DATE                     PIC 9(6).                        01890001

        01  WS-FIELDS.                                                   0001070
           05  WS-CUSTFILE-STATUS      PIC X(2)  VALUE SPACES.          00010800
           05  WS-CUSTOUT-STATUS       PIC X(2)  VALUE SPACES.          00010900
           05  WS-TRANFILE-STATUS      PIC X(2)  VALUE SPACES.          00011000
           05  WS-REPORT-STATUS        PIC X(2)  VALUE SPACES.          00011100
           05  WS-TRAN-EOF             PIC X     VALUE SPACES.          00011200
           05  WS-TRAN-OK              PIC X     VALUE 'N'.             00011300
           05  WS-CUST-FILE-OK         PIC X     VALUE 'N'.             00011400
           05  WS-CUST-FILE-EOF        PIC X     VALUE 'N'.             00011500
           05  WS-TRAN-MSG             PIC X(50) VALUE SPACES.          00011600
           05  WS-PREV-TRAN-KEY        PIC X(10) VALUE LOW-VALUES.      00011700
           05  INCR-CUST-ID            PIC 9(5)  VALUE 0.               00011800
           05  START-CUST-ID           PIC 9(5)  VALUE 0.               00011900
           05  MAX-CUST-ID             PIC 9(5)  VALUE 0.               00012000

       01  WORK-VARIABLES.                                              00012200
           05  I                     PIC S9(9)   COMP-3  VALUE +0.      00012300
           05  WORK-NUM              PIC s9(8)   COMP.

019000                                                                  01900001
019100 01  COUNTERS-AND-ACCUMULATORS.                                   01910001
019200     05 RECORDS-WRITTEN          PIC 9(7) COMP.                   01920001
019200     05 RECORDS-WRITTEN-1        PIC 9(7) COMP.                   01920002
019300     05 RECORDS-IN-ERROR         PIC 9(7) COMP.                   01930001
019400     05 RECORDS-READ             PIC 9(7) COMP.                   01940001
019500     05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.             01950001
019600     05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.             01960001
019700     05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.             01970001
019800                                                                  01980001
019900 01  MISC-WS-FLDS.                                                01990001
      *    05  WS-CUSTOUT-STATUS       PIC X(2)  VALUE SPACES.
020000     05 STR-LTH                  PIC 9(04) VALUE 0.               02000001
020100     05 RETURN-CD                PIC S9(04) VALUE 0.              02010001
020200     05 ROW-SUB                  PIC 9(02).                       02020001
020200     05 LITERAL-TEST             PIC X(08) VALUE 'ABCDEFG'.       02021004
020300     05 ERROR-MSG-WS.                                             02030001
020400        10  ERROR-MSG-FIRST      PIC X(20).                       02040001
020500        10  ERROR-MSG-REST       PIC X(60).                       02050001
020600                                                                  02060001
020700 01  FLAGS-AND-SWITCHES.                                          02070001
020800     05 MORE-DATA-SW             PIC X(01) VALUE "Y".             02080001
020900         88 NO-MORE-DATA VALUE "N".                               02090001
021000     05 ERROR-FOUND-SW           PIC X(01) VALUE "N".             02100001
021100         88 RECORD-ERROR-FOUND VALUE "Y".                         02110001
021200         88 VALID-RECORD  VALUE "N".                              02120001
021300     05  MORE-TABLE-ROWS         PIC X(01) VALUE "Y".             02130001
021400         88 NO-MORE-TABLE-ROWS VALUE "N".                         02140001
021500                                                                  02150001
021600* COPY ABENDREC.                                                  02160001
021700** QSAM FILE                                                      02170001
021800 COPY ABENDREC.                                                   02180001
022000 COPY PATINS.                                                     02200001
022100 COPY PATPERSN.                                                   02210001
022200* COPY DIAGCODE.                                                  02220001
022300******************************************************************02230001
022400***** DB2 TABLE DCLGENS                                           02240001
022500                                                                  02250001
022600     EXEC SQL INCLUDE WARDDATA END-EXEC.                          02260001
022700     EXEC SQL INCLUDE MEDICATN END-EXEC.                          02270001
022800     EXEC SQL INCLUDE HOSPBED END-EXEC.                           02280001
022900     EXEC SQL INCLUDE DIAGCODE END-EXEC.                          02290001
023000                                                                  02300001
023100 COPY SQLCA.                                                      02310001
023200                                                                  02320001
023300 PROCEDURE DIVISION.                                              02330001
023400     PERFORM 000-HOUSEKEEPING THRU 000-EXIT.                      02340001
023500     PERFORM 100-MAINLINE THRU 100-EXIT                           02350001
023600             UNTIL NO-MORE-DATA OR                                02360001
023700******* Balancing logic put in by TGD 02/12/92                    02370001
023800             TRAILER-REC.                                         02380001
023900     PERFORM 999-CLEANUP THRU 999-EXIT.                           02390001
024000     Compute RETURN-CODE = 0.                                     02400001
           MOVE 'ABCDEF' TO LITERAL-TEST.                               02401004
024100     GOBACK.                                                      02410001
024200                                                                  02420001
024300 000-HOUSEKEEPING.                                                02430001
024400     MOVE "000-HOUSEKEEPING" TO PARA-NAME.                        02440001
024500     DISPLAY "HOUSEKEEPING".                                      02450001
024600*  Code your statement here to OPEN files                         02460001
024700     ACCEPT  WS-DATE FROM DATE.                                   02470001
024800     INITIALIZE COUNTERS-AND-ACCUMULATORS.                        02480001
024900     PERFORM 800-OPEN-FILES THRU 800-EXIT.                        02490001
025000     PERFORM 900-READ-TRMTDATA THRU 900-EXIT.                     02500001
025100     IF NO-MORE-DATA                                              02510001
025200         MOVE "EMPTY INPUT FILE" TO ABEND-REASON                  02520001
025300         GO TO 1000-ABEND-RTN.                                    02530001
025400 000-EXIT.                                                        02540001
025500     EXIT.                                                        02550001
025600                                                                  02560001
025700 100-MAINLINE.                                                    02570001
025800     MOVE "100-MAINLINE" TO PARA-NAME.                            02580001
025900*     DISPLAY "100-MAINLINE".                                     02590001
026000*  Validate patient type and insurance coverage                   02600001
026100     PERFORM 300-FIELD-EDITS THRU 300-EXIT.                       02610001
026200                                                                  02620001
026300     IF RECORD-ERROR-FOUND                                        02630001
026400         ADD +1 TO RECORDS-IN-ERROR                               02640001
026500         PERFORM 710-WRITE-TRMTERR THRU 710-EXIT                  02650001
026600     ELSE                                                         02660001
026700         PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.                02670001
026800     PERFORM 900-READ-TRMTDATA THRU 900-EXIT.                     02680001
026900 100-EXIT.                                                        02690001
027000     EXIT.                                                        02700001
027100                                                                  02710001
027200 300-FIELD-EDITS.                                                 02720001
027300     MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.            02730001
027400     MOVE "300-FIELD-EDITS" TO PARA-NAME.                         02740001
027500******** non-numeric fields                                       02750001
027600     IF NOT VALID-BILLABLE-TYPES IN BILLABLE-TREATMENT-IND        02760001
027700        MOVE "*** INVALID BILLABLE TYPE" TO                       02770001
027800        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    02780001
027900        MOVE "Y" TO ERROR-FOUND-SW                                02790001
028000        GO TO 300-EXIT.                                           02800001
028100                                                                  02810001
028200     IF NOT VALID-TRTMNT-MODES IN TREATMENT-MODE                  02820001
028300        MOVE "*** INVALID TREATMENT MODE" TO                      02830001
028400        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    02840001
028500        MOVE "Y" TO ERROR-FOUND-SW                                02850001
028600        GO TO 300-EXIT.                                           02860001
028700                                                                  02870001
028800     IF PATIENT-ID IN INPATIENT-TREATMENT-REC NOT NUMERIC         02880001
028900        MOVE "*** NON-NUMERIC PATIENT-ID" TO                      02890001
029000        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    02900001
029100        MOVE "Y" TO ERROR-FOUND-SW                                02910001
029200        GO TO 300-EXIT.                                           02920001
029300                                                                  02930001
029400     IF PATIENT-ID IN INPATIENT-TREATMENT-REC = ZERO              02940001
029500        MOVE "*** INVALID (000000) PATIENT-ID" TO                 02950001
029600        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    02960001
029700        MOVE "Y" TO ERROR-FOUND-SW                                02970001
029800        GO TO 300-EXIT.                                           02980001
029900                                                                  02990001
030000     IF BED-IDENTITY IN INPATIENT-TREATMENT-REC NOT NUMERIC       03000001
030100        MOVE "*** NON-NUMERIC BED-IDENTITY" TO                    03010001
030200        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03020001
030300        MOVE "Y" TO ERROR-FOUND-SW                                03030001
030400        GO TO 300-EXIT.                                           03040001
030500                                                                  03050001
030600     IF MEDICATION-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC    03060001
030700        MOVE "*** NON-NUMERIC MEDICATION-COST" TO                 03070001
030800        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03080001
030900        MOVE "Y" TO ERROR-FOUND-SW                                03090001
031000        GO TO 300-EXIT.                                           03100001
031100                                                                  03110001
031200     IF PHARMACY-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC      03120001
031300        MOVE "*** NON-NUMERIC PHARMACY COSTS" TO                  03130001
031400        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03140001
031500        MOVE "Y" TO ERROR-FOUND-SW                                03150001
031600        GO TO 300-EXIT.                                           03160001
031700                                                                  03170001
031800     IF ANCILLARY-CHARGE IN INPATIENT-TREATMENT-REC NOT NUMERIC   03180001
031900        MOVE "*** NON-NUMERIC ANCILLARY-CHARGES" TO               03190001
032000        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03200001
032100        MOVE "Y" TO ERROR-FOUND-SW                                03210001
032200        GO TO 300-EXIT.                                           03220001
032300                                                                  03230001
032400     IF ATTENDING-PHYS-ID = SPACES                                03240001
032500        MOVE "*** BLANK ATTENDING PHYSICIAN-ID" TO                03250001
032600        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03260001
032700        MOVE "Y" TO ERROR-FOUND-SW                                03270001
032800        GO TO 300-EXIT.                                           03280001
032900                                                                  03290001
033000     IF GROUP-NBR in INS-COMPANY-PRIMARY = SPACES                 03300001
033100        MOVE "*** BLANK INS GROUP NBR" TO                         03310001
033200        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03320001
033300        MOVE "Y" TO ERROR-FOUND-SW                                03330001
033400        GO TO 300-EXIT.                                           03340001
033500                                                                  03350001
033600     IF PRESCRIBING-PHYS-ID = SPACES                              03360001
033700        MOVE "*** BLANK PRESCRIBING PHYSICIAN-ID" TO              03370001
033800        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03380001
033900        MOVE "Y" TO ERROR-FOUND-SW                                03390001
034000        GO TO 300-EXIT.                                           03400001
034100                                                                  03410001
034200     CALL 'DTEVAL' USING TREATMENT-DATE, RETURN-CD.               03420001
034300     IF RETURN-CD < 0                                             03430001
034400        MOVE "*** BAD DATE PORTION OF DATE-TIME" TO               03440001
034500        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03450001
034600        MOVE "Y" TO ERROR-FOUND-SW                                03460001
034700        GO TO 300-EXIT.                                           03470001
034800                                                                  03480001
034900     MOVE "Y" TO MORE-TABLE-ROWS.                                 03490001
035000     PERFORM 350-CHECK-LAB-TABLE THRU 350-EXIT VARYING ROW-SUB    03500001
035100          FROM 1 BY 1 UNTIL NO-MORE-TABLE-ROWS OR ROW-SUB = 12.   03510001
035200                                                                  03520001
035300     IF VALID-RECORD                                              03530001
035400         PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-EXIT.           03540001
035500                                                                  03550001
035600****** VERIFY TABLE (JUST TYPES AND LAB-TEST-ID)                  03560001
035700                                                                  03570001
035800 300-EXIT.                                                        03580001
035900     EXIT.                                                        03590001
036000                                                                  03600001
036100 350-CHECK-LAB-TABLE.                                             03610001
036200     IF LAB-TEST-ID(ROW-SUB) = SPACES                             03620001
036300        MOVE "N" TO MORE-TABLE-ROWS                               03630001
036400        GO TO 350-EXIT.                                           03640001
036500                                                                  03650001
036600     IF NOT VALID-CATEGORY(ROW-SUB)                               03660001
036700        MOVE "*** INVALID LAB-TEST CATEGORY" TO                   03670001
036800        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03680001
036900        MOVE "Y" TO ERROR-FOUND-SW                                03690001
037000        GO TO 350-EXIT.                                           03700001
037100                                                                  03710001
037200 350-EXIT.                                                        03720001
037300     EXIT.                                                        03730001
037400                                                                  03740001
037500                                                                  03750001
037600 400-NUMERIC-RANGE-EDITS.                                         03760001
037700     MOVE "400-NUMERIC-RANGE-EDITS" TO PARA-NAME.                 03770001
037800******** Call to VSAM file to read record                         03780001
037900     IF  (MEDICATION-COST > 99000                                 03790001
038000     OR  MEDICATION-COST < 1.01)                                  03800001
038100         MOVE "*** INVALID MEDICATION COST" TO                    03810001
038200         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   03820001
038300         MOVE "Y" TO ERROR-FOUND-SW                               03830001
038400         GO TO 400-EXIT.                                          03840001
038500                                                                  03850001
038600                                                                  03860001
038700     IF  (PHARMACY-COST IN INPATIENT-TREATMENT-REC > 990          03870001
038800     OR  PHARMACY-COST IN INPATIENT-TREATMENT-REC < .99)          03880001
038900         MOVE "*** INVALID PHARMACY COSTS" TO                     03890001
039000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   03900001
039100         MOVE "Y" TO ERROR-FOUND-SW                               03910001
039200         GO TO 400-EXIT.                                          03920001
039300                                                                  03930001
039400                                                                  03940001
039500     IF  (ANCILLARY-CHARGE > 900                                  03950001
039600     OR  ANCILLARY-CHARGE < 1.01)                                 03960001
039700         MOVE "*** INVALID ANCILLARY CHARGES" TO                  03970001
039800         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   03980001
039900         MOVE "Y" TO ERROR-FOUND-SW                               03990001
040000         GO TO 400-EXIT.                                          04000001
040100                                                                  04010001
040200                                                                  04020001
040300*    IF  (SSN IN RESPONSIBLE-PARTY > "999999999"                  04030001
040400*    OR  SSN IN RESPONSIBLE-PARTY < "0000000001")                 04040001
040500*        MOVE "*** INVALID SOCIAL SECURITY #" TO                  04050001
040600*        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04060001
040700*        MOVE "Y" TO ERROR-FOUND-SW                               04070001
040800*        GO TO 400-EXIT.                                          04080001
040900                                                                  04090001
041000                                                                  04100001
042500*                                                                 04250001
042600*    IF  (EXP-MONTH > 12                                          04260001
042700*    OR  EXP-MONTH < 1)                                           04270001
042800*        MOVE "*** INVALID CREDIT-CARD EXP. DATE" TO              04280001
042900*        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04290001
043000*        MOVE "Y" TO ERROR-FOUND-SW                               04300001
043100*        GO TO 400-EXIT.                                          04310001
043200*                                                                 04320001
043300     IF VALID-RECORD                                              04330001
043400         PERFORM 450-CROSS-FIELD-EDITS THRU 450-EXIT.             04340001
043500                                                                  04350001
043600     IF VALID-RECORD                                              04360001
043700         PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.              04370001
043800                                                                  04380001
043900 400-EXIT.                                                        04390001
044000     EXIT.                                                        04400001
044100                                                                  04410001
044200 450-CROSS-FIELD-EDITS.                                           04420001
044300     MOVE "450-CROSS-FIELD-EDITS" TO PARA-NAME.                   04430001
044400******** Specific requirements for certain procedures             04440001
044500     IF  MRI OR CAT OR CHEMO-THERAPY OR RADIATION-THERAPY         04450001
044600         OR SURGERY OR LAB-TESTS                                  04460001
044700         IF MEDICATION-COST = ZERO OR                             04470001
044800            ANCILLARY-CHARGE = ZERO                               04480001
044900         MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO          04490001
045000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04500001
045100         MOVE "Y" TO ERROR-FOUND-SW                               04510001
045200         GO TO 450-EXIT.                                          04520001
045300                                                                  04530001
045400     IF  ORAL-ADMIN OR INTRAVENOUS-ADMIN OR INJECTION             04540001
045500         IF PHARMACY-COST IN INPATIENT-TREATMENT-REC = ZERO OR    04550001
045600            ANCILLARY-CHARGE = ZERO                               04560001
045700         MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO          04570001
045800         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04580001
045900         MOVE "Y" TO ERROR-FOUND-SW                               04590001
046000         GO TO 450-EXIT.                                          04600001
046100                                                                  04610001
046200     IF  NOT OTHER-TREATMENT                                      04620001
046300         IF TREATMENT-NURSE-ID = SPACES OR                        04630001
046400            SUPERVISOR-NURSE-ID = SPACES                          04640001
046500         MOVE "*** INVALID NURSING ENTRIES" TO                    04650001
046600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04660001
046700         MOVE "Y" TO ERROR-FOUND-SW                               04670001
046800         GO TO 450-EXIT.                                          04680001
046900                                                                  04690001
047000     IF  NOT (OTHER-TREATMENT AND LAB-TESTS)                      04700001
047100         IF TREATMENT-COMMENTS = SPACES                           04710001
047200         MOVE "*** INVALID TREATMENT COMMENTS" TO                 04720001
047300         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04730001
047400         MOVE "Y" TO ERROR-FOUND-SW                               04740001
047500         GO TO 450-EXIT.                                          04750001
047600                                                                  04760001
047700     IF  CHEMO-THERAPY OR RADIATION-THERAPY OR SURGERY            04770001
047800        MOVE +0 TO STR-LTH                                        04780001
047900        CALL 'STRLTH' USING TREATMENT-COMMENTS, STR-LTH           04790001
048000        IF STR-LTH < 25                                           04800001
048100         MOVE "*** INVALID TREATMENT COMMENT LENGTH" TO           04810001
048200         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04820001
048300         MOVE "Y" TO ERROR-FOUND-SW                               04830001
048400         GO TO 450-EXIT.                                          04840001
048500                                                                  04850001
048600     IF VALID-RECORD                                              04860001
048700         PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.              04870001
048800                                                                  04880001
048900 450-EXIT.                                                        04890001
049000     EXIT.                                                        04900001
049100                                                                  04910001
049200 500-CROSS-FILE-EDITS.                                            04920001
049300     MOVE "500-CROSS-FILE-EDITS" TO PARA-NAME.                    04930001
049400******** Call to VSAM file to read record                         04940001
049500     MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO                04950001
049600            PATIENT-KEY.                                          04960001
049700     READ PATMSTR INTO PATIENT-MASTER-REC.                        04970001
049800     IF  NOT RECORD-FOUND                                         04980001
049900         MOVE "*** PATIENT NOT-FOUND ON MASTER FILE" TO           04990001
050000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05000001
050100         MOVE "Y" TO ERROR-FOUND-SW                               05010001
050200         GO TO 500-EXIT.                                          05020001
050300                                                                  05030001
041100     IF  (HOSPITAL-STAY-LTH > 365                                 04110001
041200     OR  HOSPITAL-STAY-LTH < 1)                                   04120001
041300         MOVE "*** INVALID HOSPITAL STAY LTH" TO                  04130001
041400         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04140001
041500         MOVE "Y" TO ERROR-FOUND-SW                               04150001
041600         GO TO 400-EXIT.                                          04160001
041700                                                                  04170001
041800     IF  (EQUIPMENT-CHARGES(ROW-SUB) > 999.00                     04180001
041900     OR  EQUIPMENT-CHARGES(ROW-SUB) < 99.99)                      04190001
042000         MOVE "*** INVALID EQUIPMENT CHARGES" TO                  04200001
042100         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   04210001
042200         MOVE "Y" TO ERROR-FOUND-SW                               04220001
042300         GO TO 400-EXIT.                                          04230001
042400                                                                  04240001
050400     IF VALID-RECORD                                              05040001
050500        PERFORM 600-DB2-TABLE-EDITS THRU 600-EXIT.                05050001
050600                                                                  05060001
050700 500-EXIT.                                                        05070001
050800     EXIT.                                                        05080001
050900                                                                  05090001
051000 600-DB2-TABLE-EDITS.                                             05100001
051100     MOVE "600-DB2-TABLE-EDITS" TO PARA-NAME.                     05110001
051200******** EXEC SQL to get info from DB2                            05120001
051300     MOVE ZERO TO  ANCILLARY-CHARGE.                              05130001
051400     MOVE DIAGNOSTIC-CODE-PRIMARY IN PATIENT-MASTER-REC TO        05140001
051500          DIAG-CODE IN DCLDIAG-CODES.                             05150001
051600                                                                  05160001
051700****** CHECK FOR VALID DIAGNOSTIC CODE                            05170001
051800     EXEC SQL                                                     05180001
051900        SELECT DIAG_CODE INTO :DIAG-CODE                          05190001
052000        FROM DDS0001.DIAG_CODES                                   05200001
052100        WHERE DIAG_CODE = :DIAG-CODE                              05210001
052200     END-EXEC.                                                    05220001
052300                                                                  05230001
052400     IF SQLCODE = -811 OR 0                                       05240001
052500         NEXT SENTENCE                                            05250001
052600     ELSE                                                         05260001
052700     IF SQLCODE = +100                                            05270001
052800         MOVE "*** DIAGNOSTIC CODE NOT-FOUND IN DIAG_CODES" TO    05280001
052900         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05290001
053000         MOVE "Y" TO ERROR-FOUND-SW                               05300001
053100         move SQLCODE to  PATIENT-ID IN INPATIENT-TREATMENT-REC   05310001
053200         MOVE DIAG-CODE IN DCLDIAG-CODES                          05320001
053300                            TO PRIMARY-DIAGNOSTIC-CODE            05330001
053400         move sqlcode to  EXPECTED-VAL                            05340001
053500         move PATIENT-ID IN INPATIENT-TREATMENT-REC               05350001
053600                         to ACTUAL-VAL                            05360001
053700         WRITE SYSOUT-REC FROM ABEND-REC                          05370001
053800         GO TO 600-EXIT                                           05380001
053900     ELSE                                                         05390001
054000     IF SQLCODE < 0                                               05400001
054100         MOVE "***  FATAL DB2 ERROR" TO                           05410001
054200         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05420001
054300         MOVE "Y" TO ERROR-FOUND-SW                               05430001
054400         move sqlcode to  PATIENT-ID IN INPATIENT-TREATMENT-REC   05440001
054500         MOVE DIAG-CODE IN DCLDIAG-CODES                          05450001
054600                            TO PRIMARY-DIAGNOSTIC-CODE            05460001
054700         move sqlcode to  EXPECTED-VAL                            05470001
054800         move PATIENT-ID IN INPATIENT-TREATMENT-REC               05480001
054900                         to ACTUAL-VAL                            05490001
055000         WRITE SYSOUT-REC FROM ABEND-REC                          05500001
055100         GO TO 1000-DB2-ERROR-RTN.                                05510001
055200                                                                  05520001
055300****** CHECK FOR VALID BED IDENTITY                               05530001
055400     MOVE BED-IDENTITY TO BED-ID.                                 05540001
055500     EXEC SQL                                                     05550001
055600        SELECT BED_ID INTO :BED-ID                                05560001
055700        FROM DDS0001.HOSP_BED                                     05570001
055800        WHERE BED_ID = :BED-ID                                    05580001
055900     END-EXEC.                                                    05590001
056000                                                                  05600001
056100     IF SQLCODE = -811 OR 0                                       05610001
056200         NEXT SENTENCE                                            05620001
056300     ELSE                                                         05630001
056400     IF SQLCODE = +100                                            05640001
056500         MOVE "*** BED IDENT NOT-FOUND IN HOSP_BED" TO            05650001
056600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05660001
056700         MOVE "Y" TO ERROR-FOUND-SW                               05670001
056800         move sqlcode to  EXPECTED-VAL                            05680001
056900         move PATIENT-ID IN INPATIENT-TREATMENT-REC               05690001
057000                         to ACTUAL-VAL                            05700001
057100         WRITE SYSOUT-REC FROM ABEND-REC                          05710001
057200         GO TO 600-EXIT                                           05720001
057300     ELSE                                                         05730001
057400     IF SQLCODE < 0                                               05740001
057500         MOVE "***  FATAL DB2 ERROR" TO                           05750001
057600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05760001
057700         MOVE "Y" TO ERROR-FOUND-SW                               05770001
057800         move sqlcode to  EXPECTED-VAL                            05780001
057900         move PATIENT-ID IN INPATIENT-TREATMENT-REC               05790001
058000                         to ACTUAL-VAL                            05800001
058100         WRITE SYSOUT-REC FROM ABEND-REC                          05810001
058200         GO TO 1000-DB2-ERROR-RTN.                                05820001
058300                                                                  05830001
058400****** CHECK FOR VALID PHYSICIAN-ID                               05840001
058500     MOVE ATTENDING-PHYS-ID TO PRIMARY-PHYSICIAN-ID.              05850001
058600     EXEC SQL                                                     05860001
058700        SELECT PRIMARY_PHYSICIAN_ID INTO :PRIMARY-PHYSICIAN-ID    05870001
058800        FROM DDS0001.WARD_DATA                                    05880001
058900        WHERE PRIMARY_PHYSICIAN_ID = :PRIMARY-PHYSICIAN-ID        05890001
059000     END-EXEC.                                                    05900001
059100                                                                  05910001
059200     IF SQLCODE = -811 OR 0                                       05920001
059300         NEXT SENTENCE                                            05930001
059400     ELSE                                                         05940001
059500     IF SQLCODE = +100                                            05950001
059600         MOVE "*** ATTENDING PHYSICIAN NOT FOUND IN TABLE" TO     05960001
059700         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   05970001
059800         MOVE "Y" TO ERROR-FOUND-SW                               05980001
059900         move sqlcode to  EXPECTED-VAL                            05990001
060000         move PATIENT-ID IN INPATIENT-TREATMENT-REC               06000001
060100                         to ACTUAL-VAL                            06010001
060200         WRITE SYSOUT-REC FROM ABEND-REC                          06020001
060300         GO TO 600-EXIT                                           06030001
060400     ELSE                                                         06040001
060500         MOVE "***  FATAL DB2 ERROR" TO                           06050001
060600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06060001
060700         MOVE "Y" TO ERROR-FOUND-SW                               06070001
060800         move sqlcode to  EXPECTED-VAL                            06080001
060900         move PATIENT-ID IN INPATIENT-TREATMENT-REC               06090001
061000                         to ACTUAL-VAL                            06100001
061100         WRITE SYSOUT-REC FROM ABEND-REC                          06110001
061200         GO TO 1000-DB2-ERROR-RTN.                                06120001
061300                                                                  06130001
061400****** CHECK FOR VALID MEDICATION-ID                              06140001
061500     MOVE MEDICATION-ID IN INPATIENT-TREATMENT-REC TO             06150001
061600            MEDICATION-ID IN DCLMEDICATION.                       06160001
061700                                                                  06170001
061800     EXEC SQL                                                     06180001
061900        SELECT MEDICATION_ID                                      06190001
062000                       INTO :DCLMEDICATION.MEDICATION-ID          06200001
062100        FROM DDS0001.MEDICATION                                   06210001
062200        WHERE MEDICATION_ID = :DCLMEDICATION.MEDICATION-ID        06220001
062300     END-EXEC.                                                    06230001
062400                                                                  06240001
062500     IF SQLCODE = -811 OR 0                                       06250001
062600         NEXT SENTENCE                                            06260001
062700     ELSE                                                         06270001
062800     IF SQLCODE = +100                                            06280001
062900         MOVE "*** MEDICATION-ID NOT FOUND IN TABLE" TO           06290001
063000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06300001
063100         MOVE "Y" TO ERROR-FOUND-SW                               06310001
063200         move sqlcode to  EXPECTED-VAL                            06320001
063300         move PATIENT-ID IN INPATIENT-TREATMENT-REC               06330001
063400                         to ACTUAL-VAL                            06340001
063500         WRITE SYSOUT-REC FROM ABEND-REC                          06350001
063600         GO TO 600-EXIT                                           06360001
063700     ELSE                                                         06370001
063800     IF SQLCODE < 0                                               06380001
063900         MOVE "***  FATAL DB2 ERROR" TO                           06390001
064000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06400001
064100         MOVE "Y" TO ERROR-FOUND-SW                               06410001
064200         move sqlcode to  EXPECTED-VAL                            06420001
064300         move PATIENT-ID IN INPATIENT-TREATMENT-REC               06430001
064400                         to ACTUAL-VAL                            06440001
064500         WRITE SYSOUT-REC FROM ABEND-REC                          06450001
064600         GO TO 1000-DB2-ERROR-RTN.                                06460001
064700                                                                  06470001
064800****** CHECK FOR VALID SUPERVISOR NURSE-ID                        06480001
064900     MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.              06490001
065000     EXEC SQL                                                     06500001
065100        SELECT SUPERVISE_NURSE_ID                                 06510001
065200                       INTO :SUPERVISE-NURSE-ID                   06520001
065300        FROM DDS0001.WARD_DATA                                    06530001
065400        WHERE SUPERVISE_NURSE_ID = :SUPERVISE-NURSE-ID            06540001
065500     END-EXEC.                                                    06550001
065600                                                                  06560001
065700     IF SQLCODE = -811 OR 0                                       06570001
065800         NEXT SENTENCE                                            06580001
065900     ELSE                                                         06590001
066000     IF SQLCODE = +100                                            06600001
066100         MOVE "*** SUPERVISOR NURSE NOT FOUND" TO                 06610001
066200         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06620001
066300         MOVE "Y" TO ERROR-FOUND-SW                               06630001
066400         move sqlcode to  EXPECTED-VAL                            06640001
066500         move PATIENT-ID IN INPATIENT-TREATMENT-REC               06650001
066600                         to ACTUAL-VAL                            06660001
066700         WRITE SYSOUT-REC FROM ABEND-REC                          06670001
066800         GO TO 600-EXIT                                           06680001
066900     ELSE                                                         06690001
067000     IF SQLCODE < 0                                               06700001
067100         MOVE "*** FATAL DB2 ERROR" TO                            06710001
067200         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06720001
067300         MOVE "Y" TO ERROR-FOUND-SW                               06730001
067400         move sqlcode to  EXPECTED-VAL                            06740001
067500         move PATIENT-ID IN INPATIENT-TREATMENT-REC               06750001
067600                         to ACTUAL-VAL                            06760001
067700         WRITE SYSOUT-REC FROM ABEND-REC                          06770001
067800         GO TO 1000-DB2-ERROR-RTN.                                06780001
067900****** CHECK FOR VALID SUPERVISOR NURSE-ID                        06790001
068000                                                                  06800001
068100     MOVE IN-PHARMACY-CHARGES  TO PHARMACY-COST IN DCLMEDICATION. 06810001
068200     EXEC SQL                                                     06820001
068300        SELECT MEDICATION_ID                                      06830001
068400                       INTO :DCLMEDICATION.MEDICATION-ID          06840001
068500        FROM DDS0001.MEDICATION                                   06850001
068600        WHERE PHARMACY_COST >= :DCLMEDICATION.PHARMACY-COST       06860001
068700     END-EXEC.                                                    06870001
068800                                                                  06880001
068900     IF SQLCODE = -811 OR 0                                       06890001
069000         NEXT SENTENCE                                            06900001
069100     ELSE                                                         06910001
069200     IF SQLCODE = +100                                            06920001
069300         MOVE "*** PHARMACY COST" TO                              06930001
069400         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06940001
069500         MOVE "Y" TO ERROR-FOUND-SW                               06950001
069600         move sqlcode to  EXPECTED-VAL                            06960001
069700         move PATIENT-ID IN INPATIENT-TREATMENT-REC               06970001
069800                         to ACTUAL-VAL                            06980001
069900         WRITE SYSOUT-REC FROM ABEND-REC                          06990001
070000         GO TO 600-EXIT                                           07000001
070100     ELSE                                                         07010001
070200     IF SQLCODE < 0                                               07020001
070300         MOVE "*** FATAL DB2 ERROR" TO                            07030001
070400         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   07040001
070500         MOVE "Y" TO ERROR-FOUND-SW                               07050001
070600         move sqlcode to  EXPECTED-VAL                            07060001
070700         move PATIENT-ID IN INPATIENT-TREATMENT-REC               07070001
070800                         to ACTUAL-VAL                            07080001
070900         WRITE SYSOUT-REC FROM ABEND-REC                          07090001
071000         GO TO 1000-DB2-ERROR-RTN.                                07100001
071100 600-EXIT.                                                        07110001
071200     EXIT.                                                        07120001
071300                                                                  07130001
071400 700-WRITE-TRMTEDIT.                                              07140001
071500     MOVE "700-WRITE-TRMTEDIT" TO PARA-NAME.                      07150001
071600                                                                  07160001
071700     WRITE INPATIENT-TREATMENT-REC-EDIT                           07170001
071800         FROM INPATIENT-TREATMENT-REC.                            07180001
071900     ADD MEDICATION-COST  TO WS-MEDICATION-CHARGES.               07190001
072000     ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.                07200001
072100     ADD PHARMACY-COST IN INPATIENT-TREATMENT-REC                 07210001
072200                          TO WS-PHARMACY-CHARGES.                 07220001
072300     ADD +1 TO RECORDS-WRITTEN.                                   07230001
072400 700-EXIT.                                                        07240001
072500     EXIT.                                                        07250001
072600                                                                  07260001
072700 710-WRITE-TRMTERR.                                               07270001
072800     MOVE INPATIENT-TREATMENT-REC TO REST-OF-REC.                 07280001
072900     WRITE INPATIENT-TREATMENT-REC-ERR.                           07290001
073000     ADD +1 TO RECORDS-IN-ERROR.                                  07300001
073100 710-EXIT.                                                        07310001
073200     EXIT.                                                        07320001
073300                                                                  07330001
073400 800-OPEN-FILES.                                                  07340001
073500     MOVE "800-OPEN-FILES" TO PARA-NAME.                          07350001
073600     OPEN INPUT TRMTDATA.                                         07360001
073700     OPEN OUTPUT TRMTEDIT, SYSOUT, TRMTERR, CUSTOMER-FILE-OUT.    07370001
073800     OPEN I-O PATMSTR.                                            07380001
073900 800-EXIT.                                                        07390001
074000     EXIT.                                                        07400001
074100                                                                  07410001
074200 850-CLOSE-FILES.                                                 07420001
074300     MOVE "850-CLOSE-FILES" TO PARA-NAME.                         07430001
074400     CLOSE TRMTDATA,                                              07440001
074500           TRMTEDIT, SYSOUT, TRMTERR,                             07450001
074600           PATMSTR.                                               07460001
074700 850-EXIT.                                                        07470001
074800     EXIT.                                                        07480001
074900                                                                  07490001
075000 900-READ-TRMTDATA.                                               07500001
075100*  Code your statements here to read the input file               07510001
075200*  Remember to move "NO" to IFCODE if the input file is AT END    07520001
075300     READ TRMTDATA  INTO INPATIENT-TREATMENT-REC                  07530001
075400         AT END MOVE "N" TO MORE-DATA-SW                          07540001
075500         GO TO 900-EXIT                                           07550001
075600     END-READ                                                     07560001
075700     MOVE "N" TO ERROR-FOUND-SW.                                  07570001
075800     ADD +1 TO RECORDS-READ.                                      07580001
075900 900-EXIT.                                                        07590001
076000     EXIT.                                                        07600001
076100                                                                  07610001
       740-WRITE-CUSTOUT-FILE.                                          00041800
           IF CUST-REC-TYPE = 'A'                                       00041900
               WRITE CSTOUT-REC FROM CUST-REC                           00042000
           ELSE                                                         00042100
               MOVE CUST-REC  TO  ERROR-MSG-FIRST IN ERROR-MSG-WS       00042200
               WRITE CSTOUT-CONTACT-REC FROM CUST-REC                   00042300
           END-IF .                                                     00042400
           EVALUATE WS-CUSTOUT-STATUS                                   00042500
              WHEN '00'                                                 00042600
                  CONTINUE                                              00042700
              WHEN OTHER                                                00042800
                  MOVE 'CUSTOMER OUTPUT FILE I/O ERROR ON WRITE. RC: '  00042900
                              TO ERR-MSG                                00043000
                  MOVE WS-CUSTFILE-STATUS TO ERROR-MSG-FIRST            00043100
                  PERFORM 999-CLEANUP                                   00043200
           END-EVALUATE .
076200 999-CLEANUP.                                                     07620001
076300     MOVE "999-CLEANUP" TO PARA-NAME.                             07630001
076400*  Final file-handling edits and trailer record handling          07640001
076500     IF NOT TRAILER-REC                                           07650001
076600         MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON  07660001
076700         GO TO 1000-ABEND-RTN.                                    07670001
076800                                                                  07680001
076900     MOVE INPATIENT-TREATMENT-REC-DATA TO WS-TRAILER-REC.         07690001
077000                                                                  07700001
077100     IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT                 07710001
077200         MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"        07720001
077300                               TO ABEND-REASON                    07730001
077400         GO TO 1000-ABEND-RTN.                                    07740001
077500                                                                  07750001
077600                                                                  07760001
077700     IF WS-ANCILLARY-CHARGES NOT EQUAL TO IN-ANCILLARY-CHARGES    07770001
077800         MOVE "** ANCILLARY CHARGES OUT OF BALANCE"               07780001
077900                               TO ABEND-REASON                    07790001
078000         MOVE WS-ANCILLARY-CHARGES TO EXPECTED-VAL                07800001
078100         MOVE IN-ANCILLARY-CHARGES TO ACTUAL-VAL                  07810001
078200         DISPLAY "** ANCILLARY CHARGES IN **"                     07820001
078300         DISPLAY WS-ANCILLARY-CHARGES                             07830001
078400         DISPLAY "** ANCILLARY CHARGES EXPECTED **"               07840001
078500         DISPLAY  IN-ANCILLARY-CHARGES.                           07850001
078600                                                                  07860001
078700     IF WS-MEDICATION-CHARGES  NOT EQUAL TO IN-MEDICATION-CHARGES 07870001
078800         MOVE "** MEDICATION CHARGES OUT OF BALANCE"              07880001
078900                               TO ABEND-REASON                    07890001
079000         DISPLAY "** MEDICATION CHARGES IN **"                    07900001
079100         DISPLAY WS-MEDICATION-CHARGES                            07910001
079200         DISPLAY "** MEDICATION CHARGES EXPECTED **"              07920001
079300         DISPLAY  IN-MEDICATION-CHARGES.                          07930001
079400                                                                  07940001
079500     IF WS-PHARMACY-CHARGES  NOT EQUAL TO IN-PHARMACY-CHARGES     07950001
079600         MOVE "** PHARMACY CHARGES OUT OF BALANCE"                07960001
079700                               TO ABEND-REASON                    07970001
079800         DISPLAY "** PHARMACY CHARGES IN **"                      07980001
079900         DISPLAY WS-PHARMACY-CHARGES                              07990001
080000         DISPLAY "** PHARMACY CHARGES EXPECTED **"                08000001
080100         DISPLAY  IN-PHARMACY-CHARGES.                            08010001
080200                                                                  08020001
080300     MOVE "T" TO RECORD-TYPE.                                     08030001
080400     ADD +1 TO RECORDS-WRITTEN.                                   08040001
080500     MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.                     08050001
080600     MOVE WS-ANCILLARY-CHARGES TO IN-ANCILLARY-CHARGES.           08060001
080700     MOVE WS-MEDICATION-CHARGES TO IN-MEDICATION-CHARGES.         08070001
080800     MOVE WS-PHARMACY-CHARGES TO IN-PHARMACY-CHARGES.             08080001
080900     WRITE INPATIENT-TREATMENT-REC-EDIT FROM WS-TRAILER-REC.      08090001
081000                                                                  08100001
081100*  Code the statement to close all files                          08110001
081200     PERFORM 850-CLOSE-FILES THRU 850-EXIT.                       08120001
081300                                                                  08130001
081400     DISPLAY "** RECORDS READ **".                                08140001
081500     DISPLAY RECORDS-READ.                                        08150001
081600     DISPLAY "** RECORD-IN EXPECTED **".                          08160001
081700     DISPLAY  IN-RECORD-COUNT.                                    08170001
081800     DISPLAY "** RECORDS WRITTEN **".                             08180001
081900     DISPLAY  RECORDS-WRITTEN.                                    08190001
082000     DISPLAY  RECORDS-IN-ERROR.                                   08200001
082100                                                                  08210001
082200*  Code the statement to Display a successful end-of-job msg      08220001
082300     DISPLAY "******** NORMAL END OF JOB TRTMNT ********".        08230001
082400 999-EXIT.                                                        08240001
082500     EXIT.                                                        08250001
082600                                                                  08260001
082700                                                                  08270001
082800 1000-ABEND-RTN.                                                  08280001
082900     WRITE SYSOUT-REC FROM ABEND-REC.                             08290001
083000     PERFORM 850-CLOSE-FILES THRU 850-EXIT.                       08300001
083100     DISPLAY "*** ABNORMAL END OF JOB - TRTMNT ***" UPON CONSOLE. 08310001
083200     DIVIDE ZERO-VAL INTO ONE-VAL.                                08320001
083300                                                                  08330001
083400 1000-DB2-ERROR-RTN.                                              08340001
083500************************************************************      08350001
083600*       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *      08360001
083700************************************************************      08370001
083800                                                                  08380001
083900      DISPLAY '**** DB2 ACCESS PROBLEM *****'.                    08390001
084000      DISPLAY '999-ERROR-TRAP-RTN '.                              08400001
084100      MOVE "*** DB2 PROBLEM ***" TO ERROR-MSG-FIRST.              08410001
084200      MOVE SQLCA TO ERROR-MSG-REST.                               08420001
084300      MULTIPLY SQLCODE BY -1 GIVING SQLCODE.                      08430001
084400      DISPLAY 'SQLCODE ==> ' SQLCODE.                             08440001
084500      DISPLAY SQLCA.                                              08450001
084600      DISPLAY SQLERRM.                                            08460001
084700      EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.               08470001
084800      EXEC SQL ROLLBACK WORK END-EXEC.                            08480001
084900      GO TO 1000-ABEND-RTN.                                       08490001
