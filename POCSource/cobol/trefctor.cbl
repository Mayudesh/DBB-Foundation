000100 IDENTIFICATION DIVISION.                                         00010001
000200 PROGRAM-ID.  TRTMNT.                                             00020001
000300 AUTHOR. JON SAYLES.                                              00030001
000400 INSTALLATION. COBOL DEV Center.                                  00040001
000500 DATE-WRITTEN. 01/23/05.                                          00050001
000600 DATE-COMPILED. 01/23/99.                                         00060067
000700 SECURITY. CONFIDENTIAL PATIENT DATA.                             00070001
000800                                                                  00080001
000900******************************************************************00090001
001000*REMARKS.                                                         00100008
001100*                                                                 00110001
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
002100*          NOTE: Y2K WINDOWING USED ON ALL DATE FIELDS            00210101
002100*                                                                 00210201
002200******************************************************************00220001
002300                                                                  00230001
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
006000                                                                  00600001
006100     SELECT PATMSTR                                               00610001
006200            ASSIGN       TO PATMSTR                               00620061
006300            ORGANIZATION IS INDEXED                               00630061
006400            ACCESS MODE  IS RANDOM                                00640061
006500            RECORD KEY   IS PATIENT-KEY                           00650061
006600            FILE STATUS  IS PATMSTR-STATUS.                       00660061
006700                                                                  00670001
006800 DATA DIVISION.                                                   00680001
006900 FILE SECTION.                                                    00690001
007000 FD  SYSOUT                                                       00700001
007100     RECORDING MODE IS F                                          00710001
007200     LABEL RECORDS ARE STANDARD                                   00720001
007300     RECORD CONTAINS 130 CHARACTERS                               00730001
007400     BLOCK CONTAINS 0 RECORDS                                     00740001
007500     DATA RECORD IS SYSOUT-REC.                                   00750061
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
012100 01  FILLER                     PIC X(33) VALUE                   01210001
012200        '* WORKING STORAGE BEGINS HERE *'.                        01220001
012300                                                                  01230001
012400 01  FILLER                     PIC X(33) VALUE                   01240001
012500             '****** DUMP MSG ****************'.                  01250001
012600***************************************************************** 01260001
012700*    DUMP POINTER AREA2s                                          01270001
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
014600     05 FILLER             PIC X(32)                              01460001
014700             VALUE '<<<<<<< WS DUMP POINTERS <<<<<<<'.            01470001
014800                                                                  01480001
014900 01  WS-CALLED-MODULE   PIC X(8) VALUE "DTEVAL".                  01490001
015000 01  DUMP-DISPLAY.                                                01500001
015100     05 DUMP-STATUS               PIC X(3)  VALUE SPACES.         01510001
015200     05 DUMP-MESSAGE              PIC X(61) VALUE 'NO MSG'.       01520001
           05 WS-CICS-XFER              PIC X(8)  VALUE SPACES.
015300                                                                  01530001
015400 01  FILE-STATUS-CODES.                                           01540001
015500     05  PATMSTR-STATUS          PIC X(2).                        01550001
015600         88 RECORD-FOUND         VALUE "00".                      01560001
015700         88 PATMSTR-NOT-FOUND    VALUE "23".                      01570001
015800     05  OFCODE                  PIC X(2).                        01580001
015900         88 CODE-WRITE    VALUE SPACES.                           01590001
016000                                                                  01600001
016100 COPY TREATMNT.                                                   01610001
016200                                                                  01620001
016300 01  WS-TRAILER-REC.                                              01630001
016400     05  FILLER                  PIC X(1).                        01640001
016500     05  IN-RECORD-COUNT         PIC 9(9).                        01650001
016600     05  FILLER                  PIC X(1).                        01660001
016700     05  IN-MEDICATION-CHARGES   PIC S9(9)V99.                    01670001
016800     05  IN-PHARMACY-CHARGES     PIC S9(7)V99.                    01680001
016900     05  IN-ANCILLARY-CHARGES    PIC S9(5)V99.                    01690001
017000                                                                  01700001
017100 01  WS-OUTPUT-REC.                                               01710001
017200     05  PATIENT-NBR-O           PIC 9(6).                        01720001
017300     05  FILLER                  PIC X(2) VALUE SPACES.           01730001
017400     05  PATIENT-NAME-O          PIC X(20).                       01740001
017500     05  PATIENT-PHONE-O         PIC X(10).                       01750001
017600     05  FILLER                  PIC X(2) VALUE SPACES.           01760001
017700     05  PATIENT-TYPE-O          PIC X(2).                        01770001
017800     05  BED-IDENTITY-O          PIC ZZZ9.                        01780001
017900     05  FILLER                  PIC X(2) VALUE SPACES.           01790001
018000     05  CURR-DATE-O             PIC X(6).                        01800001
018100     05  FILLER                  PIC X(2) VALUE SPACES.           01810001
018200     05  PATIENT-AMT-PER-DAY-O   PIC $$,$$9.99.                   01820001
018300     05  FILLER                  PIC X(2) VALUE SPACES.           01830001
018400     05  INS-COVERAGE-PERC-O     PIC 999.                         01840001
018500     05  FILLER                  PIC X(2) VALUE SPACES.           01850001
018600     05  INS-TYPE-O              PIC X(4).                        01860001
018700     05  HOSPITAL-STAY-LTH-O     PIC 999.                         01870001
018800     05  FILLER                  PIC X(7) VALUE SPACES.           01880001
018900                                                                  01890001
019000     COPY PATMSTR.                                                01900061
019100** VSAM FILE                                                      01910001
019200                                                                  01920001
019300 01  WS-SYSOUT-REC.                                               01930001
019400     05  MSG                     PIC X(80).                       01940001
019500                                                                  01950001
019600 77  WS-DATE                     PIC 9(6) COMP-3.                 01960001
019700                                                                  01970001
019800 01  COUNTERS-AND-ACCUMULATORS.                                   01980001
019900     05 RECORDS-WRITTEN          PIC 9(7) COMP.                   01990001
020000     05 RECORDS-IN-ERROR         PIC 9(7) COMP.                   02000001
020100     05 RECORDS-READ             PIC 9(7) COMP.                   02010001
020200     05 WS-MEDICATION-CHARGES    PIC S9(8)V99 COMP-3.             02020009
020300     05 WS-PHARMACY-CHARGES      PIC S9(9)V99 COMP-3.             02030009
020400     05 WS-ANCILLARY-CHARGES     PIC S9(6)V99 COMP-3.             02040009
           05 WS-DIAGCODE-TEMP         PIC  9(6).                       02040171
020500                                                                  02050001
020600 01  MISC-WS-FLDS.                                                02060001
020700     05 STR-LTH                  PIC 9(04) VALUE 0.               02070004
020800     05 RETURN-CD                PIC S9(04) VALUE 0.              02080001
020900     05 ROW-SUB                  PIC S9(04) COMP-3.               02090009
021000                                                                  02100001
021100 01  FLAGS-AND-SWITCHES.                                          02110001
021200     05 MORE-DATA-SW             PIC X(01) VALUE "Y".             02120001
021300         88 NO-MORE-DATA VALUE "N".                               02130001
021400     05 ERROR-FOUND-SW           PIC X(01) VALUE "N".             02140001
021500         88 RECORD-ERROR-FOUND VALUE "Y".                         02150001
021600         88 VALID-RECORD  VALUE "N".                              02160001
021600         88 VALID-ROW  VALUE    "R".                              02160153
021700     05  MORE-TABLE-ROWS         PIC X(01) VALUE "Y".             02170001
021800         88 NO-MORE-TABLE-ROWS VALUE "N".                         02180001
021900                                                                  02190001
022000* COPY ABENDREC.                                                  02200031
022100** QSAM FILE                                                      02210001
022200 COPY ABENDREC.                                                   02220001
022300 COPY BADD400.                                                    02230001
022300 COPY CUST.                                                       02230001
022300 COPY CDAT3L.                                                     02230001
022300 COPY EMPL.                                                       02230001
022300 COPY FUNDFILE.                                                   02230001
022400                                                                  02240001
022500* COPY DIAGCODE.                                                  02250001
022600******************************************************************02260001
022700***** DB2 TABLE DCLGENS                                           02270001
022800 01  DCLDIAG-CODES.                                               02280001
022900     10 DIAG-CODE                      PIC X(05).                 02290001
023000     10 INS-TYPE                       PIC X(03).                 02300001
023100     10 COPAY                          PIC S9(4) COMP.            02310001
023200     10 DEDUCTIBLE                     PIC S9(4) COMP.            02320001
023300                                                                  02330001
023400 01  DCLWARD-CODES.                                               02340001
023500     10 WARD-ID                        PIC X(04).                 02350001
023600     10 PRIMARY-PHYSICIAN-ID           PIC X(08).                 02360001
023700     10 SUPERVISE-NURSE-ID             PIC X(08).                 02370001
023800     10 LOCATION                       PIC X(08).                 02380001
023900     10 NUMBER-OF-BEDS                 PIC S9(4) COMP.            02390001
024000     10 BASE-ROOM-CHARGE               PIC S9(5)V99 COMP-3.       02400001
024100                                                                  02410001
024200 01  DCLHOSP-BED.                                                 02420001
024300     10 BED-ID                         PIC X(04).                 02430001
024400     10 ROOM-ID                        PIC X(08).                 02440001
024500     10 WARD-ID                        PIC X(08).                 02450001
024600     10 SPECIAL-CHARGES                PIC S9(5)V99 COMP-3.       02460001
024700                                                                  02470001
024800 01  DCLMEDICATION.                                               02480001
024900     10 MEDICATION-ID                  PIC X(08).                 02490001
025000     10 MED-NAME                       PIC X(08).                 02500001
025100     10 SHORT-DESCRIPTION              PIC X(08).                 02510001
025200     10 COST                           PIC S9(5)V99 COMP-3.       02520001
025300     10 PHARMACY-COST                  PIC S9(3)V99 COMP-3.       02530001
025400                                                                  02540001
025500     EXEC SQL INCLUDE  SQLCA END-EXEC.                            02550001
025600                                                                  02560001
025700 PROCEDURE DIVISION.                                              02570001
025800     PERFORM 000-HOUSEKEEPING THRU 000-EXIT.                      02580001
025900     PERFORM 100-MAINLINE THRU 100-EXIT                           02590001
026000             UNTIL NO-MORE-DATA OR                                02600001
026100******* Balancing logic put in by TGD 02/12/92                    02610001
026200             TRAILER-REC.                                         02620001
026300     PERFORM 999-CLEANUP THRU 999-EXIT.                           02630001
026400     MOVE +0 TO RETURN-CODE.                                      02640001
026500     GOBACK.                                                      02650001
026600                                                                  02660001
026700 000-HOUSEKEEPING.                                                02670001
026800     MOVE "000-HOUSEKEEPING" TO PARA-NAME.                        02680001
026900     DISPLAY "HOUSEKEEPING".                                      02690001
027000*  Code your statement here to OPEN files                         02700001
027100     ACCEPT  WS-DATE FROM DATE.                                   02710001
027200     INITIALIZE COUNTERS-AND-ACCUMULATORS.                        02720001
027300     PERFORM 800-OPEN-FILES THRU 800-EXIT.                        02730001
027400     PERFORM 900-READ-TRMTDATA THRU 900-EXIT.                     02740001
027500     DISPLAY 'MORE-DATA-SW ' MORE-DATA-SW.                        02750001
027600     IF NO-MORE-DATA                                              02760001
027700         MOVE "EMPTY INPUT FILE" TO ABEND-REASON                  02770001
027800         GO TO 1000-ABEND-RTN.                                    02780001
027900 000-EXIT.                                                        02790001
028000     EXIT.                                                        02800001
028100                                                                  02810001
028200 100-MAINLINE.                                                    02820001
028300     DISPLAY '100-MAINLINE...JS'.                                 02830055
           DISPLAY 'TRMT REC ' INPATIENT-TREATMENT-REC.                 02830155
028400     MOVE "100-MAINLINE" TO PARA-NAME.                            02840001
028500*  Validate patient type and insurance coverage                   02850001
028600     PERFORM 300-FIELD-EDITS THRU 300-EXIT.                       02860001
028700                                                                  02870001
028800     IF RECORD-ERROR-FOUND                                        02880001
028900         ADD +1 TO RECORDS-IN-ERROR                               02890001
029000         PERFORM 710-WRITE-TRMTERR THRU 710-EXIT                  02900001
029100     ELSE                                                         02910001
029200         PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.                02920001
029300     PERFORM 900-READ-TRMTDATA THRU 900-EXIT.                     02930001
029400 100-EXIT.                                                        02940001
029500     EXIT.                                                        02950001
029600                                                                  02960001
029700 300-FIELD-EDITS.                                                 02970001
029800     MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.            02980001
029900     MOVE "300-FIELD-EDITS" TO PARA-NAME.                         02990001
030000******** non-numeric fields                                       03000001
030100     IF NOT VALID-BILLABLE-TYPES IN BILLABLE-TREATMENT-IND        03010001
030200        MOVE "*** INVALID BILLABLE TYPE" TO                       03020001
030300        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03030001
030400        MOVE "Y" TO ERROR-FOUND-SW                                03040001
030500        GO TO 300-EXIT.                                           03050001
030600                                                                  03060001
030700     IF NOT VALID-TRTMNT-MODES IN TREATMENT-MODE                  03070001
030800        MOVE "*** INVALID TREATMENT MODE" TO                      03080001
030900        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03090001
031000        MOVE "Y" TO ERROR-FOUND-SW                                03100001
031100        GO TO 300-EXIT.                                           03110001
031200                                                                  03120001
031300     IF PATIENT-ID IN INPATIENT-TREATMENT-REC NOT NUMERIC         03130001
031400        MOVE "*** NON-NUMERIC PATIENT-ID" TO                      03140001
031500        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03150001
031600        MOVE "Y" TO ERROR-FOUND-SW                                03160001
031700        GO TO 300-EXIT.                                           03170001
031800                                                                  03180001
031900     IF PATIENT-ID IN INPATIENT-TREATMENT-REC = ZERO              03190001
032000        MOVE "*** INVALID (000000) PATIENT-ID" TO                 03200001
032100        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03210001
032200        MOVE "Y" TO ERROR-FOUND-SW                                03220001
032300        GO TO 300-EXIT.                                           03230001
032400                                                                  03240001
032500     IF BED-IDENTITY IN INPATIENT-TREATMENT-REC NOT NUMERIC       03250001
032600        MOVE "*** NON-NUMERIC BED-IDENTITY" TO                    03260001
032700        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03270001
032800        MOVE "Y" TO ERROR-FOUND-SW                                03280001
032900        GO TO 300-EXIT.                                           03290001
033000                                                                  03300001
033100     IF MEDICATION-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC    03310001
033200        MOVE "*** NON-NUMERIC MEDICATION-COST" TO                 03320001
033300        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03330001
033400        MOVE "Y" TO ERROR-FOUND-SW                                03340001
033500        GO TO 300-EXIT.                                           03350001
033600                                                                  03360001
033700     IF PHARMACY-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC      03370001
033800        MOVE "*** NON-NUMERIC PHARMACY COSTS" TO                  03380001
033900        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03390001
034000        MOVE "Y" TO ERROR-FOUND-SW                                03400049
034100        GO TO 300-EXIT.                                           03410001
034200                                                                  03420001
034300     IF ANCILLARY-CHARGE IN INPATIENT-TREATMENT-REC NOT NUMERIC   03430001
034400        MOVE "*** NON-NUMERIC ANCILLARY-CHARGES" TO               03440001
034500        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03450001
034600        MOVE "Y" TO ERROR-FOUND-SW                                03460049
034700        GO TO 300-EXIT.                                           03470001
034800********************                                              03480001
034900     IF ATTENDING-PHYS-ID = SPACES                                03490001
035000        MOVE "*** BLANK ATTENDING PHYSICIAN-ID" TO                03500001
035100        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03510001
035200        MOVE "Y" TO ERROR-FOUND-SW                                03520001
035300        GO TO 300-EXIT.                                           03530001
035400                                                                  03540001
035500     IF PRESCRIBING-PHYS-ID = SPACES                              03550001
035600        MOVE "*** BLANK PRESCRIBING PHYSICIAN-ID" TO              03560001
035700        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03570001
035800        MOVE "Y" TO ERROR-FOUND-SW                                03580001
035900        GO TO 300-EXIT.                                           03590001
036000                                                                  03600001
036100*    CALL WS-CALLED-MODULE USING TREATMENT-DATE, RETURN-CD.       03610026
036200*     CALL 'DTEVAL' USING TREATMENT-DATE, RETURN-CD.              03620060
036300     CALL 'DATEVAL' USING TREATMENT-DATE, RETURN-CD.              03630060
036400     IF RETURN-CD < 0                                             03640001
036500        MOVE "*** BAD DATE PORTION OF DATE-TIME" TO               03650001
036600        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03660001
036700        MOVE "Y" TO ERROR-FOUND-SW                                03670001
036800        GO TO 300-EXIT.                                           03680001
036900                                                                  03690001
037000     MOVE "Y" TO MORE-TABLE-ROWS.                                 03700001
037100     PERFORM 350-CHECK-LAB-TABLE THRU 350-EXIT VARYING ROW-SUB    03710001
037200          FROM 1 BY 1 UNTIL NO-MORE-TABLE-ROWS OR ROW-SUB = 8.    03720034
037300                                                                  03730001
037400     IF VALID-RECORD                                              03740001
037500         PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-EXIT.           03750001
037600                                                                  03760001
037700****** VERIFY TABLE (JUST TYPES AND LAB-TEST-ID)                  03770001
037800                                                                  03780001
037900 300-EXIT.                                                        03790001
038000     EXIT.                                                        03800001
038100                                                                  03810001
038200 350-CHECK-LAB-TABLE.                                             03820001
038300     IF LAB-TEST-ID(ROW-SUB) = SPACES                             03830001
038400        MOVE "N" TO MORE-TABLE-ROWS                               03840001
038500        GO TO 350-EXIT.                                           03850001
038600                                                                  03860001
038700     IF NOT VALID-CATEGORY(ROW-SUB)                               03870001
038800        MOVE "*** INVALID LAB-TEST CATEGORY" TO                   03880001
038900        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    03890001
039000        MOVE "Y" TO ERROR-FOUND-SW                                03900001
039100        GO TO 350-EXIT.                                           03910001
039200                                                                  03920001
039300 350-EXIT.                                                        03930001
039400     EXIT.                                                        03940001
039500                                                                  03950001
039600                                                                  03960001
039700 400-NUMERIC-RANGE-EDITS.                                         03970001
           CALL 'test1' USING MISC-WS-FLDS, PATIENT-MASTER-REC, SQLCA,
                   TRMTDATA, PATMSTR, DCLHOSP-BED, SYSOUT,
                   FILE-STATUS-CODES, DCLWARD-CODES, WS-CALLED-MODULE,
                   ABEND-REC, TRMTEDIT, SYSOUT-REC,
                   COUNTERS-AND-ACCUMULATORS, DCLMEDICATION,
                   FLAGS-AND-SWITCHES, PATMSTR-REC, DCLDIAG-CODES,
                   INPATIENT-TREATMENT-REC, TRMTERR,
                   INPATIENT-TREATMENT-REC-ERR.
042300                                                                  04230001
042600                                                                  04260001
047300                                                                  04730001
047600                                                                  04760001
049100                                                                  04910001
049400                                                                  04940001
066600                                                                  06660001
066700 700-WRITE-TRMTEDIT.                                              06670001
066800     MOVE "700-WRITE-TRMTEDIT" TO PARA-NAME.                      06680001
066900                                                                  06690001
067000     WRITE INPATIENT-TREATMENT-REC-EDIT                           06700001
067100         FROM INPATIENT-TREATMENT-REC.                            06710001
067200     ADD MEDICATION-COST  TO WS-MEDICATION-CHARGES.               06720001
067300     ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.                06730001
067400     ADD PHARMACY-COST IN INPATIENT-TREATMENT-REC                 06740001
067500                          TO WS-PHARMACY-CHARGES.                 06750001
067600     ADD +1 TO RECORDS-WRITTEN.                                   06760001
067700 700-EXIT.                                                        06770001
067800     EXIT.                                                        06780001
067900                                                                  06790001
068000 710-WRITE-TRMTERR.                                               06800001
068100     MOVE INPATIENT-TREATMENT-REC TO REST-OF-REC.                 06810001
068200     WRITE INPATIENT-TREATMENT-REC-ERR.                           06820001
068300     ADD +1 TO RECORDS-IN-ERROR.                                  06830001
068400 710-EXIT.                                                        06840001
068500     EXIT.                                                        06850001
068600                                                                  06860001
068700 800-OPEN-FILES.                                                  06870001
068800     DISPLAY '800-OPEN-FILES..'.                                  06880074
068900     MOVE "800-OPEN-FILES" TO PARA-NAME.                          06890001
069000     OPEN INPUT TRMTDATA.                                         06900001
069100     OPEN OUTPUT TRMTEDIT, SYSOUT, TRMTERR.                       06910001
069200     OPEN I-O PATMSTR.                                            06920001
069300 800-EXIT.                                                        06930001
069400     EXIT.                                                        06940001
069500                                                                  06950001
070300                                                                  07030001
069600 851-WRITE-AUDIT-LOGS.                                            07030135
069700     MOVE "851-CLOSE-FILES" TO PARA-NAME.                         07030235
           WRITE SYSOUT-REC AFTER ADVANCING 3.                          07030336
      ***** END LOG/AUDIT TRAIL                                         07030535
070100 851-EXIT.                                                        07030635
070200     EXIT.                                                        07030735
070300                                                                  07030835
      *    IF SQLCODE = -803 GO TO 852-EXIT.                            07031036
069700*    MOVE "852-WRITE-DUP-IDX-ERR" TO PARA-NAME.                   07031136
      *    WRITE SYSOUT-REC AFTER ADVANCING 1.                          07031236
      ***** END LOG/AUDIT TRAIL                                         07031336
070300                                                                  07031636
069600 853-CLOSE-PGM.                                                   07031736
      *    IF SQLCODE = -803 GO TO 852-WRITE-DUP-IDX-ERR.               07031836
069700     MOVE "853-WRITE-DUP-IDX-ERR" TO PARA-NAME.                   07031936
      *    GOBACK.                                                      07032036
           WRITE SYSOUT-REC AFTER ADVANCING 1.                          07032136
           EXEC CICS
             LINK PROGRAM ('CDAT1')
           END-EXEC.
           EXEC CICS
             XCTL PROGRAM ('CDAT2')
           END-EXEC.
      ***** END LOG/AUDIT TRAIL                                         07032236
070100 853-EXIT.                                                        07032336
070200     EXIT.                                                        07032436
070300                                                                  07032536
070400 900-READ-TRMTDATA.                                               07040001
070500     DISPLAY '900-READ-TRMTDATA...'.                              07050001
070600*  Code your statements here to read the input file               07060001
070700*  Remember to move "NO" to IFCODE if the input file is AT END    07070001
070800     READ TRMTDATA  INTO INPATIENT-TREATMENT-REC                  07080001
070900         AT END MOVE "N" TO MORE-DATA-SW                          07090001
071000         GO TO 900-EXIT                                           07100001
071100     END-READ                                                     07110001
071200     MOVE "N" TO ERROR-FOUND-SW.                                  07120001
071300     ADD +1 TO RECORDS-READ.                                      07130001
071400     DISPLAY '900.1 RECORDS-READ = ' RECORDS-READ.                07140001
071500 900-EXIT.                                                        07150001
071600     EXIT.                                                        07160001
071700                                                                  07170001
071800 999-CLEANUP.                                                     07180001
071900     MOVE "999-CLEANUP" TO PARA-NAME.                             07190001
072000*  Final file-handling edits and trailer record handling          07200001
072100     IF NOT TRAILER-REC                                           07210001
072200         MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON  07220001
072300         GO TO 1000-ABEND-RTN.                                    07230001
072400                                                                  07240001
072500     MOVE INPATIENT-TREATMENT-REC-DATA TO WS-TRAILER-REC.         07250001
072600                                                                  07260001
072700     IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT                 07270001
072800         MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"        07280001
072900                               TO ABEND-REASON                    07290001
073000         GO TO 1000-ABEND-RTN.                                    07300001
073100                                                                  07310001
073200                                                                  07320001
073300     IF WS-ANCILLARY-CHARGES NOT EQUAL TO IN-ANCILLARY-CHARGES    07330001
073400         MOVE "** ANCILLARY CHARGES OUT OF BALANCE"               07340001
073500                               TO ABEND-REASON                    07350001
073600         MOVE WS-ANCILLARY-CHARGES TO EXPECTED-VAL                07360001
073700         MOVE IN-ANCILLARY-CHARGES                                07370001
073800                TO ACTUAL-VAL                                     07380001
073900         DISPLAY "** ANCILLARY CHARGES IN **"                     07390001
074000         DISPLAY WS-ANCILLARY-CHARGES                             07400001
074100         DISPLAY "** ANCILLARY CHARGES EXPECTED **"               07410001
074200         DISPLAY  IN-ANCILLARY-CHARGES.                           07420001
074300                                                                  07430001
074400     IF WS-MEDICATION-CHARGES  NOT EQUAL TO IN-MEDICATION-CHARGES 07440001
074500         MOVE "** MEDICATION CHARGES OUT OF BALANCE"              07450001
074600                               TO ABEND-REASON                    07460001
074700         DISPLAY "** MEDICATION CHARGES IN **"                    07470001
074800         DISPLAY WS-MEDICATION-CHARGES                            07480001
074900         DISPLAY "** MEDICATION CHARGES EXPECTED **"              07490001
075000         DISPLAY  IN-MEDICATION-CHARGES.                          07500001
075100                                                                  07510001
075200     IF WS-PHARMACY-CHARGES  NOT EQUAL TO IN-PHARMACY-CHARGES     07520001
075300         MOVE "** PHARMACY CHARGES OUT OF BALANCE"                07530001
075400                               TO ABEND-REASON                    07540001
075500         DISPLAY "** PHARMACY CHARGES IN **"                      07550001
075600         DISPLAY WS-PHARMACY-CHARGES                              07560001
075700         DISPLAY "** PHARMACY CHARGES EXPECTED **"                07570001
075800         DISPLAY  IN-PHARMACY-CHARGES.                            07580001
075900                                                                  07590001
076000     MOVE "T" TO RECORD-TYPE.                                     07600001
076100     ADD +1 TO RECORDS-WRITTEN.                                   07610001
076200     MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.                     07620001
076300     MOVE WS-ANCILLARY-CHARGES TO IN-ANCILLARY-CHARGES.           07630001
076400     MOVE WS-MEDICATION-CHARGES TO IN-MEDICATION-CHARGES.         07640001
076500     MOVE WS-PHARMACY-CHARGES TO IN-PHARMACY-CHARGES.             07650001
076600     WRITE INPATIENT-TREATMENT-REC-EDIT FROM WS-TRAILER-REC.      07660001
076700                                                                  07670001
076800*  Code the statement to close all files                          07680001
076900     PERFORM 850-CLOSE-FILES THRU 850-EXIT.                       07690001
077000                                                                  07700001
077100     DISPLAY "** RECORDS READ **".                                07710001
077200     DISPLAY RECORDS-READ.                                        07720001
077300     DISPLAY "** RECORD-IN EXPECTED **".                          07730001
077400     DISPLAY  IN-RECORD-COUNT.                                    07740001
077500     DISPLAY "** RECORDS WRITTEN **".                             07750001
077600     DISPLAY  RECORDS-WRITTEN.                                    07760001
077700     DISPLAY "** ERROR RECORDS FOUND **".                         07770001
077800     DISPLAY  RECORDS-IN-ERROR.                                   07780001
077900                                                                  07790001
078000*  Code the statement to Display a successful end-of-job msg      07800001
078100     DISPLAY "******** NORMAL END OF JOB TRTMNT ********".        07810001
078200 999-EXIT.                                                        07820001
078300     EXIT.                                                        07830001
078400                                                                  07840001
079000                                                                  07900001


       COPY test2.

