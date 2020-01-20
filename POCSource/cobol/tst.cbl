000100 IDENTIFICATION DIVISION.                                         00010001
000200 PROGRAM-ID.  SANDBOX.                                            00020001
000300 AUTHOR. JON SAYLES.                                              JSTHSD01
000400 INSTALLATION. COBOL DEV Center.                                  00040001
000500 DATE-WRITTEN. 01/23/88.                                          00050001
000600 DATE-COMPILED. 01/23/88.                                         00060001
000700 SECURITY. NON-CONFIDENTIAL.                                      00070001
000800                                                                  00080001
000900******************************************************************00090001
001000*REMARKS.                                                         00100001
001100*                                                                 00110001
001200*          THE PROGRAM'S PROCEDURE INCLUDES                       00120001
001300*                A DAILY CONTROL BREAK REPORT AND FILE UPDATES    00130001
001400*                FOR THE AVAILABLE HOSPITAL BEDS/ROOMS/WARDS      00140001
001500*                                                                 00150001
001600*          IT BASES IT'S PROCESSING ON THE DAILY PATIENT FILE     00160001
001700*          WHICH IS SORTED ON WARD/ROOM/BED - FOR CORRECT         00170001
001800*          CONTROL-BREAK REPORTING                                00180001
001900*                                                                 00190001
002000*                                                                 00200001
002100******************************************************************00210001
002200                                                                  00220001
002300       INPUT FILE           - DDS0001.PATSRCH                     00230001
002400                                                                  00240001
002500       VSAM MASTER FILES    - DDS0001.PATMASTR & DDS0001.PATPERSN 00250001
002600                                                                  00260001
002700       INPUT ERROR FILE     - DDS0001.PATERR                      00270001
002800                                                                  00280001
002900       OUTPUT FILE PRODUCED -  DDS001.WARDPRT                     00290001
003000                                                                  00300001
003100       DUMP FILE            -   SYSOUT                            00310001
003200                                                                  00320001
003300******************************************************************00330001
003400                                                                  00340001
003500 ENVIRONMENT DIVISION.                                            00350001
003600 CONFIGURATION SECTION.                                           00360001
003700 SOURCE-COMPUTER. IBM-390.                                        00370001
003800 OBJECT-COMPUTER. IBM-390.                                        00380001
003900 SPECIAL-NAMES.                                                   00390001
004000     C01 IS NEXT-PAGE.                                            00400001
004100                                                                  00410001
004200 INPUT-OUTPUT SECTION.                                            00420001
004300 FILE-CONTROL.                                                    00430001
004400     SELECT SYSOUT                                                00440001
004500     ASSIGN TO UT-S-SYSOUT                                        00450001
004600       ORGANIZATION IS SEQUENTIAL.                                00460001
004700                                                                  00470001
004800     SELECT TRMTDATA                                              00480001
004900     ASSIGN TO UT-S-TRMTDATA                                      00490001
005000       ACCESS MODE IS SEQUENTIAL                                  00500001
005100       FILE STATUS IS OFCODE.                                     00510001
005200                                                                  00520001
005300                                                                  00530001
005400     SELECT PATSRCH                                               00540001
005500     ASSIGN TO UT-S-PATSRCH                                       00550001
005600       ACCESS MODE IS SEQUENTIAL                                  00560001
005700       FILE STATUS IS OFCODE.                                     00570001
005800                                                                  00580001
005900     SELECT WARDFILE                                              00590001
006000     ASSIGN TO UT-S-WARDRPT                                       00600001
006100       ACCESS MODE IS SEQUENTIAL                                  00610001
006200       FILE STATUS IS OFCODE.                                     00620001
006300                                                                  00630001
006400     SELECT PATERR                                                00640001
006500     ASSIGN TO UT-S-PATERR                                        00650001
006600       ACCESS MODE IS SEQUENTIAL                                  00660001
006700       FILE STATUS IS OFCODE.                                     00670001
006800                                                                  00680001
006900                                                                  00690001
007000     SELECT TRMTERR                                               00700001
007100     ASSIGN TO UT-S-TRMTERR                                       00710001
007200       ACCESS MODE IS SEQUENTIAL                                  00720001
007300       FILE STATUS IS OFCODE.                                     00730001
007400                                                                  00740001
007500     SELECT PATMSTR                                               00750001
007600            ASSIGN       to PATMSTR                               00760001
007700            ORGANIZATION is INDEXED                               00770001
007800            ACCESS MODE  is RANDOM                                00780001
007900            RECORD KEY   is PATMSTR-KEY                           00790001
008000            FILE STATUS  is PATMSTR-STATUS.                       00800001
008100                                                                  00810001
008200     SELECT PATPERSN                                              00820001
008300            ASSIGN       to PATPERSN                              00830001
008400            ORGANIZATION is INDEXED                               00840001
008500            ACCESS MODE  is RANDOM                                00850001
008600            RECORD KEY   is PATPERSN-KEY                          00860001
008700            FILE STATUS  is PATPERSN-STATUS.                      00870001
008800                                                                  00880001
008900 DATA DIVISION.                                                   00890001
009000 FILE SECTION.                                                    00900001
009100 FD  SYSOUT                                                       00910001
009200     RECORDING MODE IS F                                          00920001
009300     LABEL RECORDS ARE STANDARD                                   00930001
009400     RECORD CONTAINS 130 CHARACTERS                               00940001
009500     BLOCK CONTAINS 0 RECORDS                                     00950001
009600     DATA RECORD IS SYSOUT-Rec.                                   00960001
009700 01  SYSOUT-REC  PIC X(130).                                      00970001
009800                                                                  00980001
009900                                                                  00990001
010000 FD  TRMTERR                                                      01000001
010100     RECORDING MODE IS F                                          01010001
010200     LABEL RECORDS ARE STANDARD                                   01020001
010300     RECORD CONTAINS 1141 CHARACTERS                              01030001
010400     BLOCK CONTAINS 0 RECORDS                                     01040001
010500     DATA RECORD IS INPATIENT-TREATMENT-REC-ERR.                  01050001
010600 01  INPATIENT-TREATMENT-REC-ERR.                                 01060001
010700     05  ERR-MSG                     PIC X(40).                   01070001
010800     05  REST-OF-REC                 PIC X(1101).                 01080001
010900                                                                  01090001
011000***** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM     01100001
011100****** IT CONSISTS OF ALL PATIENT TREATMENTS ENTERED              01110001
011200****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS     01120001
011300****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND    01130001
011400 FD  TRMTDATA                                                     01140001
011500     RECORDING MODE IS F                                          01150001
011600     LABEL RECORDS ARE STANDARD                                   01160001
011700     RECORD CONTAINS 1101 CHARACTERS                              01170001
011800     BLOCK CONTAINS 0 RECORDS                                     01180001
011900     DATA RECORD IS INPATIENT-TREATMENT-REC-DATA.                 01190001
012000 01  INPATIENT-TREATMENT-REC-DATA PIC X(1101).                    01200001
012100                                                                  01210001
012200 FD  WARDFILE                                                     01220001
012300     RECORDING MODE IS F                                          01230001
012400     LABEL RECORDS ARE STANDARD                                   01240001
012500     RECORD CONTAINS 132 CHARACTERS                               01250001
012600     BLOCK CONTAINS 0 RECORDS                                     01260001
012700     DATA RECORD IS SYSOUT-Rec.                                   01270001
012800 01  RPT-REC  PIC X(132).                                         01280001
012900                                                                  01290001
013000****** THIS FILE IS PASSED IN FROM THE DATA COLLECTIONS SYSTEM    01300001
013100****** IT CONSISTS OF ALL PATIENT RECORDS ENTERED                 01310001
013200****** THERE ARE TWO RECORD FORMATS - DETAIL AND TRAILER RECS     01320001
013300****** OUT OF BALANCE CONDITIONS SHOULD CAUSE THE JOB TO ABEND    01330001
013400 FD  PATSRCH                                                      01340001
013500     RECORDING MODE IS F                                          01350001
013600     LABEL RECORDS ARE STANDARD                                   01360001
013700     RECORD CONTAINS 993 CHARACTERS                               01370001
013800     BLOCK CONTAINS 0 RECORDS                                     01380001
013900     DATA RECORD IS INPATIENT-DAILY-REC-SRCH.                     01390001
014000 01  INPATIENT-DAILY-REC-SRCH PIC X(993).                         01400001
014100                                                                  01410001
014200 FD  PATERR                                                       01420001
014300     RECORDING MODE IS F                                          01430001
014400     LABEL RECORDS ARE STANDARD                                   01440001
014500     RECORD CONTAINS 1133 CHARACTERS                              01450001
014600     BLOCK CONTAINS 0 RECORDS                                     01460001
014700     DATA RECORD IS INPATIENT-DAILY-REC-ERR.                      01470001
014800 01  INPATIENT-DAILY-REC-ERR.                                     01480001
014900     05  ERR-MSG                      PIC X(40).                  01490001
015000     05  REST-OF-PAT-REC              PIC X(993).                 01500001
015100                                                                  01510001
015200 FD  PATMSTR                                                      01520001
015300     RECORD CONTAINS 2964 CHARACTERS                              01530001
015400     DATA RECORD IS PATIENT-MASTER-REC.                           01540001
015500 01  PATMSTR-REC.                                                 01550001
015600     05 PATMSTR-KEY      PIC X(06).                               01560001
015700     05 FILLER           PIC X(2958).                             01570001
015800                                                                  01580001
015900 FD  PATPERSN                                                     01590001
016000     RECORD CONTAINS 800 CHARACTERS                               01600001
016100     DATA RECORD IS PATPERSN-REC.                                 01610001
016200 01  PATPERSN-REC.                                                01620001
016300     05 PATPERSN-KEY      PIC X(06).                              01630001
016400     05 FILLER           PIC X(794).                              01640001
016500                                                                  01650001
016600 WORKING-STORAGE SECTION.                                         01660001
016700                                                                  01670001
016800 01  FILE-STATUS-CODES.                                           01680001
016900     05  PATMSTR-STATUS          PIC X(2).                        01690001
017000         88 PATMSTR-FOUND    VALUE "00".                          01700001
017100     05  PATPERSN-STATUS          PIC X(2).                       01710001
017200         88 PATPERSN-FOUND    VALUE "00".                         01720001
017300     05  OFCODE                  PIC X(2).                        01730001
017400         88 CODE-WRITE    VALUE SPACES.                           01740001
017500     05 MORE-DATA-SW             PIC X(1) VALUE SPACE.            01750001
017600     05 WS-INFILE-RECORD-READ-CTR   PIC 9(7) COMP VALUE 0.        01760001
017700                                                                  01770001
017800 01  FLAGS-AND-SWITCHES.                                          01780001
017900*    05 MORE-DATA-SWITCH             PIC X(01) VALUE "Y".         01790001
018000         88 NO-MORE-DATA VALUE "N".                               01800001
018100     05 ERR-FOUND-SWITCH           PIC X(01) VALUE "N".           01810001
018200     05 ERROR-FOUND-SW           PIC X(01) VALUE "N".             01820001
018300         88 RECORD-ERROR-FOUND VALUE "Y".                         01830001
018400         88 VALID-RECORD  VALUE "N".                              01840001
018500     05  MORE-TABLE-ROWS         PIC X(01) VALUE "Y".             01850001
018600         88 NO-MORE-TABLE-ROWS VALUE "N".                         01860001
018700                                                                  01870001
018800 COPY BNCHTRMT.                                                   01880001
018900** QSAM FILE                                                      01890001
019000 01  INPATIENT-DAILY-REC.                                         01900001
019100     05  PATIENT-RECORD-TYPE     PIC X(01).                       01910001
019200         88  TRAILER-REC-D     VALUE "T".                         01920001
019300     05  PATIENT-ID              PIC 9(6).                        01930001
019400     05  CURR-DTE                PIC X(08).                       01940001
019500     05  BED-IDENTITY-W            PIC 9(4).                      01950001
019600     05  ROOM-IDENTITY           PIC 9(4).                        01960001
019700     05  TOTAL-ROOM-CHARGE       PIC 9(7)V99.                     01970001
019800     05  BASE-ROOM-CHARGE        PIC 9(7)V99.                     01980001
019900     05  ROOM-DATE-FROM          PIC X(08).                       01990001
020000     05  ROOM-DATE-TO            PIC X(08).                       02000001
020100     05  PRIMARY-DIAG-CODE-W      PIC X(5).                       111111JS
020200     05  WARD-NBR                PIC X(4).                        02020001
020300         88  INTENSIVE-CARE  VALUE "0010".                        02030001
020400         88  OBSTETRICS      VALUE "2010".                        02040001
020500         88  PEDIATRICS      VALUE "1010".                        02050001
020600         88  ONCOLOGY        VALUE "0011".                        02060001
020700         88  CARDIO-THORACIC VALUE "0110".                        02070001
020800         88  GENERAL         VALUE "0000".                        02080001
020900         88  VALID-WARD VALUES ARE                                02090001
021000         "0010", "2010", "1010", "0011", "0110", "0000".          02100001
021100     05  ADDITIONAL-EQUIP-CHARGES OCCURS 12 TIMES.                02110001
021200         10  EQUIPMENT-ID            PIC X(08).                   02120001
021300         10  EQUIPMENT-CATEGORY      PIC X(04).                   02130001
021400             88 HEATING-PAD   VALUE "HEAT".                       02140001
021500             88 AUTOCLAVE     VALUE "AUTO".                       02150001
021600             88 SCOPE         VALUE "SCOP".                       02160001
021700             88 DRIP          VALUE "DRIP".                       02170001
021800             88 MONITOR       VALUE "MON ".                       02180001
021900             88 SHUNT         VALUE "SHNT".                       02190001
022000             88 MISCELLANEOUS VALUE "MISC".                       02200001
022100             88 VALID-CATEGORY VALUES ARE "HEAT", "AUTO",         02210001
022200                "SCOP", "DRIP", "MON ", "SHNT", "MISC".           02220001
022300         10  EQUIPMENT-SHORT-DESC    PIC X(30).                   02230001
022400         10  EQUIPMENT-COST          PIC 9(5)V99.                 02240001
022500         10  EQUIPMENT-PRES-PHYS     PIC X(08).                   02250001
022600         10  EQUIPMENT-REASON-CDE    PIC X(04).                   02260001
022700     05  DAILY-CHARGES-COMMENTS      PIC X(255).                  02270001
022800                                                                  02280001
022900 01  WS-HDR-REC.                                                  02290001
023000     05  FILLER                  PIC X(1) VALUE " ".              02300001
023100     05  HDR-DATE.                                                02310001
023200         10  HDR-YY              PIC 9(4).                        02320001
023300         10  DASH-1              PIC X(1) VALUE "-".              02330001
023400         10  HDR-MM              PIC 9(2).                        02340001
023500         10  DASH-2              PIC X(1) VALUE "-".              02350001
023600         10  HDR-DD              PIC 9(2).                        02360001
023700     05  FILLER                  PIC X(20) VALUE SPACE.           02370001
023800     05  FILLER                  PIC X(65) VALUE                  02380001
023900     "HOSPITAL WARDS / ROOMS / BEDS AVAILABILITY REPORT".         02390001
024000     05  FILLER         PIC X(14)                                 02400001
024100                   VALUE "Page Number:" Justified Right.          02410001
024200     05  PAGE-NBR-O              PIC ZZ9.                         02420001
024300                                                                  02430001
024400 01  WS-TRAILER-REC.                                              02440001
024500     05  FILLER                  PIC X(1).                        02450001
024600     05  IN-RECORD-COUNT         PIC 9(9).                        02460001
024700     05  FILLER                  PIC X(1).                        02470001
024800     05  IN-TOTAL-ROOM-CHARGE    PIC S9(9)V99.                    02480001
024900     05  IN-BASE-ROOM-CHARGE     PIC S9(9)V99.                    02490001
025000     05  IN-EQUIPMENT-CHARGES    PIC S9(9)V99.                    02500001
025100                                                                  02510001
025200 01  WS-BLANK-LINE.                                               02520001
025300     05  FILLER     PIC X(130) VALUE SPACES.                      02530001
025400                                                                  02540001
025500 01  WS-WARD-RPT-REC.                                             02550001
025600     05  FILLER     PIC X(1) VALUE SPACES.                        02560001
025700     05  FILLER     PIC X(09) VALUE "WARD-ID:".                   02570001
025800     05  WARD-O               PIC X(8).                           02580001
025900     05  FILLER     PIC X(19) VALUE "PRIMARY PHYSICIAN:".         02590001
026000     05  PHYS-O               PIC X(12).                          02600001
026100     05  FILLER     PIC X(18) VALUE "NURSE SUPERVISOR:".          02610001
026200     05  NURSE-O              PIC X(12).                          02620001
026300     05  FILLER     PIC X(16) VALUE "NUMBER OF BEDS:".            02630001
026400     05  BEDS-O           PIC ZZZ9.                               02640001
026500     05  FILLER     PIC X(22) VALUE "    BASE ROOM CHARGE:".      02650001
026600     05  ROOM-CHARGE-O       PIC $,$$9.99.                        02660001
026700     05  FILLER     PIC X(50) VALUE SPACES.                       02670001
026800                                                                  02680001
026900 01  WS-PATIENT-RPT.                                              02690001
027000     05  FILLER     PIC X(2) VALUE SPACES.                        02700001
027100     05  FILLER     PIC X(10) VALUE "ROOM-NBR:".                  02710001
027200     05  ROOM-PO               PIC X(6).                          02720001
027300     05  FILLER     PIC X(12) VALUE " ROOM TYPE:".                02730001
027400     05  ROOM-PTYPE            PIC X(14).                         02740001
027500     05  FILLER     PIC X(17) VALUE " NUMBER OF BEDS:".           02750001
027600     05  BEDS-PO           PIC Z99.                               02760001
027700     05  FILLER     PIC X(23) VALUE "  SPECIAL EQUIPMENT:".       02770001
027800     05  SPECIAL-EQUIP-PO  PIC X(60).                             02780001
027900                                                                  02790001
028000 01  WS-ROOM-RPT-REC.                                             02800001
028100     05  FILLER     PIC X(2) VALUE SPACES.                        02810001
028200     05  FILLER     PIC X(10) VALUE "ROOM-NBR:".                  02820001
028300     05  ROOM-O               PIC X(6).                           02830001
028400     05  FILLER     PIC X(12) VALUE " ROOM TYPE:".                02840001
028500     05  ROOM-TYPE            PIC X(14).                          02850001
028600     05  FILLER     PIC X(17) VALUE " NUMBER OF BEDS:".           02860001
028700     05  BEDS-O           PIC Z99.                                02870001
028800     05  FILLER     PIC X(23) VALUE "  SPECIAL EQUIPMENT:".       02880001
028900     05  SPECIAL-EQUIP-O  PIC X(60).                              02890001
029000                                                                  02900001
029100 01  WS-BED-PATIENT-DETAIL.                                       02910001
029200     05  FILLER     PIC X(4) VALUE SPACES.                        02920001
029300     05  FILLER     PIC X(14) VALUE "PATIENT NAME:".              02930001
029400     05  PAT-NAME.                                                02940001
029500        15  LAST-NAME-O   PIC X(10).                              02950001
029600        15  FILLER        PIC X(1) VALUE SPACES.                  02960001
029700        15  MIDINIT-O     PIC X(1).                               02970001
029800        15  FILLER        PIC X(1) VALUE SPACES.                  02980001
029900        15  FIRST-NAME-O  PIC X(10).                              02990001
030000     05  FILLER     PIC X(11) VALUE "  BED-NBR:".                 03000001
030100     05  BED-O      PIC X(4).                                     03010001
030200     05  FILLER     PIC X(14) VALUE "  ADMIT DATE:".              03020001
030300     05  ADMIT-DATE-O   PIC X(10).                                03030001
030400     05  FILLER     PIC X(13) VALUE "  DIAGNOSIS:".               03040001
030500     05  DIAGNOSIS-O  PIC X(7).                                   03050001
030600     05  FILLER     PIC X(11) VALUE "COMMENTS:".                  03060001
030700     05  DAILY-COMMENTS-O           PIC X(50).                    03070001
030800     05  INPUT-FILE-ERROR-MSG       PIC X(50).                    03080001
030800     05  HEX-VAL                 PIC X(1) VALUE ''.
030900                                                                  03090001
031000 01  COUNTERS-AND-ACCUMULATORS.                                   03100001
031100     05 WS-MEDICATION-CHARGES    PIC S9(9)V99 COMP-3.             03110001
031200     05 WS-PHARMACY-CHARGES      PIC S9(7)V99 COMP-3.             03120001
031300     05 WS-ANCILLARY-CHARGES     PIC S9(5)V99 COMP-3.             03130001
031400                                                                  03140001
031500                                                                  03150001
031600 COPY BNCHMSTR.                                                   03160001
031700** VSAM FILE                                                      03170001
031800                                                                  03180001
031900 01  WS-SYSOUT-REC.                                               03190001
032000     05  MSG                     PIC X(80).                       03200001
032100                                                                  03210001
032200 01  WS-CURRENT-DATE-FIELDS.                                      03220001
032300       05  WS-CURRENT-DATE.                                       03230001
032400           10  WS-CURRENT-YEAR    PIC  9(4).                      03240001
032500           10  WS-CURRENT-MONTH   PIC  9(2).                      03250001
032600           10  WS-CURRENT-DAY     PIC  9(2).                      03260001
032700       05  WS-CURRENT-TIME.                                       03270001
032800           10  WS-CURRENT-HOUR    PIC  9(2).                      03280001
032900           10  WS-CURRENT-MINUTE  PIC  9(2).                      03290001
033000           10  WS-CURRENT-SECOND  PIC  9(2).                      03300001
033100           10  WS-CURRENT-MS      PIC  9(2).                      03310001
033200       05  WS-DIFF-FROM-GMT       PIC S9(4).                      03320001
033300                                                                  03330001
033400                                                                  03340001
033500 01  COUNTERS-IDXS-AND-ACCUMULATORS.                              03350001
033600     05 RECORDS-WRITTEN          PIC 9(7) COMP.                   03360001
033700     05 PAT-WS-RECORDS-IN-ERROR     PIC 9(7) COMP.                03370001
033800     05 PAT-WS-INFILE-RECORD-READ-CTR         PIC 9(9) COMP.      03380001
033900     05 WS-BASE-ROOM-CHARGE      PIC S9(9)V99 COMP-3.             03390001
034000     05 WS-TOTAL-ROOM-CHARGE     PIC S9(9)V99 COMP-3.             03400001
034100     05 WS-EQUIPMENT-COST        PIC S9(7)V99 COMP-3.             03410001
034200     05 HOLD-WARD-ID             PIC 9(4) VALUE 0.                03420001
034300     05 WS-RECORDS-IN-ERROR      PIC 9(5) VALUE 0.                03430001
034400     05 HOLD-ROOM-NBR            PIC 9(4) VALUE 0.                03440001
034500     05 ROW-SUB                  PIC 9(2) VALUE 0.                03450001
034600     05 WS-LINES                 PIC 9(03) VALUE 0.               03460001
034700     05 WS-PAGES                 PIC 9(03) VALUE 1.               03470001
034800     05 TRLR-REC-SW              PIC 9(01) VALUE 0.               03480001
034900        88 TRLR-REC-FOUND        VALUE 1.                         03490001
035000                                                                  03500001
035100 01  MISC-WS-FLDS.                                                03510001
035200     05 STR-LTH                  PIC 9(04) VALUE 0.               03520001
035300     05 RETURN-CD                PIC S9(04) VALUE 0.              03530001
035400     05 TABLE-SIZE               PIC 9(02) VALUE 12.              03540001
035500     05 MORE-TABLE-ROWS          PIC X(01).                       03550001
035600        88 MORE-TABLE-ROWS     VALUE "Y".                         03560001
035700        88 NO-MORE-TABLE-ROWS  VALUE "N".                         03570001
035800                                                                  03580001
035900 01  FLAGS-AND-SWITCHES.                                          03590001
036000     05 MORE-WARD-DATA-SW          PIC X(01) VALUE "Y".           03600001
036100         88 NO-MORE-PATIENTS VALUE "N".                           03610001
036200         88 MORE-PATIENTS VALUE "Y".                              03620001
036300     05 ERROR-FOUND-SWT           PIC X(01) VALUE "Y".            03630001
036400     05 WS-ERROR-FOUND-SWITCH         PIC X(01) VALUE "Y".        03640001
036500         88 RECORD-ERROR-FOUND VALUE "Y".                         03650001
036600         88 VALID-RECORD  VALUE "N".                              03660001
036700     05 FIRST-TIME-IN-SW           PIC X(01) VALUE "Y".           03670001
036800         88 FIRST-TREATMENT-READ VALUE "Y".                       03680001
036900         88 NOT-FIRST-TIME  VALUE "N".                            03690001
037000     05 WARD-SW           PIC X(01) VALUE "N".                    03700001
037100         88 NEW-WARD VALUE "Y".                                   03710001
037200     05 ROOM-SW           PIC X(01) VALUE "N".                    03720001
037300         88 NEW-ROOM VALUE "Y".                                   03730001
037400                                                                  03740001
037500 COPY BNCHPRSN.                                                   03750001
037600 COPY ABENDREC.                                                   03760001
037700 COPY BNCHINS.                                                    03770001
037800 EXEC SQL INCLUDE SQLCA END-EXEC.                                 03780001
037900       EXEC SQL INCLUDE DIAGCODE END-EXEC.                        03790001
038000 EXEC SQL INCLUDE MEDICATN END-EXEC.                              03800001
038100** QSAM FILE                                                      03810001
038200                                                                  03820001
038300* COPY DIAGCODE.                                                  03830001
038400******************************************************************03840001
038500***** DB2 TABLE DCLGENS                                           03850001
038600 01  DCLWARD-CODES.                                               03860001
038700     10 WARD-ID                        PIC X(04).                 03870001
038800     10 PRIMARY-PHYSICIAN-ID           PIC X(08).                 03880001
038900     10 SUPERVISE-NURSE-ID             PIC X(08).                 03890001
039000     10 LOCATION                       PIC X(08).                 03900001
039100     10 NUMBER-OF-BEDS                 PIC S9(4) COMP.            03910001
039200     10 BASE-ROOM-CHARGE               PIC S9(5)V99 COMP-3.       03920001
039300                                                                  03930001
039400 01  DCLHOSP-BED.                                                 03940001
039500     10 BED-ID                         PIC X(04).                 03950001
039600     10 ROOM-ID                        PIC X(08).                 03960001
039700     10 WARD-ID                        PIC X(08).                 03970001
039800     10 SPECIAL-CHARGES                PIC S9(5)V99 COMP-3.       03980001
039900                                                                  03990001
040000 01  DCLROOM-DATA.                                                04000001
040100     10 WARD-ID                        PIC X(04).                 04010001
040200     10 ROOM-ID                        PIC X(08).                 04020001
040300     10 PRIVATE                        PIC S9(4) COMP.            04030001
040400     10 SEMI-PRIVATE                   PIC S9(4) COMP.            04040001
040500     10 NUMBER-OF-BEDS                 PIC S9(4) COMP.            04050001
040600     10 SPECIAL-EQUIPMENT              PIC X(254).                04060001
040700                                                                  04070001
040800 PROCEDURE DIVISION.                                              04080001
040900     PERFORM 000-HOUSEKEEPING THRU 000-EXIT.                      04090001
041000     PERFORM 100-MAINLINE THRU 100-EXIT                           04100001
041100             UNTIL NO-MORE-PATIENTS OR                            04110001
041200******* Balancing logic put in by TGD 02/12/92                    04120001
041300             TRAILER-REC.                                         04130001
041400     PERFORM 999-CLEANUP THRU 999-EXIT.                           04140001
041500     MOVE +0 TO RETURN-CODE.                                      04150001
041600     GOBACK.                                                      04160001
041700                                                                  04170001
041800 000-HOUSEKEEPING.                                                04180001
041900     MOVE "000-HOUSEKEEPING" TO PARA-NAME.                        04190001
042000     DISPLAY "HOUSEKEEPING".                                      04200001
042100*  DATE VALUES                                                    04210001
042200     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.        04220001
042300     MOVE WS-CURRENT-YEAR  TO HDR-YY.                             04230001
042400     MOVE WS-CURRENT-MONTH  TO HDR-MM.                            04240001
042500     MOVE WS-CURRENT-DAY  TO HDR-DD.                              04250001
042600                                                                  04260001
042700     INITIALIZE COUNTERS-IDXS-AND-ACCUMULATORS, WS-TRAILER-REC,   04270001
042800      INPATIENT-TREATMENT-REC-DATA.                               04280001
042900     MOVE +1 TO WS-LINES, WS-PAGES.                               04290001
043000     PERFORM 800-OPEN-FILES THRU 800-EXIT.                        04300001
043100     PERFORM 900-READ-WARD-DATA THRU 900-EXIT.                    04310001
043200                                                                  04320001
043300     IF NO-MORE-PATIENTS                                          04330001
043400         MOVE "EMPTY PATIENT INPUT FILE" TO ABEND-REASON          04340001
043500         GO TO 1000-ABEND-RTN.                                    04350001
043600

      * TODO check out the TRLR-REC logic
043700**** PUT IN TO HANDLE NEW SORT REQUIREMENTS                       04370001
043800     IF TRAILER-REC                                               04380001
043900         MOVE 1 TO TRLR-REC-SW                                    04380101
044000         PERFORM 900-READ-WARD-DATA THRU 900-EXIT.                04380201
044100                                                                  04380301
044200 000-EXIT.                                                        04380401
044300     EXIT.                                                        04380501
044400                                                                  04380601
044500 100-MAINLINE.                                                    04380701
044600     MOVE "100-MAINLINE" TO PARA-NAME.                            04380801
044700     IF WARD-NBR IN INPATIENT-DAILY-REC NOT = HOLD-WARD-ID        04380901
044800         PERFORM 200-NEW-WARD THRU 200-EXIT                       04381001
044900         PERFORM 300-NEW-ROOM THRU 300-EXIT                       04382001
045000         PERFORM 400-NEW-PATIENT THRU 400-EXIT                    04383001
045100         MOVE WARD-NBR IN INPATIENT-DAILY-REC TO HOLD-WARD-ID     04384001
045200         MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC                04385001
045300                          TO HOLD-ROOM-NBR                        04386001
045400     ELSE                                                         04387001
045500     IF ROOM-IDENTITY IN INPATIENT-DAILY-REC                      04388001
045600                      NOT = HOLD-ROOM-NBR                         04389001
045700         PERFORM 300-NEW-ROOM THRU 300-EXIT                       04390001
045800         PERFORM 300-FIELD-EDITS THRU 300-FIELD-EXIT              04400001
045900         PERFORM 400-NEW-PATIENT THRU 400-EXIT                    04410001
046000         MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC                04420001
046100                      TO HOLD-ROOM-NBR                            04430001
046200     ELSE                                                         04440001
046300         PERFORM 400-NEW-PATIENT THRU 400-EXIT.                   04450001
046400                                                                  04460001
046500     PERFORM 900-READ-WARD-DATA THRU 900-EXIT.                    04470001
046600                                                                  04480001
046700 100-EXIT.                                                        04490001
046800     EXIT.                                                        04500001
046900                                                                  04510001
047000 200-NEW-WARD.                                                    04520001
047100     MOVE "200-NEW-WARD" TO PARA-NAME.                            04530001
047200     MOVE "N" TO ERR-FOUND-SWITCH IN FLAGS-AND-SWITCHES.          04540001
047300                                                                  04550001
047400     MOVE WARD-NBR IN INPATIENT-DAILY-REC TO                      04560001
047500        WARD-ID IN DCLWARD-CODES,                                 04570001
047600        WARD-ID IN DCLROOM-DATA.                                  04580001
047700***     WARD-ID IN DCLHOSP-BED. ??                                04590001
047800                                                                  04600001
047900     PERFORM 250-PROCESS-WARD-TABLE-DATA  THRU 250-EXIT.          04610001
048000*** SET UP PAGE HEADERS                                           04620001
048100     PERFORM 700-WRITE-PAGE-HDR    THRU 700-EXIT.                 04630001
048200     PERFORM 720-WRITE-WARD-RPT    THRU 720-EXIT.                 04640001
048300                                                                  04650001
048400***PROCESS PATIENT TREATMENTS                                     04660001
048500     PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.                    04670001
048600 200-EXIT.                                                        04680001
048700     EXIT.                                                        04690001
048800                                                                  04700001
048900 250-PROCESS-WARD-TABLE-DATA.                                     04710001
049000*    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.              04720001
049100     EXEC SQL                                                     04730001
049200       SELECT PRIMARY_PHYSICIAN_ID,                               04740001
049300              SUPERVISE_NURSE_ID,                                 04750001
049400              LOCATION,                                           04760001
049500              NUMBER_OF_BEDS,                                     04770001
049600              BASE_ROOM_CHARGE                                    04780001
049700       INTO                                                       04790001
049800              :PRIMARY-PHYSICIAN-ID,                              04800001
049900              :SUPERVISE-NURSE-ID,                                04810001
050000              :LOCATION,                                          04820001
050100              :DCLWARD-CODES.NUMBER-OF-BEDS,                      04830001
050200              :DCLWARD-CODES.BASE-ROOM-CHARGE                     04840001
050300       FROM DDS0001.WARD_DATA                                     04850001
050400       WHERE WARD_ID = :DCLWARD-CODES.WARD-ID                     04860001
050500     END-EXEC.                                                    04870001
050600                                                                  04880001
050700     IF SQLCODE = -811 OR 0                                       04890001
050800         NEXT SENTENCE                                            04900001
050900     ELSE                                                         04910001
051000     IF SQLCODE = +100                                            04920001
051100         MOVE "*** PATIENT WARD DATA IN ERROR" TO                 04930001
051200         ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR             04940001
051300         MOVE "Y" TO ERR-FOUND-SWITCH                             04950001
051400         move sqlcode to  EXPECTED-VAL                            04960001
051500         move PATIENT-ID IN INPATIENT-DAILY-REC                   04970001
051600                         to ACTUAL-VAL                            04980001
051700         WRITE SYSOUT-REC FROM ABEND-REC                          04990001
051800         GO TO 250-EXIT                                           05000001
051900     ELSE                                                         05010001
052000     IF SQLCODE < 0                                               05020001
052100         MOVE "*** FATAL DB2 ERROR" TO                            05030001
052200         ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR             05040001
052300         MOVE "Y" TO ERR-FOUND-SWITCH                             05050001
052400         move sqlcode to  EXPECTED-VAL                            05060001
052500         move PATIENT-ID IN INPATIENT-DAILY-REC                   05070001
052600                         to ACTUAL-VAL                            05080001
052700         WRITE SYSOUT-REC FROM ABEND-REC                          05090001
052800         GO TO 1000-DB2-ERROR-RTN.                                05100001
052900                                                                  05110001
053000     MOVE WARD-ID IN DCLWARD-CODES TO WARD-O.                     05120001
053100     MOVE PRIMARY-PHYSICIAN-ID IN DCLWARD-CODES TO PHYS-O.        05130001
053200     MOVE SUPERVISE-NURSE-ID TO NURSE-O.                          05140001
053300     MOVE NUMBER-OF-BEDS IN DCLWARD-CODES                         05150001
053400                              TO BEDS-O IN WS-WARD-RPT-REC.       05160001
053500     MOVE BASE-ROOM-CHARGE IN DCLWARD-CODES TO ROOM-CHARGE-O.     05170001
053600     PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-N-EXIT.             05180001
053700 250-EXIT.                                                        05190001
053800     EXIT.                                                        05200001
053900                                                                  05210001
054000 300-NEW-ROOM.                                                    05220001
054100     MOVE "300-NEW-ROOM" TO PARA-NAME.                            05230001
054200     MOVE "N" TO ERR-FOUND-SWITCH IN FLAGS-AND-SWITCHES.          05240001
054300                                                                  05250001
054400     MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC TO                 05260001
054500        ROOM-ID IN DCLHOSP-BED,                                   05270001
054600        ROOM-ID  IN DCLROOM-DATA.                                 05280001
054700***     WARD-ID IN DCLHOSP-BED. ??                                05290001
054800                                                                  05300001
054900     PERFORM 350-PROCESS-ROOM-TABLE-DATA  THRU 350-EXIT.          05310001
055000*** SET UP PAGE HEADERS                                           05320001
055100     PERFORM 740-WRITE-ROOM-RPT   THRU 740-EXIT.                  05330001
055200     PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.                    05340001
055300 300-EXIT.                                                        05350001
055400     EXIT.                                                        05360001
055500                                                                  05370001
055600 300-FIELD-EDITS.                                                 05380001
055700     MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.            05390001
055800     MOVE "300-FIELD-EDITS" TO PARA-NAME.                         05400001
055900******** non-numeric fields                                       05410001
056000     IF NOT VALID-BILLABLE-TYPES IN BILLABLE-TREATMENT-IND        05420001
056100        MOVE "*** INVALID BILLABLE TYPE" TO                       05430001
056200        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05440001
056300        MOVE "Y" TO ERROR-FOUND-SW                                05450001
056400        GO TO 300-FIELD-EXIT.                                     05460001
056500                                                                  05470001
056600     IF NOT VALID-TRTMNT-MODES IN TREATMENT-MODE                  05480001
056700        MOVE "*** INVALID TREATMENT MODE" TO                      05490001
056800        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05500001
056900        MOVE "Y" TO ERROR-FOUND-SW                                05510001
057000        GO TO 300-FIELD-EXIT.                                     05520001
057100                                                                  05530001
057200     IF PATIENT-ID IN INPATIENT-TREATMENT-REC NOT NUMERIC         05540001
057300        MOVE "*** NON-NUMERIC PATIENT-ID" TO                      05550001
057400        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05560001
057500        MOVE "Y" TO ERROR-FOUND-SW                                05570001
057600        GO TO 300-FIELD-EXIT.                                     05580001
057700                                                                  05590001
057800     IF PATIENT-ID IN INPATIENT-TREATMENT-REC = ZERO              05600001
057900        MOVE "*** INVALID (000000) PATIENT-ID" TO                 05610001
058000        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05620001
058100        MOVE "Y" TO ERROR-FOUND-SW                                05630001
058200        GO TO 300-FIELD-EXIT.                                     05640001
058300                                                                  05650001
058400     IF BED-IDENTITY IN INPATIENT-TREATMENT-REC NOT NUMERIC       05660001
058500        MOVE "*** NON-NUMERIC BED-IDENTITY" TO                    05670001
058600        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05680001
058700        MOVE "Y" TO ERROR-FOUND-SW                                05690001
058800        GO TO 300-FIELD-EXIT.                                     05700001
058900                                                                  05710001
059000     IF MEDICATION-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC    05720001
059100        MOVE "*** NON-NUMERIC MEDICATION-COST" TO                 05730001
059200        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05740001
059300        MOVE "Y" TO ERROR-FOUND-SW                                05750001
059400        GO TO 300-FIELD-EXIT.                                     05760001
059500                                                                  05770001
059600     IF PHARMACY-COST IN INPATIENT-TREATMENT-REC NOT NUMERIC      05780001
059700        MOVE "*** NON-NUMERIC PHARMACY COSTS" TO                  05790001
059800        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05800001
059900        MOVE "Y" TO ERROR-FOUND-SW                                05810001
060000        GO TO 300-FIELD-EXIT.                                     05820001
060100                                                                  05830001
060200     IF ANCILLARY-CHARGE IN INPATIENT-TREATMENT-REC NOT NUMERIC   05840001
060300        MOVE "*** NON-NUMERIC ANCILLARY-CHARGES" TO               05850001
060400        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05860001
060500        MOVE "Y" TO ERROR-FOUND-SW                                05870001
060600        GO TO 300-FIELD-EXIT.                                     05880001
060700                                                                  05890001
060800     IF ATTENDING-PHYS-ID = SPACES                                05900001
060900        MOVE "*** BLANK ATTENDING PHYSICIAN-ID" TO                05910001
061000        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05920001
061100        MOVE "Y" TO ERROR-FOUND-SW                                05930001
061200        GO TO 300-FIELD-EXIT.                                     05940001
061300                                                                  05950001
061400     IF PRESCRIBING-PHYS-ID = SPACES                              05960001
061500        MOVE "*** BLANK PRESCRIBING PHYSICIAN-ID" TO              05970001
061600        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    05980001
061700        MOVE "Y" TO ERROR-FOUND-SW                                05990001
061800        GO TO 300-FIELD-EXIT.                                     06000001
061900                                                                  06010001
062000     CALL 'DTEVAL' USING TREATMENT-DATE, RETURN-CD.               06020001
062100     IF RETURN-CD < 0                                             06030001
062200        MOVE "*** BAD DATE PORTION OF DATE-TIME" TO               06040001
062300        ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                    06050001
062400        MOVE "Y" TO ERROR-FOUND-SW                                06060001
062500        GO TO 300-FIELD-EXIT.                                     06070001
062600                                                                  06080001
062700     IF VALID-RECORD IN ERROR-FOUND-SW                            06090001
062800         PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-EXIT.           06100001
062900                                                                  06110001
063000****** VERIFY TABLE (JUST TYPES AND LAB-TEST-ID)                  06120001
063100                                                                  06130001
063200 300-FIELD-EXIT.                                                  06140001
063300     EXIT.                                                        06150001
063400                                                                  06160001
063500 350-PROCESS-ROOM-TABLE-DATA.                                     06170001
063600     PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-N-EXIT.             06180001
063700*    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.              06190001
063800     EXEC SQL                                                     06200001
063900       SELECT PRIVATE,                                            06210001
064000              SEMI_PRIVATE,                                       06220001
064100              NUMBER_OF_BEDS,                                     06230001
064200              SPECIAL_EQUIPMENT                                   06240001
064300       INTO                                                       06250001
064400              :DCLROOM-DATA.PRIVATE,                              06260001
064500              :DCLROOM-DATA.SEMI-PRIVATE,                         06270001
064600              :DCLROOM-DATA.NUMBER-OF-BEDS,                       06280001
064700              :DCLROOM-DATA.SPECIAL-EQUIPMENT                     06290001
064800       FROM DDS0001.ROOM_DATA                                     06300001
064900       WHERE WARD_ID = :DCLROOM-DATA.WARD-ID                      06310001
065000       AND   ROOM_ID = :DCLROOM-DATA.ROOM-ID                      06320001
065100     END-EXEC.                                                    06330001
065200                                                                  06340001
065300     IF SQLCODE = -811 OR 0                                       06350001
065400         NEXT SENTENCE                                            06360001
065500     ELSE                                                         06370001
065600     IF SQLCODE = +100                                            06380001
065700         MOVE "*** PATIENT ROOM DATA IN ERROR" TO                 06390001
065800         ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR             06400001
065900         MOVE "Y" TO ERROR-FOUND-SW                               06410001
066000         move sqlcode to  EXPECTED-VAL                            06420001
066100         move PATIENT-ID IN INPATIENT-DAILY-REC                   06430001
066200                         to ACTUAL-VAL                            06440001
066300         WRITE SYSOUT-REC FROM ABEND-REC                          06450001
066400         GO TO 350-EXIT                                           06460001
066500     ELSE                                                         06470001
066600     IF SQLCODE < 0                                               06480001
066700         MOVE "*** FATAL DB2 ERROR" TO                            06490001
066800         ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR             06500001
066900         MOVE "Y" TO ERROR-FOUND-SW                               06510001
067000         move sqlcode to  EXPECTED-VAL                            06520001
067100         move PATIENT-ID IN INPATIENT-DAILY-REC                   06530001
067200                         to ACTUAL-VAL                            06540001
067300         WRITE SYSOUT-REC FROM ABEND-REC                          06550001
067400         GO TO 1000-DB2-ERROR-RTN.                                06560001
067500                                                                  06570001
067600     MOVE ROOM-ID IN DCLROOM-DATA TO ROOM-O.                      06580001
067700     IF PRIVATE IN DCLROOM-DATA = 1                               06590001
067800        MOVE "PRIVATE" TO ROOM-TYPE                               06600001
067900     ELSE                                                         06610001
068000     IF SEMI-PRIVATE = 1                                          06620001
068100        MOVE "SEMI-PRIVATE" TO ROOM-TYPE                          06630001
068200     ELSE                                                         06640001
068300        MOVE "SPECIAL-NEEDS" TO ROOM-TYPE.                        06650001
068400                                                                  06660001
068500     MOVE WARD-ID IN DCLWARD-CODES TO PHYS-O.                     06670001
068600     MOVE SUPERVISE-NURSE-ID TO NURSE-O.                          06680001
068700     MOVE NUMBER-OF-BEDS IN DCLWARD-CODES                         06690001
068800                            TO BEDS-O IN WS-ROOM-RPT-REC.         06700001
068900     MOVE SPECIAL-EQUIPMENT IN DCLROOM-DATA TO SPECIAL-EQUIP-O.   06710001
069000     PERFORM 700-WRITE-TRMTEDIT THRU 700-EXIT.                    06720001
069100 350-EXIT.                                                        06730001
069200     EXIT.                                                        06740001
069300                                                                  06750001
069400 400-NUMERIC-RANGE-EDITS.                                         06760001
069500     MOVE "400-NUMERIC-RANGE-EDITS" TO PARA-NAME.                 06770001
069600******** Call to VSAM file to read record                         06780001
069700     IF  (MEDICATION-COST > 99000                                 06790001
069800     OR  MEDICATION-COST < 1.01)                                  06800001
069900         MOVE "*** INVALID MEDICATION COST" TO                    06810001
070000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06820001
070100         MOVE "Y" TO ERROR-FOUND-SW                               06830001
070200         GO TO 400-N-EXIT.                                        06840001
070300                                                                  06850001
070400                                                                  06860001
070500     IF  (PHARMACY-COST IN INPATIENT-TREATMENT-REC > 990          06870001
070600     OR  PHARMACY-COST IN INPATIENT-TREATMENT-REC < .99)          06880001
070700         MOVE "*** INVALID PHARMACY COSTS" TO                     06890001
070800         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06900001
070900         MOVE "Y" TO ERROR-FOUND-SW                               06910001
071000         GO TO 400-N-EXIT.                                        06920001
071100                                                                  06930001
071200                                                                  06940001
071300     IF  (ANCILLARY-CHARGE > 900                                  06950001
071400     OR  ANCILLARY-CHARGE < 1.01)                                 06960001
071500         MOVE "*** INVALID ANCILLARY CHARGES" TO                  06970001
071600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   06980001
071700         MOVE "Y" TO ERROR-FOUND-SW                               06990001
071800         GO TO 400-N-EXIT.                                        07000001
071900                                                                  07010001
072000     IF  (SSN IN RESPONSIBLE-PARTY > "999999999"                  07020001
072100     OR  SSN IN RESPONSIBLE-PARTY < "0000000001")                 07030001
072200         MOVE "*** INVALID SOCIAL SECURITY #" TO                  07040001
072300         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   07050001
072400         MOVE "Y" TO ERROR-FOUND-SW                               07060001
072500         GO TO 400-EXIT.                                          07070001
072600                                                                  07080001
072700                                                                  07090001
072800     IF  (HOSPITAL-STAY-LTH > 365                                 07100001
072900     OR  HOSPITAL-STAY-LTH < 1)                                   07110001
073000         MOVE "*** INVALID HOSPITAL STAY LTH" TO                  07120001
073100         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   07130001
073200         MOVE "Y" TO ERROR-FOUND-SW                               07140001
073300         GO TO 400-EXIT.                                          07150001
073400                                                                  07160001
073500     IF  (EQUIPMENT-CHARGES(ROW-SUB) > 999.00                     07170001
073600     OR  EQUIPMENT-CHARGES(ROW-SUB) < 99.99)                      07180001
073700         MOVE "*** INVALID EQUIPMENT CHARGES" TO                  07190001
073800         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   07200001
073900         MOVE "Y" TO ERROR-FOUND-SW                               07210001
074000         GO TO 400-EXIT.                                          07220001
074100                                                                  07230001
074200                                                                  07240001
074300     IF  (EXP-MONTH > 12                                          07250001
074400     OR  EXP-MONTH < 1)                                           07260001
074500         MOVE "*** INVALID CREDIT-CARD EXP. DATE" TO              07270001
074600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   07280001
074700         MOVE "Y" TO ERROR-FOUND-SW                               07290001
074800         GO TO 400-EXIT.                                          07300001
074900                                                                  07310001
075000     IF VALID-RECORD IN ERROR-FOUND-SW                            07320001
075100         PERFORM 450-CROSS-FIELD-EDITS THRU 450-FIELD-EXIT.       07330001
075200                                                                  07340001
075300     IF VALID-RECORD IN ERROR-FOUND-SW                            07350001
075400         PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.              07360001
075500                                                                  07370001
075600 400-N-EXIT.                                                      07380001
075700     EXIT.                                                        07390001
075800                                                                  07400001
075900 400-NEW-PATIENT.                                                 07410001
076000     MOVE "400-NEW-PATIENT" TO PARA-NAME.                         07420001
076100     MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.            07430001
076200                                                                  07440001
076300     MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO                    07450001
076400     PATMSTR-KEY, PATPERSN-KEY.                                   07460001
076500                                                                  07470001
076600     PERFORM 450-GET-PATIENT-DATA THRU 450-EXIT.                  07480001
076700*** SET UP PAGE HEADERS                                           07490001
076800     PERFORM 760-WRITE-PATIENT-RPT THRU 760-EXIT.                 07500001
076900                                                                  07510001
077000 400-EXIT.                                                        07520001
077100     EXIT.                                                        07530001
077200                                                                  07540001
077300 450-GET-PATIENT-DATA.                                            07550001
077400*    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.              07560001
077500                                                                  07570001
077600     MOVE DAILY-CHARGES-COMMENTS TO DAILY-COMMENTS-O.             07580001
077700                                                                  07590001
077800     READ PATMSTR.                                                07600001
077900     IF PATMSTR-FOUND                                             07610001
078000        MOVE PATMSTR-REC TO PATIENT-MASTER-REC                    07620001
078100        MOVE DATE-ADMIT TO ADMIT-DATE-O                           07630001
078200        MOVE DIAGNOSTIC-CODE-PRIMARY TO DIAGNOSIS-O               07640001
078300        MOVE BED-IDENTITY-PRIMARY TO BED-O                        07650001
078400     ELSE                                                         07660001
078500        MOVE "PATIENT NOT FOUND IN PATMASTR" TO ABEND-REASON      07670001
078600        MOVE "500-GET-PATIENT-DATA" TO PARA-NAME                  07680001
078700        MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO  EXPECTED-VAL   07690001
078800        GO TO 1000-ABEND-RTN.                                     07700001
078900                                                                  07710001
079000     READ PATPERSN.                                               07720001
079100     IF PATPERSN-FOUND                                            07730001
079200        MOVE PATPERSN-REC TO PATIENT-PERSONAL-MASTER-REC          07740001
079300        MOVE LAST-NAME TO LAST-NAME-O                             07750001
079400        MOVE MIDINIT TO MIDINIT-O                                 07760001
079500        MOVE DIAGNOSTIC-CODE-SECONDARY TO DIAGNOSIS-O             07770001
079600        MOVE FIRST-NAME TO FIRST-NAME-O                           07780001
079700     ELSE                                                         07790001
079800        MOVE "PATIENT NOT FOUND IN PATPERSN" TO ABEND-REASON      07800001
079900        MOVE "500-GET-PATIENT-DATA" TO PARA-NAME                  07810001
080000        MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO  EXPECTED-VAL   07820001
080100        MOVE PRIMARY-CARE-PHYSICIAN-ID TO DIAGNOSIS-O             07830001
080200        GO TO 1000-ABEND-RTN.                                     07840001
080300     MOVE PRIMARY-CARE-PHYSICIAN-ID TO PHYS-O IN WS-WARD-RPT-REC. 07850001
080400     MOVE SPACES TO EQUIP-DIAG-CODE (ROW-SUB).                    07860001
080500 450-EXIT.                                                        07870001
080600     EXIT.                                                        07880001
080700                                                                  07890001
080800 450-CROSS-FIELD-EDITS.                                           07900001
080900     MOVE "450-CROSS-FIELD-EDITS" TO PARA-NAME.                   07910001
081000******** Specific requirements for certain procedures             07920001
081100     IF  MRI OR CAT OR CHEMO-THERAPY OR RADIATION-THERAPY         07930001
081200         OR SURGERY OR LAB-TESTS                                  07940001
081300         IF MEDICATION-COST = ZERO OR                             07950001
081400            ANCILLARY-CHARGE = ZERO                               07960001
081500         MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO          07970001
081600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   07980001
081700         MOVE "Y" TO ERROR-FOUND-SW                               07990001
081800         GO TO 450-FIELD-EXIT.                                    08000001
081900                                                                  08010001
082000     IF  ORAL-ADMIN OR INTRAVENOUS-ADMIN OR INJECTION             08020001
082100         IF PHARMACY-COST IN INPATIENT-TREATMENT-REC = ZERO OR    08030001
082200            ANCILLARY-CHARGE = ZERO                               08040001
082300         MOVE "*** INVALID $$ AMOUNTS FOR PROCEDURES" TO          08050001
082400         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   08060001
082500         MOVE "Y" TO ERROR-FOUND-SW                               08070001
082600         GO TO 450-FIELD-EXIT.                                    08080001
082700                                                                  08090001
082800     IF  NOT OTHER-TREATMENT                                      08100001
082900         IF TREATMENT-NURSE-ID = SPACES OR                        08110001
083000            SUPERVISOR-NURSE-ID = SPACES                          08120001
083100         MOVE "*** INVALID NURSING ENTRIES" TO                    08130001
083200         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   08140001
083300         MOVE "Y" TO ERROR-FOUND-SW                               08150001
083400         GO TO 450-FIELD-EXIT.                                    08160001
083500                                                                  08170001
083600     IF  NOT (OTHER-TREATMENT AND LAB-TESTS)                      08180001
083700         IF TREATMENT-COMMENTS = SPACES                           08190001
083800         MOVE "*** INVALID TREATMENT COMMENTS" TO                 08200001
083900         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   08210001
084000         MOVE "Y" TO ERROR-FOUND-SW                               08220001
084100         GO TO 450-FIELD-EXIT.                                    08230001
084200                                                                  08240001
084300     IF  CHEMO-THERAPY OR RADIATION-THERAPY OR SURGERY            08250001
084400        MOVE +0 TO STR-LTH                                        08260001
084500        CALL 'STRLTH' USING TREATMENT-COMMENTS, STR-LTH           08270001
084600        IF STR-LTH < 25                                           08280001
084700         MOVE "*** INVALID TREATMENT COMMENT LENGTH" TO           08290001
084800         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   08300001
084900         MOVE "Y" TO ERROR-FOUND-SW                               08310001
085000         GO TO 450-FIELD-EXIT.                                    08320001
085100                                                                  08330001
085200     MOVE SPACES TO GROUP-NBR IN INS-COMPANY-PRIMARY.             08340001
085300     IF VALID-RECORD IN ERROR-FOUND-SW                            08350001
085400         PERFORM 500-CROSS-FILE-EDITS THRU 500-EXIT.              08360001
085500                                                                  08370001
085600 450-FIELD-EXIT.                                                  08380001
085700     EXIT.                                                        08390001
085800                                                                  08400001
085900 500-CROSS-FILE-EDITS.                                            08410001
086000     MOVE "500-CROSS-FILE-EDITS" TO PARA-NAME.                    08420001
086100******** Call to VSAM file to read record                         08430001
086200     MOVE PATIENT-ID IN INPATIENT-TREATMENT-REC TO                08440001
086300            PATMSTR-KEY.                                          08450001
086400     READ PATMSTR INTO PATIENT-MASTER-REC.                        08460001
086500     IF  NOT PATMSTR-FOUND IN PATMSTR-STATUS                      08470001
086600         MOVE "*** PATIENT NOT-FOUND ON MASTER FILE" TO           08480001
086700         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   08490001
086800         MOVE "Y" TO ERROR-FOUND-SW                               08500001
086900         GO TO 500-EXIT.                                          08510001
087000                                                                  08520001
087100     MOVE SPACES TO LAB-TEST-DATE(ROW-SUB).                       08530001
087200     MOVE SPACES TO GROUP-NBR IN INS-COMPANY-PRIMARY.             08540001
087300                                                                  08550001
087400     IF VALID-RECORD IN WS-ERROR-FOUND-SWITCH                     08560001
087500        PERFORM 600-DB2-TABLE-EDITS THRU 600-EXIT.                08570001
087600     MOVE PRIMARY-CARE-PHYSICIAN-ID TO PHYS-O IN WS-WARD-RPT-REC. 08580001
087700                                                                  08590001
087800 500-EXIT.                                                        08600001
087900     EXIT.                                                        08610001
088000                                                                  08620001
088100 600-DB2-TABLE-EDITS.                                             08630001
088200     MOVE "600-DB2-TABLE-EDITS" TO PARA-NAME.                     08640001
088300******** EXEC SQL to get info from DB2                            08650001
088400     MOVE DIAGNOSTIC-CODE-PRIMARY IN PATIENT-MASTER-REC TO        08660001
088500          DIAG-CODE IN DCLDIAG-CODES.                             08670001
088600                                                                  08680001
088700****** CHECK FOR VALID DIAGNOSTIC CODE                            08690001
088800     EXEC SQL                                                     08700001
088900        SELECT DIAG_CODE INTO :DIAG-CODE                          08710001
089000        FROM DDS0001.DIAG_CODES                                   08720001
089100        WHERE DIAG_CODE = :DIAG-CODE                              08730001
089200     END-EXEC.                                                    08740001
089300                                                                  08750001
089400     IF SQLCODE = -811 OR 0                                       08760001
089500         NEXT SENTENCE                                            08770001
089600     ELSE                                                         08780001
089700     IF SQLCODE = +100                                            08790001
089800         MOVE "*** DIAGNOSTIC CODE NOT-FOUND IN DIAG_CODES" TO    08800001
089900         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   08810001
090000         MOVE "Y" TO ERROR-FOUND-SW                               08820001
090100         move sqlcode to  PATIENT-ID IN INPATIENT-TREATMENT-REC   08830001
090200         MOVE DIAG-CODE IN DCLDIAG-CODES                          08840001
090300                            TO PRIMARY-DIAGNOSTIC-CODE            08850001
090400         move sqlcode to  EXPECTED-VAL                            08860001
090500         move PATIENT-ID IN INPATIENT-TREATMENT-REC               08870001
090600                         to ACTUAL-VAL                            08880001
090700         WRITE SYSOUT-REC FROM ABEND-REC                          08890001
090800         GO TO 600-EXIT                                           08900001
090900     ELSE                                                         08910001
091000     IF SQLCODE < 0                                               08920001
091100         MOVE "***  FATAL DB2 ERROR" TO                           08930001
091200         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   08940001
091300         MOVE "Y" TO ERROR-FOUND-SW                               08950001
091400         move sqlcode to  PATIENT-ID IN INPATIENT-TREATMENT-REC   08960001
091500         MOVE DIAG-CODE IN DCLDIAG-CODES                          08970001
091600                            TO PRIMARY-DIAGNOSTIC-CODE            08980001
091700         move sqlcode to  EXPECTED-VAL                            08990001
091800         move PATIENT-ID IN INPATIENT-TREATMENT-REC               09000001
091900                         to ACTUAL-VAL                            09010001
092000         WRITE SYSOUT-REC FROM ABEND-REC                          09020001
092100         GO TO 1000-DB2-ERROR-RTN.                                09030001
092200                                                                  09040001
092300****** CHECK FOR VALID BED IDENTITY                               09050001
092400     MOVE BED-IDENTITY-W TO BED-ID IN DCLHOSP-BED.                09060001
092500     EXEC SQL                                                     09070001
092600        SELECT BED_ID INTO :DCLHOSP-BED.BED-ID                    09080001
092700        FROM DDS0001.HOSP_BED                                     09090001
092800        WHERE BED_ID = :DCLHOSP-BED.BED-ID                        09100001
092900     END-EXEC.                                                    09110001
093000                                                                  09120001
093100     IF SQLCODE = -811 OR 0                                       09130001
093200         NEXT SENTENCE                                            09140001
093300     ELSE                                                         09150001
093400     IF SQLCODE = +100                                            09160001
093500         MOVE "*** BED IDENT NOT-FOUND IN HOSP_BED" TO            09170001
093600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   09180001
093700         MOVE "Y" TO ERROR-FOUND-SW                               09190001
093800         move sqlcode to  EXPECTED-VAL                            09200001
093900         move PATIENT-ID IN INPATIENT-TREATMENT-REC               09210001
094000                         to ACTUAL-VAL                            09220001
094100         WRITE SYSOUT-REC FROM ABEND-REC                          09230001
094200         GO TO 600-EXIT                                           09240001
094300     ELSE                                                         09250001
094400     IF SQLCODE < 0                                               09260001
094500         MOVE "***  FATAL DB2 ERROR" TO                           09270001
094600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   09280001
094700         MOVE "Y" TO ERROR-FOUND-SW                               09290001
094800         move sqlcode to  EXPECTED-VAL                            09300001
094900         move PATIENT-ID IN INPATIENT-TREATMENT-REC               09310001
095000                         to ACTUAL-VAL                            09320001
095100         WRITE SYSOUT-REC FROM ABEND-REC                          09330001
095200         GO TO 1000-DB2-ERROR-RTN.                                09340001
095300                                                                  09350001
095400****** CHECK FOR VALID PHYSICIAN-ID                               09360001
095500     MOVE ATTENDING-PHYS-ID TO PRIMARY-PHYSICIAN-ID.              09370001
095600     EXEC SQL                                                     09380001
095700        SELECT PRIMARY_PHYSICIAN_ID INTO :PRIMARY-PHYSICIAN-ID    09390001
095800        FROM DDS0001.WARD_DATA                                    09400001
095900        WHERE PRIMARY_PHYSICIAN_ID = :PRIMARY-PHYSICIAN-ID        09410001
096000     END-EXEC.                                                    09420001
096100                                                                  09430001
096200     IF SQLCODE = -811 OR 0                                       09440001
096300         NEXT SENTENCE                                            09450001
096400     ELSE                                                         09460001
096500     IF SQLCODE = +100                                            09470001
096600         MOVE "*** ATTENDING PHYSICIAN NOT FOUND IN TABLE" TO     09480001
096700         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   09490001
096800         MOVE "Y" TO ERROR-FOUND-SW                               09500001
096900         move sqlcode to  EXPECTED-VAL                            09510001
097000         move PATIENT-ID IN INPATIENT-TREATMENT-REC               09520001
097100                         to ACTUAL-VAL                            09530001
097200         WRITE SYSOUT-REC FROM ABEND-REC                          09540001
097300         GO TO 600-EXIT                                           09550001
097400     ELSE                                                         09560001
097500         MOVE "***  FATAL DB2 ERROR" TO                           09570001
097600         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   09580001
097700         MOVE "Y" TO ERROR-FOUND-SW                               09590001
097800         move sqlcode to  EXPECTED-VAL                            09600001
097900         move PATIENT-ID IN INPATIENT-TREATMENT-REC               09610001
098000                         to ACTUAL-VAL                            09620001
098100         WRITE SYSOUT-REC FROM ABEND-REC                          09630001
098200         GO TO 1000-DB2-ERROR-RTN.                                09640001
098300                                                                  09650001
098400****** CHECK FOR VALID MEDICATION-ID                              09660001
098500     MOVE MEDICATION-ID IN INPATIENT-TREATMENT-REC TO             09670001
098600            MEDICATION-ID IN DCLMEDICATION.                       09680001
098700                                                                  09690001
098800     EXEC SQL                                                     09700001
098900        SELECT MEDICATION_ID                                      09710001
099000                       INTO :DCLMEDICATION.MEDICATION-ID          09720001
099100        FROM DDS0001.MEDICATION                                   09730001
099200        WHERE MEDICATION_ID = :DCLMEDICATION.MEDICATION-ID        09740001
099300     END-EXEC.                                                    09750001
099400                                                                  09760001
099500     IF SQLCODE = -811 OR 0                                       09770001
099600         NEXT SENTENCE                                            09780001
099700     ELSE                                                         09790001
099800     IF SQLCODE = +100                                            09800001
099900         MOVE "*** MEDICATION-ID NOT FOUND IN TABLE" TO           09810001
100000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   09820001
100100         MOVE "Y" TO ERROR-FOUND-SW                               09830001
100200         move sqlcode to  EXPECTED-VAL                            09840001
100300         move PATIENT-ID IN INPATIENT-TREATMENT-REC               09850001
100400                         to ACTUAL-VAL                            09860001
100500         WRITE SYSOUT-REC FROM ABEND-REC                          09870001
100600         GO TO 600-EXIT                                           09880001
100700     ELSE                                                         09890001
100800     IF SQLCODE < 0                                               09900001
100900         MOVE "***  FATAL DB2 ERROR" TO                           09910001
101000         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   09920001
101100         MOVE "Y" TO ERROR-FOUND-SW                               09930001
101200         move sqlcode to  EXPECTED-VAL                            09940001
101300         move PATIENT-ID IN INPATIENT-TREATMENT-REC               09950001
101400                         to ACTUAL-VAL                            09960001
101500         WRITE SYSOUT-REC FROM ABEND-REC                          09970001
101600         GO TO 1000-DB2-ERROR-RTN.                                09980001
101700                                                                  09990001
101800****** CHECK FOR VALID SUPERVISOR NURSE-ID                        10000001
101900     MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.              10010001
102000     MOVE PRIMARY-CARE-PHYSICIAN-ID TO PHYS-O IN WS-WARD-RPT-REC. 10020001
102100     EXEC SQL                                                     10030001
102200        SELECT SUPERVISE_NURSE_ID                                 10040001
102300                       INTO :SUPERVISE-NURSE-ID                   10050001
102400        FROM DDS0001.WARD_DATA                                    10060001
102500        WHERE SUPERVISE_NURSE_ID = :SUPERVISE-NURSE-ID            10070001
102600     END-EXEC.                                                    10080001
102700                                                                  10090001
102800     IF SQLCODE = -811 OR 0                                       10100001
102900         NEXT SENTENCE                                            10110001
103000     ELSE                                                         10120001
103100     IF SQLCODE = +100                                            10130001
103200         MOVE "*** SUPERVISOR NURSE NOT FOUND" TO                 10140001
103300         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   10150001
103400         MOVE "Y" TO ERROR-FOUND-SW                               10160001
103500         move sqlcode to  EXPECTED-VAL                            10170001
103600         move PATIENT-ID IN INPATIENT-TREATMENT-REC               10180001
103700                         to ACTUAL-VAL                            10190001
103800         WRITE SYSOUT-REC FROM ABEND-REC                          10200001
103900         GO TO 600-EXIT                                           10210001
104000     ELSE                                                         10220001
104100     IF SQLCODE < 0                                               10230001
104200         MOVE "*** FATAL DB2 ERROR" TO                            10240001
104300         ERR-MSG IN INPATIENT-TREATMENT-REC-ERR                   10250001
104400         MOVE "Y" TO ERROR-FOUND-SW                               10260001
104500         move sqlcode to  EXPECTED-VAL                            10270001
104600         move PATIENT-ID IN INPATIENT-TREATMENT-REC               10280001
104700                         to ACTUAL-VAL                            10290001
104800         WRITE SYSOUT-REC FROM ABEND-REC                          10300001
104900         GO TO 1000-DB2-ERROR-RTN.                                10310001
105000 600-EXIT.                                                        10320001
105100     EXIT.                                                        10330001
105200 700-WRITE-TRMTEDIT.                                              10340001
105300     PERFORM 400-NUMERIC-RANGE-EDITS THRU 400-N-EXIT.             10350001
105400     MOVE "700-WRITE-TRMTEDIT" TO PARA-NAME.                      10360001
105500                                                                  10370001
105600     WRITE RPT-REC.                                               10380001
105700     ADD MEDICATION-COST  TO WS-MEDICATION-CHARGES.               10390001
105800     ADD ANCILLARY-CHARGE TO WS-ANCILLARY-CHARGES.                10400001
105900     ADD PHARMACY-COST IN INPATIENT-TREATMENT-REC                 10410001
106000                          TO WS-PHARMACY-CHARGES.                 10420001
106100     ADD +1 TO RECORDS-WRITTEN.                                   10430001
106200 700-T-EXIT.                                                      10440001
106300     EXIT.                                                        10450001
106400                                                                  10460001
106500 700-WRITE-PAGE-HDR.                                              10470001
106600     MOVE "700-WRITE-PAGE-HDR" TO PARA-NAME.                      10480001
106700     MOVE WS-PAGES TO PAGE-NBR-O.                                 10490001
106800     WRITE RPT-REC FROM WS-HDR-REC                                10500001
106900         AFTER ADVANCING NEXT-PAGE.                               10510001
107000     WRITE RPT-REC FROM WS-BLANK-LINE.                            10520001
107100     ADD +1 TO WS-PAGES.                                          10530001
107200     MOVE +2 TO WS-LINES.                                         10540001
107300 700-EXIT.                                                        10550001
107400     EXIT.                                                        10560001
107500                                                                  10570001
107600                                                                  10580001
107700 710-WRITE-TRMTERR.                                               10590001
107800     MOVE INPATIENT-TREATMENT-REC TO REST-OF-REC.                 10600001
107900     WRITE INPATIENT-TREATMENT-REC-ERR.                           10610001
108000     ADD +1 TO WS-RECORDS-IN-ERROR.                               10620001
108100 710-EXIT.                                                        10630001
108200     EXIT.                                                        10640001
108300                                                                  10650001
108400 720-WRITE-WARD-RPT.                                              10660001
108500     MOVE "720-WRITE-WARD-RPT" TO PARA-NAME.                      10670001
108600     WRITE RPT-REC FROM WS-WARD-RPT-REC                           10680001
108700         AFTER ADVANCING 2.                                       10690001
108800     WRITE RPT-REC FROM WS-BLANK-LINE.                            10700001
108900     ADD +3 TO WS-LINES.                                          10710001
109000 720-EXIT.                                                        10720001
109100     EXIT.                                                        10730001
109200                                                                  10740001
109300 740-WRITE-ROOM-RPT.                                              10750001
109400     MOVE "740-WRITE-ROOM-RPT" TO PARA-NAME.                      10760001
109500     PERFORM 790-CHECK-PAGINATION THRU 790-EXIT.                  10770001
109600     WRITE RPT-REC FROM WS-ROOM-RPT-REC                           10780001
109700         AFTER ADVANCING 1.                                       10790001
109800     WRITE RPT-REC FROM WS-BLANK-LINE.                            10800001
109900     ADD +2 TO WS-LINES.                                          10810001
110000 740-EXIT.                                                        10820001
110100     EXIT.                                                        10830001
110200                                                                  10840001
110300 760-WRITE-PATIENT-RPT.                                           10850001
110400     MOVE "760-WRITE-PATIENT-RPT" TO PARA-NAME.                   10860001
110500     WRITE RPT-REC FROM WS-BED-PATIENT-DETAIL                     10870001
110600         AFTER ADVANCING 1.                                       10880001
110700     PERFORM 790-CHECK-PAGINATION THRU 790-EXIT.                  10890001
110800     ADD +1 TO WS-LINES.                                          10900001
110900 760-EXIT.                                                        10910001
111000     EXIT.                                                        10920001
111100                                                                  10930001
111200 790-CHECK-PAGINATION.                                            10940001
111300     MOVE "790-CHECK-PAGINATION" TO PARA-NAME.                    10950001
111400     IF WS-LINES > 50                                             10960001
111500        WRITE RPT-REC FROM WS-BLANK-LINE                          10970001
111600        WRITE RPT-REC FROM WS-BLANK-LINE                          10980001
111700        PERFORM 700-WRITE-PAGE-HDR THRU 700-EXIT.                 10990001
111800 790-EXIT.                                                        11000001
111900     EXIT.                                                        11010001
112000                                                                  11020001
112100 795-WRITE-PATERR.                                                11030001
112200     MOVE "795-WRITE-PATERR" TO PARA-NAME.                        11040001
112300     MOVE INPATIENT-DAILY-REC TO REST-OF-PAT-REC.                 11050001
112400     WRITE INPATIENT-DAILY-REC-ERR.                               11060001
112500     ADD +1 TO PAT-WS-RECORDS-IN-ERROR.                           11070001
112600 795-EXIT.                                                        11080001
112700     EXIT.                                                        11090001
112800                                                                  11100001
112900 800-OPEN-FILES.                                                  11110001
113000     MOVE "800-OPEN-FILES" TO PARA-NAME.                          11120001
113100     OPEN INPUT PATSRCH, PATPERSN, PATMSTR, TRMTDATA.             11130001
113200     OPEN OUTPUT WARDFILE, PATERR, SYSOUT,  TRMTERR.              11140001
113300     DISPLAY PATMSTR-STATUS, PATPERSN-STATUS.                     11150001
113400*     GOBACK.                                                     11160001
113500 800-EXIT.                                                        11170001
113600     EXIT.                                                        11180001
113700                                                                  11190001
113800 850-CLOSE-FILES.                                                 11200001
113900     MOVE "850-CLOSE-FILES" TO PARA-NAME.                         11210001
114000                                                                  11220001
114100     CLOSE PATSRCH, WARDFILE,                                     11230001
114200           SYSOUT, PATPERSN,                                      11240001
114300           PATMSTR.                                               11250001
114400     DISPLAY PATMSTR-STATUS, PATPERSN-STATUS.                     11260001
114500*     GOBACK.                                                     11270001
114600 850-EXIT.                                                        11280001
114700     EXIT.                                                        11290001
114800                                                                  11300001
114900 900-READ-TRMTDATA.                                               11310001
115000*  Code your statements here to read the input file               11320001
115100*  Remember to move "NO" to IFCODE if the input file is AT END    11330001
115200     INITIALIZE INPATIENT-TREATMENT-REC-DATA .                    11340001
115300     READ TRMTDATA                                                11350001
115400         AT END MOVE "N" TO MORE-DATA-SW                          11360001
115500         GO TO 900-T-EXIT                                         11370001
115600     END-READ                                                     11380001
115700     MOVE "N" TO ERROR-FOUND-SW.                                  11390001
115800     ADD +1 TO WS-INFILE-RECORD-READ-CTR.                         11400001
115900 900-T-EXIT.                                                      11410001
116000     EXIT.                                                        11420001
116100                                                                  11430001
116200 900-READ-WARD-DATA.                                              11440001
116300*  READ SORTED PATIENT TRANS FILE                                 11450001
116400*  Remember to move "NO" to IFCODE if the input file is AT END    11460001
116500     READ PATSRCH INTO INPATIENT-DAILY-REC                        11470001
116600         AT END MOVE "N" TO MORE-WARD-DATA-SW                     11480001
116700         GO TO 900-EXIT                                           11490001
116800     END-READ.                                                    11500001
116900                                                                  11510001
117000     ADD +1 TO PAT-WS-INFILE-RECORD-READ-CTR.                     11520001
117100 900-EXIT.                                                        11530001
117200     EXIT.                                                        11540001
117300                                                                  11550001
117400 999-CLEANUP.                                                     11560001
117500     MOVE "999-CLEANUP" TO PARA-NAME.                             11570001
117600*  Final file-handling edits and trailer record handling          11580001
117700     IF TRLR-REC-FOUND                                            11590001
117800         NEXT SENTENCE                                            11600001
117900     ELSE                                                         11610001
118000     IF NOT TRAILER-REC                                           11620001
118100         MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON  11630001
118200         GO TO 1000-ABEND-RTN.                                    11640001
118300                                                                  11650001
118400     MOVE 0 TO RETURN-CD IN MISC-WS-FLDS.                         11660001
118500                                                                  11670001
118600     MOVE INPATIENT-DAILY-REC TO WS-TRAILER-REC.                  11680001
118700     ADD +1 TO RECORDS-WRITTEN.                                   11690001
118800*    IF WS-INFILE-RECORD-READ-CTR NOT EQUAL TO IN-RECORD-COUNT    11700001
118900*        MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"        11710001
119000*                              TO ABEND-REASON                    11720001
119100*        MOVE WS-INFILE-RECORD-READ-CTR     TO ACTUAL-VAL         11730001
119200*        MOVE IN-RECORD-COUNT  TO EXPECTED-VAL                    11740001
119300*        GO TO 1000-ABEND-RTN.                                    11750001
119400*                                                                 11760001
119500*    MOVE "T" TO PATIENT-RECORD-TYPE.                             11770001
119600*    MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.                     11780001
119700*    MOVE WS-BASE-ROOM-CHARGE  TO IN-BASE-ROOM-CHARGE.            11790001
119800*    MOVE WS-TOTAL-ROOM-CHARGE TO IN-TOTAL-ROOM-CHARGE.           11800001
119900*    MOVE WS-EQUIPMENT-COST TO IN-EQUIPMENT-CHARGES.              11810001
120000*    WRITE INPATIENT-DAILY-REC  FROM WS-TRAILER-REC.              11820001
120100                                                                  11830001
120200*  Code the statement to close all files                          11840001
120300     PERFORM 850-CLOSE-FILES THRU 850-EXIT.                       11850001
120400                                                                  11860001
120500*  Code the statement to Display a successful end-of-job msg      11870001
120600     DISPLAY "NORMAL END OF JOB".                                 11880001
120700 999-EXIT.                                                        11890001
120800     EXIT.                                                        11900001
120900                                                                  11910001
121000 1000-ABEND-RTN.                                                  11920001
121100     WRITE SYSOUT-REC FROM ABEND-REC.                             11930001
121200     PERFORM 850-CLOSE-FILES THRU 850-EXIT.                       11940001
121300     DISPLAY "*** ABNORMAL END OF JOB- DALYEDIT ***" UPON CONSOLE.11950001
121400     DIVIDE ZERO-VAL INTO ONE-VAL.                                11960001
121500                                                                  11970001
121600 1000-DB2-ERROR-RTN.                                              11980001
121700************************************************************      11990001
121800*       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *      12000001
121900************************************************************      12010001
122000      DISPLAY '**** WE HAVE A SERIOUS PROBLEM HERE *****'.        12020001
122100      DISPLAY '999-ERROR-TRAP-RTN '.                              12030001
122200      MULTIPLY SQLCODE BY -1 GIVING SQLCODE.                      12040001
122300      DISPLAY 'SQLCODE ==> ' SQLCODE.                             12050001
122400      DISPLAY SQLCA.                                              12060001
122500      DISPLAY SQLERRM.                                            12070001
122600      EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.               12080001
122700      EXEC SQL ROLLBACK WORK END-EXEC.                            12090001
122800      GO TO 1000-ABEND-RTN.                                       12100001
