000100****************************************************************  00000100
000200* LICENSED MATERIALS - PROPERTY OF IBM                            00000200
000300* ALL RIGHTS RESERVED                                             00000300
000400****************************************************************  00000400
000500* PROGRAM:  SAM1                                                  00000500
000600*                                                                 00000600
000700* AUTHOR :  Doug Stout                                            00000700
000800*                                                                 00000800
000900* READS A SEQUENTIAL TRANSACTION FILE AND MAKES UPDATES           00000900
001000* TO A SORTED SEQUENTIAL CUSTOMER FILE                            00001000
001100*                                                                 00001100
001200* A GOOD CASE FOR DEBUGGING LAB - INDEED                          00001200
001300*                                                                 00001300
001400* CAN BE MADE TO ABEND WITH BAD INPUT DATA FOR FAULT ANALYSIS LAB 00001400
001500***************************************************************** 00001500
001501
001600*                                                                 00001600
001700* Transaction file record descriptions:                           00001700
001800*     0    1    1    2    2    3    3    4    4    5    5    6    00001800
001900* ....5....0....5....0....5....0....5....0....5....0....5....0....00001900
002000*                                                                 00002000
002100* *  <== an asterisk in first column is a comment                 00002100
002200* UPDATE ---key---- -command-- field-name ss -----------value-----00002200
002300*                   can be:                  valid formats:       00002300
002400*                   REPLACE                  character_string_____00002400
002500*                   ADD                      +99999999            00002500
002600*                   SUBTRACT                 +99999999.99         00002600
002700*       (The "ss" field is a subscript, used for the MONTH field o00002700
002800* DELETE ___key____  <== Delete Record                            00002800
002900* ADD    ___key____  <== Add a new blank record                   00002900
003000*                                                                 00003000
003100***************************************************************** 00003100
003200 IDENTIFICATION DIVISION.                                         00003200
003300 PROGRAM-ID. SAM1.                                                00003300
003400 ENVIRONMENT DIVISION.                                            00003400
003500 INPUT-OUTPUT SECTION.                                            00003500
003600 FILE-CONTROL.                                                    00003600
003700                                                                  00003700
003800     SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE                      00003800
003900         ACCESS IS SEQUENTIAL                                     00003900
004000         FILE STATUS  IS  WS-CUSTFILE-STATUS.                     00004000
004100                                                                  00004100
004200     SELECT CUSTOMER-FILE-OUT ASSIGN TO CUSTOUT                   00004200
004300         ACCESS IS SEQUENTIAL                                     00004300
004400         FILE STATUS  IS  WS-CUSTOUT-STATUS.                      00004400
004500                                                                  00004500
004600      SELECT TRANSACTION-FILE ASSIGN TO TRANFILE                  00004600
004700             FILE STATUS  IS  WS-TRANFILE-STATUS.                 00004700
004800                                                                  00004800
004900      SELECT REPORT-FILE      ASSIGN TO CUSTRPT                   00004900
005000             FILE STATUS  IS  WS-REPORT-STATUS.                   00005000
005100                                                                  00005100
005200***************************************************************** 00005200
005300 DATA DIVISION.                                                   00005300
005400 FILE SECTION.                                                    00005400
005500                                                                  00005500
005600 FD  CUSTOMER-FILE                                                00005600
005602     RECORDING MODE IS F                                          00006200
005603     BLOCK CONTAINS 0 RECORDS.                                    00006300
005900 01  CUST-REC-FD                 PIC X(640).                      00005900
006000                                                                  00006000
006100 FD  CUSTOMER-FILE-OUT                                            00006100
006200     RECORDING MODE IS V                                          00006200
006300     BLOCK CONTAINS 0 RECORDS                                     00006300
006400     RECORD IS VARYING FROM 20 TO 596 CHARACTERS.                 00006400
006500 COPY CUSTCOPY REPLACING ==:TAG:== BY ==CSTOUT==.                 00006500
006600                                                                  00006600
006700 FD  TRANSACTION-FILE                                             00006700
006800     RECORDING MODE IS F.                                         00006800
006900 COPY TRANREC.                                                    00006900
007000                                                                  00007000
007100 FD  REPORT-FILE                                                  00007100
007200     RECORDING MODE IS F.                                         00007200
007300 01  REPORT-RECORD              PIC X(132).                       00007300
007400                                                                  00007400
007500***************************************************************** 00007500
007600 WORKING-STORAGE SECTION.                                         00007600
007700***************************************************************** 00007700
007800*                                                                 00007800
007900 01  SYSTEM-DATE-AND-TIME.                                        00007900
008000     05  CURRENT-DATE.                                            00008000
008100         10  CURRENT-YEAR            PIC 9(2).                    00008100
008200         10  CURRENT-MONTH           PIC 9(2).                    00008200
008300         10  CURRENT-DAY             PIC 9(2).                    00008300
008400     05  CURRENT-TIME.                                            00008400
008500         10  CURRENT-HOUR            PIC 9(2).                    00008500
008600         10  CURRENT-MINUTE          PIC 9(2).                    00008600
008700         10  CURRENT-SECOND          PIC 9(2).                    00008700
008800         10  CURRENT-HNDSEC          PIC 9(2).                    00008800
008900*                                                                 00008900
009000 01  CUST-REC.                                                    00009000
009100     05  CUST-KEY.                                                00009100
009200         10  CUST-ID             PIC X(5).                        00009200
009300         10  CUST-REC-TYPE       PIC X.                           00009300
009400     05  CUST-NAME               PIC X(17).                       00009400
009500     05  CUST-ACCT-BALANCE       PIC S9(7)V99  COMP-3.            00009500
009600     05  CUST-ORDERS-YTD         PIC S9(5)     COMP.              00009600
009700     05  CUST-ADDR               PIC X(20).                       00009700
009800     05  CUST-CITY               PIC X(14).                       00009800
009900     05  CUST-STATE              PIC X(02).                       00009900
010000     05  CUST-COUNTRY            PIC X(11).                       00010000
010100     05  CUST-MONTH              PIC S9(7)V99 COMP-3 OCCURS 12.   00010100
010200     05  CUST-OCCUPATION         PIC X(30).                       00010200
010300     05  CUST-NOTES              PIC X(120).                      00010300
010400     05  CUST-DATA-1             PIC X(05).                       00010400
010500     05  CUST-DATA-2             PIC X(40).                       00010500
010600*                                                                 00010600
010700 01  WS-FIELDS.                                                   00010700
010800     05  WS-CUSTFILE-STATUS      PIC X(2)  VALUE SPACES.          00010800
010900     05  WS-CUSTOUT-STATUS       PIC X(2)  VALUE SPACES.          00010900
011000     05  WS-TRANFILE-STATUS      PIC X(2)  VALUE SPACES.          00011000
011100     05  WS-REPORT-STATUS        PIC X(2)  VALUE SPACES.          00011100
011200     05  WS-TRAN-EOF             PIC X     VALUE SPACES.          00011200
011300     05  WS-TRAN-OK              PIC X     VALUE 'N'.             00011300
011400     05  WS-CUST-FILE-OK         PIC X     VALUE 'N'.             00011400
011401     05  WS-CUST-FILE-OK1        PIC 9(2) COMP-3.                 00011400
011500     05  WS-CUST-FILE-EOF        PIC X     VALUE 'N'.             00011500
011600     05  WS-TRAN-MSG             PIC X(50) VALUE SPACES.          00011600
011700     05  WS-PREV-TRAN-KEY        PIC X(10) VALUE LOW-VALUES.      00011700
011800     05  INCR-CUST-ID            PIC 9(5)  VALUE 0.               00011800
011900     05  START-CUST-ID           PIC 9(5)  VALUE 0.               00011900
012000     05  MAX-CUST-ID             PIC 9(5)  VALUE 0.               00012000
012100*                                                                 00012100
012200 01  WORK-VARIABLES.                                              00012200
012300     05  I                     PIC S9(9)   COMP-3  VALUE +0.      00012300
012400     05  WORK-NUM              PIC s9(8)   COMP.                  00012400
012500*                                                                 00012500
012600 01  REPORT-TOTALS.                                               00012600
012700     05  NUM-TRAN-RECS         PIC S9(9)   COMP-3  VALUE +0.      00012700
012800     05  NUM-TRAN-ERRORS       PIC S9(9)   COMP-3  VALUE +0.      00012800
012900     05  NUM-ADD-REQUESTS      PIC S9(9)   COMP-3  VALUE +0.      00012900
013000     05  NUM-ADD-PROCESSED     PIC S9(9)   COMP-3  VALUE +0.      00013000
013100     05  NUM-UPDATE-REQUESTS   PIC S9(9)   COMP-3  VALUE +0.      00013100
013200     05  NUM-UPDATE-PROCESSED  PIC S9(9)   COMP-3  VALUE +0.      00013200
013300     05  NUM-DELETE-REQUESTS   PIC S9(9)   COMP-3  VALUE +0.      00013300
013400     05  NUM-DELETE-PROCESSED  PIC S9(9)   COMP-3  VALUE +0.      00013400
013500     05  NUM-CRUNCH-REQUESTS   PIC S9(9)   COMP-3  VALUE +0.      00013500
013600     05  NUM-CRUNCH-PROCESSED  PIC S9(9)   COMP-3  VALUE +0.      00013600
013700     05  NUM-RPTALL-REQUESTS   PIC S9(9)   COMP-3  VALUE +0.      00013700
013800     05  NUM-RPTALL-PROCESSED  PIC S9(9)   COMP-3  VALUE +0.      00013800
013900     05  NUM-GEN-REQUESTS      PIC S9(9)   COMP-3  VALUE +0.      00013900
014000     05  NUM-GEN-PROCESSED     PIC S9(9)   COMP-3  VALUE +0.      00014000
014100                                                                  00014100
014200 COPY CUSTCOPY REPLACING ==:TAG:== BY ==WS-CUST==.                00014200
014300                                                                  00014300
014400*        *******************                                      00014400
014500*            report lines                                         00014500
014600*        *******************                                      00014600
014700 01  ERR-MSG-BAD-TRAN.                                            00014700
014800     05  FILLER PIC X(31)                                         00014800
014900                  VALUE 'Error Processing Transaction. '.         00014900
015000     05  ERR-MSG-DATA1              PIC X(35)  VALUE SPACES.      00015000
015100     05  ERR-MSG-DATA2              PIC X(66)  VALUE SPACES.      00015100
015200 01  ERR-MSG-BAD-TRAN-2.                                          00015200
015300     05  FILLER                     PIC X(21)  VALUE SPACES.      00015300
015400     05  ERR-MSG-DATA3              PIC X(80).                    00015400
015500     05  FILLER                     PIC X(31)  VALUE SPACES.      00015500
015600 01  MSG-TRAN-SCALE-1.                                            00015600
015700     05  FILLER PIC X(21) VALUE SPACES.                           00015700
015800     05  FILLER                     PIC X(35)                     00015800
015900                    VALUE '         1    1    2    2    3    3'.  00015900
016000     05  FILLER                     PIC X(35)                     00016000
016100                    VALUE '    4    4    5    5    6    6    7'.  00016100
016200     05  FILLER                     PIC X(41)  VALUE SPACES.      00016200
016300 01  MSG-TRAN-SCALE-2.                                            00016300
016400     05  FILLER PIC X(21) VALUE ' Transaction Record: '.          00016400
016500     05  FILLER                     PIC X(35)                     00016500
016600                    VALUE '....5....0....5....0....5....0....5'.  00016600
016700     05  FILLER                     PIC X(35)                     00016700
016800                    VALUE '....0....5....0....5....0....5....0'.  00016800
016900     05  FILLER                     PIC X(41)  VALUE SPACES.      00016900
017000 01 RPT-HEADER1.                                                  00017000
017100     05  FILLER                     PIC X(40)                     00017100
017200               VALUE 'CUSTOMER FILE UPDATE REPORT       DATE: '.  00017200
017300     05  RPT-MM                     PIC 99.                       00017300
017400     05  FILLER                     PIC X     VALUE '/'.          00017400
017500     05  RPT-DD                     PIC 99.                       00017500
017600     05  FILLER                     PIC X     VALUE '/'.          00017600
017700     05  RPT-YY                     PIC 99.                       00017700
017800     05  FILLER                     PIC X(20)                     00017800
017900                    VALUE ' (mm/dd/yy)   TIME: '.                 00017900
018000     05  RPT-HH                     PIC 99.                       00018000
018100     05  FILLER                     PIC X     VALUE ':'.          00018100
018200     05  RPT-MIN                    PIC 99.                       00018200
018300     05  FILLER                     PIC X     VALUE ':'.          00018300
018400     05  RPT-SS                     PIC 99.                       00018400
018500     05  FILLER                     PIC X(55) VALUE SPACES.       00018500
018600 01  RPT-TRAN-DETAIL1.                                            00018600
018700     05  RPT-TRAN-MSG1      PIC X(31)                             00018700
018800                  VALUE '       Transaction processed: '.         00018800
018900     05  RPT-TRAN-RECORD            PIC X(80)  VALUE SPACES.      00018900
019000     05  FILLER                     PIC X(21)  VALUE SPACES.      00019000
019100 01  RPT-STATS-HDR1.                                              00019100
019200     05  FILLER PIC X(26) VALUE 'Transaction Totals:       '.     00019200
019300     05  FILLER PIC X(107) VALUE SPACES.                          00019300
019400 01  RPT-STATS-HDR2.                                              00019400
019500     05  FILLER PIC X(26) VALUE 'Transaction      Number of'.     00019500
019600     05  FILLER PIC X(28) VALUE '        Number        Number'.   00019600
019700     05  FILLER PIC X(79) VALUE SPACES.                           00019700
019800 01  RPT-STATS-HDR3.                                              00019800
019900     05  FILLER PIC X(26) VALUE 'Type          Transactions'.     00019900
020000     05  FILLER PIC X(28) VALUE '     Processed      In Error'.   00020000
020100     05  FILLER PIC X(79) VALUE SPACES.                           00020100
020200 01  RPT-STATS-HDR4.                                              00020200
020300     05  FILLER PIC X(26) VALUE '-----------   ------------'.     00020300
020400     05  FILLER PIC X(28) VALUE '   -----------   -----------'.   00020400
020500     05  FILLER PIC X(79) VALUE SPACES.                           00020500
020600 01  RPT-STATS-DETAIL.                                            00020600
020700     05  RPT-TRAN            PIC X(10).                           00020700
020800     05  FILLER              PIC X(4)     VALUE SPACES.           00020800
020900     05  RPT-NUM-TRANS       PIC ZZZ,ZZZ,ZZ9.                     00020900
021000     05  FILLER              PIC X(3)     VALUE SPACES.           00021000
021100     05  RPT-NUM-TRAN-PROC   PIC ZZZ,ZZZ,ZZ9.                     00021100
021200     05  FILLER              PIC X(3)     VALUE SPACES.           00021200
021300     05  RPT-NUM-TRAN-ERR    PIC ZZZ,ZZZ,ZZ9.                     00021300
021400     05  FILLER              PIC X(80)   VALUE SPACES.            00021400
021500                                                                  00021500
021600 01  ABEND-TEST              PIC X(2).                            00021600
021700 01  ABEND-TEST-N REDEFINES ABEND-TEST PIC S9(3) COMP-3.          00021700
021800                                                                  00021800
021900***************************************************************** 00021900
022000 PROCEDURE DIVISION.                                              00022000
022100***************************************************************** 00022100
022200                                                                  00022200
022300 000-MAIN.                                                        00022300
022400     ACCEPT CURRENT-DATE FROM DATE.                               00022400
022500     ACCEPT CURRENT-TIME FROM TIME.                               00022500
022600     DISPLAY 'SAM1 STARTED DATE = ' CURRENT-MONTH '/'             00022600
022700            CURRENT-DAY '/' CURRENT-YEAR '  (mm/dd/yy)'.          00022700
022800     DISPLAY '             TIME = ' CURRENT-HOUR ':'              00022800
022900            CURRENT-MINUTE ':' CURRENT-SECOND.                    00022900
023000                                                                  00023000
023100     PERFORM 700-OPEN-FILES .                                     00023100
023200     PERFORM 800-INIT-REPORT .                                    00023200
023300                                                                  00023300
023400     PERFORM 730-READ-CUSTOMER-FILE .                             00023400
023500     PERFORM 100-PROCESS-TRANSACTIONS                             00023500
023600             UNTIL WS-TRAN-EOF = 'Y' .                            00023600
023700                                                                  00023700
023800     PERFORM 850-REPORT-TRAN-STATS .                              00023800
023900     PERFORM 790-CLOSE-FILES .                                    00023900
024000                                                                  00024000
024100     GOBACK .                                                     00024100
024200                                                                  00024200
           CALL 'REF1' USING WS-FIELDS, CUST-REC, CSTOUT-CONTACT-REC,
                   RPT-TRAN-DETAIL1, MSG-TRAN-SCALE-1, MSG-TRAN-SCALE-2,
                   ERR-MSG-BAD-TRAN-2, CSTOUT-REC, WS-CUST-CONTACT-REC,
                   REPORT-TOTALS, WS-CUST-REC, TRANSACTION-FILE,
                   CUSTOMER-FILE, REPORT-RECORD, ERR-MSG-BAD-TRAN,
                   TRANSACTION-RECORD, WORK-VARIABLES.
027400                                                                  00027400
027500                                                                  00027500
029700                                                                  00029700
031700                                                                  00031700
032900                                                                  00032900
033800                                                                  00033800
033900 700-OPEN-FILES.                                                  00033900
034000     OPEN INPUT    TRANSACTION-FILE                               00034000
034100                   CUSTOMER-FILE                                  00034100
034200          OUTPUT   CUSTOMER-FILE-OUT                              00034200
034300                   REPORT-FILE .                                  00034300
034400     IF WS-CUSTFILE-STATUS NOT = '00'                             00034400
034500       DISPLAY 'ERROR OPENING CUSTOMER INPUT FILE. RC:'           00034500
034600               WS-CUSTFILE-STATUS                                 00034600
034700       DISPLAY 'Terminating Program due to File Error'            00034700
034800       MOVE 16 TO RETURN-CODE                                     00034800
034900       MOVE 'Y' TO WS-TRAN-EOF                                    00034900
035000     END-IF .                                                     00035000
035100     IF WS-CUSTOUT-STATUS NOT = '00'                              00035100
035200       DISPLAY 'ERROR OPENING CUSTOMER OUTPUT FILE. RC:'          00035200
035300               WS-CUSTOUT-STATUS                                  00035300
035400       DISPLAY 'Terminating Program due to File Error'            00035400
035500       MOVE 16 TO RETURN-CODE                                     00035500
035600       MOVE 'Y' TO WS-TRAN-EOF                                    00035600
035700     END-IF .                                                     00035700
035800     IF WS-TRANFILE-STATUS NOT = '00'                             00035800
035900       DISPLAY 'ERROR OPENING TRAN FILE. RC:' WS-TRANFILE-STATUS  00035900
036000       DISPLAY 'Terminating Program due to File Error'            00036000
036100       MOVE 16 TO RETURN-CODE                                     00036100
036200       MOVE 'Y' TO WS-TRAN-EOF                                    00036200
036300     END-IF .                                                     00036300
036400                                                                  00036400
036500                                                                  00036500
038700                                                                  00038700
039600                                                                  00039600
040100                                                                  00040100
041700                                                                  00041700
043400                                                                  00043400
044000 800-INIT-REPORT.                                                 00044000
044100     MOVE CURRENT-YEAR   TO RPT-YY.                               00044100
044200     MOVE CURRENT-MONTH  TO RPT-MM.                               00044200
044300     MOVE CURRENT-DAY    TO RPT-DD.                               00044300
044400     MOVE CURRENT-HOUR   TO RPT-HH.                               00044400
044500     MOVE CURRENT-MINUTE TO RPT-MIN.                              00044500
044600     MOVE CURRENT-SECOND TO RPT-SS.                               00044600
044700     WRITE REPORT-RECORD FROM RPT-HEADER1 AFTER PAGE.             00044700
044800                                                                  00044800
045700                                                                  00045700
045800 850-REPORT-TRAN-STATS.                                           00045800
045900     WRITE REPORT-RECORD FROM RPT-STATS-HDR1 AFTER 2.             00045900
046000     WRITE REPORT-RECORD FROM RPT-STATS-HDR2 AFTER 2.             00046000
046100     WRITE REPORT-RECORD FROM RPT-STATS-HDR3 AFTER 1.             00046100
046200     WRITE REPORT-RECORD FROM RPT-STATS-HDR4 AFTER 1.             00046200
046300                                                                  00046300
046400     MOVE 'ADD    '            TO RPT-TRAN.                       00046400
046500     MOVE NUM-ADD-REQUESTS     TO RPT-NUM-TRANS.                  00046500
046600     MOVE NUM-ADD-PROCESSED    TO RPT-NUM-TRAN-PROC.              00046600
046700     COMPUTE RPT-NUM-TRAN-ERR =                                   00046700
046800                NUM-ADD-REQUESTS  -  NUM-ADD-PROCESSED .          00046800
046900     WRITE REPORT-RECORD  FROM  RPT-STATS-DETAIL.                 00046900
047000                                                                  00047000
047100     MOVE 'DELETE '            TO RPT-TRAN.                       00047100
047200     MOVE NUM-DELETE-REQUESTS  TO RPT-NUM-TRANS.                  00047200
047300     MOVE NUM-DELETE-PROCESSED TO RPT-NUM-TRAN-PROC.              00047300
047400     COMPUTE RPT-NUM-TRAN-ERR =                                   00047400
047500                NUM-DELETE-REQUESTS  -  NUM-DELETE-PROCESSED .    00047500
047600     WRITE REPORT-RECORD  FROM  RPT-STATS-DETAIL.                 00047600
047700                                                                  00047700
047800     MOVE 'UPDATE '            TO RPT-TRAN.                       00047800
047900     MOVE NUM-UPDATE-REQUESTS  TO RPT-NUM-TRANS.                  00047900
048000     MOVE NUM-UPDATE-PROCESSED TO RPT-NUM-TRAN-PROC.              00048000
048100     COMPUTE RPT-NUM-TRAN-ERR =                                   00048100
048200                NUM-UPDATE-REQUESTS  -  NUM-UPDATE-PROCESSED .    00048200
048300     WRITE REPORT-RECORD  FROM  RPT-STATS-DETAIL.                 00048300

       COPY REFCPY.

