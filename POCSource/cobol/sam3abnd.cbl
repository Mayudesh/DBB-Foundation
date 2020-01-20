000100****************************************************************  00000100
000200* LICENSED MATERIALS - PROPERTY OF IBM                            00000200
000300* ALL RIGHTS RESERVED                                             00000300
000400****************************************************************  00000400
000500* PROGRAM:  SAM3ABND                                              00000500
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
003300 PROGRAM-ID. SAM3ABND                                             00003300
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
005900 01  CUST-REC-FD                 PIC X(600).                      00005900
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
011500     05  WS-CUST-FILE-EOF        PIC X     VALUE 'N'.             00011500
011600     05  WS-TRAN-MSG             PIC X(50) VALUE SPACES.          00011600
011700     05  WS-PREV-TRAN-KEY        PIC X(10) VALUE LOW-VALUES.      00011700
011800     05  INCR-CUST-ID            PIC 9(5)  VALUE 0.               00011800
011900     05  START-CUST-ID           PIC 9(5)  VALUE 0.               00011900
012000     05  MAX-CUST-ID             PIC 9(5)  VALUE 0.               00012000
012001     05  TEST-BASELINE           PIC 9(4)  COMP-3  VALUE 0.       00012000
012100*                                                                 00012100
012200 01  WORK-VARIABLES.                                              00012200
012300     05  I                     PIC S9(9)   COMP-3  VALUE +0.      00012300
012400     05  WORK-NUM              PIC S9(8)   COMP-3  VALUE +0.      00012400
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
021600 01  CUST-KEY-DIAG            PIC X(6).                           00021600
021601 01  REF-MOD-VAL              PIC X(4).                           00021600
021602 01  REF-MOD-1                PIC S9(1) VALUE 1.                  00021600
021603 01  REF-MOD-2                PIC S9(1) VALUE 4.                  00021600
021604 01  ABEND-TEST              PIC X(2).                            00021600
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
022701*           UPON CONSOLE.
022800     DISPLAY '             TIME = ' CURRENT-HOUR ':'              00022800
022900            CURRENT-MINUTE ':' CURRENT-SECOND .                   00022900
022901*           UPON CONSOLE.
022902
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
024300 100-PROCESS-TRANSACTIONS.                                        00024300
024400     PERFORM 710-READ-TRAN-FILE.                                  00024400
024500                                                                  00024500
024600     IF WS-TRAN-EOF NOT = 'Y'                                     00024600
024700         COMPUTE NUM-TRAN-RECS = NUM-TRAN-RECS + 1                00024700
024800         MOVE 'Y' TO WS-TRAN-OK
024801         MOVE TRAN-KEY(REF-MOD-1:REF-MOD-2) TO REF-MOD-VAL        00024800
024900         IF TRAN-KEY < WS-PREV-TRAN-KEY                           00024900
025000            MOVE 'TRANSACTION OUT OF SEQUENCE' TO ERR-MSG-DATA1   00025000
025100            MOVE SPACES TO ERR-MSG-DATA2                          00025100
025200            PERFORM 299-REPORT-BAD-TRAN                           00025200
025300         ELSE                                                     00025300
025400           EVALUATE TRAN-CODE                                     00025400
025500              WHEN 'UPDATE'                                       00025500
025600                  PERFORM 200-PROCESS-UPDATE-TRAN                 00025600
025700              WHEN 'ADD   '                                       00025700
025800                  PERFORM 210-PROCESS-ADD-TRAN                    00025800
025900              WHEN 'DELETE'                                       00025900
026000                  PERFORM 220-PROCESS-DELETE-TRAN                 00026000
026100              WHEN OTHER                                          00026100
026200                  IF TRAN-COMMENT NOT = '*'                       00026200
026300                    MOVE 'INVALID TRAN CODE:' TO ERR-MSG-DATA1    00026300
026400                    MOVE TRAN-CODE TO ERR-MSG-DATA2               00026400
026500                    PERFORM 299-REPORT-BAD-TRAN                   00026500
026600                  END-IF                                          00026600
026700           END-EVALUATE                                           00026700
026800         END-IF                                                   00026800
026900         MOVE TRAN-KEY TO WS-PREV-TRAN-KEY                        00026900
027000         IF WS-TRAN-OK = 'Y'                                      00027000
027100             PERFORM 830-REPORT-TRAN-PROCESSED                    00027100
027200         END-IF                                                   00027200
027300     END-IF .                                                     00027300
027400                                                                  00027400
027500                                                                  00027500
027600 200-PROCESS-UPDATE-TRAN.                                         00027600
027700     ADD +1 TO NUM-UPDATE-REQUESTS.                               00027700
027701     MOVE CUST-KEY  TO CUST-KEY-DIAG.
027800     PERFORM 720-POSITION-CUST-FILE.                              00027800
027900     IF CUST-KEY NOT = TRAN-KEY OR WS-CUST-FILE-EOF = 'Y'         00027900
028000         MOVE 'NO MATCHING KEY:     ' TO ERR-MSG-DATA1            00028000
028100         MOVE TRAN-KEY  TO ERR-MSG-DATA2                          00028100
028200         PERFORM 299-REPORT-BAD-TRAN                              00028200
028300     ELSE                                                         00028300
028400*                                                                 00028400
028500*        Subroutine SAM4 will apply an update to a customer record00028500
028600*                                                                 00028600
028601       IF TRAN-KEY = '13062A' MOVE 'ABCDEFG' TO CUST-NOTES
028700         CALL 'SAM4ABND' USING CUST-REC, TRANSACTION-RECORD,      00028700
028800                                WS-TRAN-OK, WS-TRAN-MSG           00028800
028900         IF WS-TRAN-OK NOT = 'Y'                                  00028900
029000             MOVE WS-TRAN-MSG TO ERR-MSG-DATA1                    00029000
029100             MOVE SPACES      TO ERR-MSG-DATA2                    00029100
029200             PERFORM 299-REPORT-BAD-TRAN                          00029200
029300         ELSE                                                     00029300
029400             ADD +1 TO NUM-UPDATE-PROCESSED                       00029400
029500         END-IF                                                   00029500
029501*     CALL 'DBGMAIN'
029600     END-IF .                                                     00029600
029700                                                                  00029700
029800 210-PROCESS-ADD-TRAN.                                            00029800
029900     ADD +1 TO NUM-ADD-REQUESTS .                                 00029900
030000     PERFORM 720-POSITION-CUST-FILE.                              00030000
030100     IF CUST-KEY = TRAN-KEY                                       00030100
030200         MOVE 'DUPLICATE KEY:       ' TO ERR-MSG-DATA1            00030200
030300         MOVE TRAN-KEY  TO ERR-MSG-DATA2                          00030300
030400         PERFORM 299-REPORT-BAD-TRAN                              00030400
030500     ELSE                                                         00030500
030600         MOVE SPACES TO WS-CUST-REC                               00030600
030700         MOVE TRAN-KEY TO WS-CUST-KEY                             00030700
030800         MOVE +0 TO WS-CUST-ACCT-BALANCE                          00030800
030900         MOVE +0 TO WS-CUST-ORDERS-YTD                            00030900
031000         PERFORM TEST AFTER VARYING I FROM 1 BY 1                 00031000
031100           UNTIL I > 12                                           00031100
031200             MOVE +0 TO WS-CUST-MONTH(I)                          00031200
031300         END-PERFORM                                              00031300
031400         PERFORM 740-WRITE-CUSTOUT-FILE                           00031400
031500         ADD +1 TO NUM-ADD-PROCESSED                              00031500
031600     END-IF .                                                     00031600
031700                                                                  00031700
031800 220-PROCESS-DELETE-TRAN.                                         00031800
031900     ADD +1 TO NUM-DELETE-REQUESTS.                               00031900
032000     PERFORM 720-POSITION-CUST-FILE.                              00032000
032100     IF CUST-KEY NOT = TRAN-KEY OR WS-CUST-FILE-EOF = 'Y'         00032100
032200         MOVE 'NO MATCHING KEY:     ' TO ERR-MSG-DATA1            00032200
032300         MOVE TRAN-KEY  TO ERR-MSG-DATA2                          00032300
032400         PERFORM 299-REPORT-BAD-TRAN                              00032400
032500     ELSE                                                         00032500
032600         ADD +1 TO NUM-DELETE-PROCESSED                           00032600
032700         PERFORM 730-READ-CUSTOMER-FILE                           00032700
032800     END-IF .                                                     00032800
032900                                                                  00032900
033000 299-REPORT-BAD-TRAN.                                             00033000
033100     ADD +1 TO NUM-TRAN-ERRORS.                                   00033100
033200     MOVE 'N' TO WS-TRAN-OK.                                      00033200
033300     WRITE REPORT-RECORD FROM ERR-MSG-BAD-TRAN  AFTER 2.          00033300
033400     WRITE REPORT-RECORD FROM MSG-TRAN-SCALE-1.                   00033400
033500     WRITE REPORT-RECORD FROM MSG-TRAN-SCALE-2.                   00033500
033600     MOVE TRANSACTION-RECORD   TO ERR-MSG-DATA3.                  00033600
033700     WRITE REPORT-RECORD FROM ERR-MSG-BAD-TRAN-2.                 00033700
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
036600 710-READ-TRAN-FILE.                                              00036600
036700     READ TRANSACTION-FILE                                        00036700
036800       AT END MOVE 'Y' TO WS-TRAN-EOF .                           00036800
036801     IF TRAN-KEY = '13062A' MOVE 'ABCDEFG' TO CUST-NOTES.
036900*    MOVE SPACES TO ABEND-TEST                                    00036900
037000*    ADD 1 TO ABEND-TEST-N                                        00037000
037100     EVALUATE      WS-TRANFILE-STATUS                             00037100
037200        WHEN '00'                                                 00037200
037300             CONTINUE                                             00037300
037400        WHEN '10'                                                 00037400
037500             MOVE 'Y' TO WS-TRAN-EOF                              00037500
037600        WHEN OTHER                                                00037600
037700            MOVE 'Error on tran file read.  Code:'                00037700
037800                        TO ERR-MSG-DATA1                          00037800
037900            MOVE WS-CUSTFILE-STATUS TO ERR-MSG-DATA2              00037900
038000            PERFORM 299-REPORT-BAD-TRAN                           00038000
038100            MOVE 'Y' TO WS-TRAN-EOF                               00038100
038200     END-EVALUATE .                                               00038200
038300     IF WS-TRAN-EOF = 'Y'                                         00038300
038400         PERFORM 721-COPY-RECORDS                                 00038400
038500           UNTIL WS-CUST-FILE-EOF = 'Y'                           00038500
038600     END-IF .                                                     00038600
038700                                                                  00038700
038800 720-POSITION-CUST-FILE.                                          00038800
038900     IF CUST-KEY < TRAN-KEY                                       00038900
039000         IF WS-CUST-FILE-EOF NOT = 'Y'                            00039000
039100             PERFORM 721-COPY-RECORDS                             00039100
039200               UNTIL CUST-KEY >= TRAN-KEY                         00039200
039300                  OR WS-CUST-FILE-EOF = 'Y'                       00039300
039400         END-IF                                                   00039400
039500     END-IF .                                                     00039500
039600                                                                  00039600
039700 721-COPY-RECORDS.                                                00039700
039800     MOVE CUST-REC TO WS-CUST-REC .                               00039800
039900     PERFORM 740-WRITE-CUSTOUT-FILE .                             00039900
040000     PERFORM 730-READ-CUSTOMER-FILE .                             00040000
040100                                                                  00040100
040200 730-READ-CUSTOMER-FILE.                                          00040200
040300     READ CUSTOMER-FILE INTO CUST-REC                             00040300
040400       AT END MOVE 'Y' TO WS-CUST-FILE-EOF .                      00040400
040402
040403     IF TRAN-KEY = '13062A' MOVE 'ABCDEFG' TO CUST-NOTES.
040500     EVALUATE WS-CUSTFILE-STATUS                                  00040500
040600        WHEN '00'                                                 00040600
040700        WHEN '04'                                                 00040700
040800            CONTINUE                                              00040800
040900        WHEN '10'                                                 00040900
041000            MOVE 'Y' TO WS-CUST-FILE-EOF                          00041000
041100        WHEN OTHER                                                00041100
041200            MOVE 'Customer input File I/O Error on Read.  RC: '   00041200
041300                        TO ERR-MSG-DATA1                          00041300
041400            MOVE WS-CUSTFILE-STATUS TO ERR-MSG-DATA2              00041400
041500            PERFORM 299-REPORT-BAD-TRAN                           00041500
041600     END-EVALUATE .                                               00041600
041700                                                                  00041700
041800 740-WRITE-CUSTOUT-FILE.                                          00041800
041900     IF WS-CUST-REC-TYPE = 'A'                                    00041900
042000         WRITE CSTOUT-REC FROM WS-CUST-REC                        00042000
042100     ELSE                                                         00042100
042200         MOVE WS-CUST-REC  TO  WS-CUST-CONTACT-REC                00042200
042300         WRITE CSTOUT-CONTACT-REC FROM WS-CUST-CONTACT-REC        00042300
042400     END-IF .                                                     00042400
042500     EVALUATE WS-CUSTOUT-STATUS                                   00042500
042600        WHEN '00'                                                 00042600
042700            CONTINUE                                              00042700
042800        WHEN OTHER                                                00042800
042900            MOVE 'CUSTOMER OUTPUT FILE I/O ERROR ON WRITE. RC: '  00042900
043000                        TO ERR-MSG-DATA1                          00043000
043100            MOVE WS-CUSTFILE-STATUS TO ERR-MSG-DATA2              00043100
043200            PERFORM 299-REPORT-BAD-TRAN                           00043200
043300     END-EVALUATE .                                               00043300
043400                                                                  00043400
043500 790-CLOSE-FILES.                                                 00043500
043600     CLOSE TRANSACTION-FILE .                                     00043600
043700     CLOSE REPORT-FILE .                                          00043700
043800     CLOSE CUSTOMER-FILE .                                        00043800
043900                                                                  00043900
044000 800-INIT-REPORT.                                                 00044000
044100     MOVE CURRENT-YEAR   TO RPT-YY.                               00044100
044200     MOVE CURRENT-MONTH  TO RPT-MM.                               00044200
044300     MOVE CURRENT-DAY    TO RPT-DD.                               00044300
044400     MOVE CURRENT-HOUR   TO RPT-HH.                               00044400
044500     MOVE CURRENT-MINUTE TO RPT-MIN.                              00044500
044600     MOVE CURRENT-SECOND TO RPT-SS.                               00044600
044700     WRITE REPORT-RECORD FROM RPT-HEADER1 AFTER PAGE.             00044700
044800                                                                  00044800
044900 830-REPORT-TRAN-PROCESSED.                                       00044900
045000     MOVE TRANSACTION-RECORD TO RPT-TRAN-RECORD.                  00045000
045100     IF TRAN-COMMENT = '*'                                        00045100
045200         MOVE SPACES TO RPT-TRAN-MSG1                             00045200
045300     ELSE                                                         00045300
045400         MOVE '       Transaction processed: ' TO RPT-TRAN-MSG1   00045400
045500     END-IF.                                                      00045500
045600     WRITE REPORT-RECORD FROM RPT-TRAN-DETAIL1.                   00045600
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
