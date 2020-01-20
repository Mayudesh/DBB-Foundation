000100 ID DIVISION.                                                     11/02/89
000200 PROGRAM-ID. B99100.                                                 LV025
000300                                                                     CL*24
000400 AUTHOR.         TRAINING4                                           CL*24
000500 INSTALLATION.   RALEIGH.                                            CL*24
000600 DATE-WRITTEN.   JUNE 1982.                                          CL*24
000700                                                                     CL*24
000800******************************************************************   CL*24
000900*REMARKS.                                                            CL*24
001000*    THIS PROGRAM WILL PROVIDE BATCH MAINTENANCE OF THE              CL*24
001100*    VADDRSEG SEGMENT ON THE VPARTSUP DATABASE VIA THE               CL*24
001200*    INPUT ACTIVITY FILE.                                            CL*24
001300*    INPUT.  ADDRESS DISK INPUT FILE.                                CL*24
001400*    INPUT.  VPARTSUP DATABASE - VB99005  PSB                        CL*24
001500*    OUTPUT. VPARTSUP DATABASE VADDRSEG & VSUPPSEG UPDATED           CL*24
001900 ENVIRONMENT DIVISION.                                               CL*24
002000                                                                     CL*24
002100 CONFIGURATION SECTION.                                              CL*24
002200 SOURCE-COMPUTER. IBM-3081.                                          CL*24
002300 OBJECT-COMPUTER. IBM-3081.                                          CL*24
002400                                                                     CL*24
002500 INPUT-OUTPUT SECTION.                                               CL*24
002600 FILE-CONTROL.                                                       CL*24
002700     SELECT INPUT-ADDRESS-FILE ASSIGN TO UT-S-B99100.
002800 DATA DIVISION.                                                      CL*24
002900 FILE SECTION.                                                       CL*24
003000 FD  INPUT-ADDRESS-FILE                                              CL*24
003100     RECORDING MODE IS F                                             CL*24
003200     LABEL RECORDS ARE STANDARD                                      CL*24
003300     RECORD CONTAINS 100 CHARACTERS                                  CL*24
003400     BLOCK CONTAINS 0 RECORDS                                        CL*24
003500     DATA RECORD IS INPUT-RECORD.                                    CL*24
003600 01  INPUT-RECORD                   PIC X(11020).                    CL*24
003700                                                                     CL*24
003800 WORKING-STORAGE SECTION.                                            CL*24
003900 01  FILLER                    PIC X(32) VALUE                       CL*24
004000        '* WORKING STORAGE BEGINS HERE *'.                           CL*24
004100                                                                     CL*24
004200 01  FILLER                    PIC X(32) VALUE                       CL*24
004300             '****** DUMP MSG ****************'.                     CL*24
004400*****************************************************************
004500*    DUMP POINTER AREA
004600*        PARA POINTER- MOVE PARAGRAPH NUMBER TO THIS POINTER    *
004700*                      AS EACH PARAGRAPH IS ENTERED. DO NOT     *
004800*                      MOVE PARAGRAPH NUMBERS OF COMMON
004900*                      PARAGRAPHS (USE COMM POINTER).
005000*                                                               *
005100*        COMM POINTER - EACH COMMON PARAGRAPH SHOULD MOVE       *
005200*                       ITS PARAGRAPH NUMBER TO THIS POINTER    *
005300*                       AT IT INCEPTION.
005400*                                                               *
005500*****************************************************************
005600 01  DUMP-LOCATOR.
005700     05 FILLER             PIC X(32)
005800               VALUE '>>>>>>> WS DUMP POINTERS >>>>>>>'.
005900     05 FILLER             PIC X(16)   VALUE 'Z PARA POINTER'.
006000     05 PARA-POINTER       PIC X(8)    VALUE SPACES.
006100     05 FILLER             PIC X(8)    VALUE '       Z'.
006200     05 FILLER             PIC X(16)   VALUE 'Z COMM POINTER'.
006300     05 COMM-POINTER       PIC X(8)    VALUE SPACES.
006400     05 FILLER             PIC X(32)
006500                   VALUE '<<<<<<< WS DUMP POINTERS <<<<<<<'.
006600     05 MODULE-POINTER     PIC X(8)    VALUE SPACES.
006700
006800 01  DUMP-DISPLAY.                                                   CL*24
006900     05 DUMP-STATUS                     PIC X(3)  VALUE SPACES.      CL*24
007000     05 DUMP-MESSAGE                    PIC X(61) VALUE 'NO MSG'.    CL*24
007100                                                                     CL*24
007200******************************************************************   CL*24
007300*                                                                *   CL*24
007400*            INPUT FILE DESCRIPTION                              *   CL*24
007500*                                                                *   CL*24
007600******************************************************************   CL*24
007700 01  INPUT-ADDRESS-RECORD.                                           CL*24
007800     05  INPUT-PART-NUMBER          PIC X(23)   VALUE SPACES.        CL*24
007900     05  INPUT-SUPPLIER-CODE        PIC X(05)   VALUE SPACES.        CL*24
008000     05  INPUT-ADDRESS-TYPE         PIC X(01)   VALUE SPACES.        CL*24
008100     05  INPUT-ADDRESS-1            PIC X(15)   VALUE SPACES.        CL*24
008200     05  INPUT-ADDRESS-2            PIC X(15)   VALUE SPACES.        CL*24
008300     05  INPUT-ADDRESS-3            PIC X(15)   VALUE SPACES.        CL*24
008400     05  INPUT-CITY                 PIC X(10)   VALUE SPACES.        CL*24
008500     05  INPUT-STATE                PIC X(02)   VALUE SPACES.        CL*24
008600     05  INPUT-ZIP-CODE             PIC 9(05)   VALUE ZEROS.         CL*24
008700     05  FILLER                     PIC X(08)   VALUE SPACES.        CL*24
008800     05  INPUT-ACTIVITY-CODE        PIC X(01)   VALUE SPACES.        CL*24
008900         88  INPUT-ADD-RECORD                   VALUE 'A'.           CL*24
009000         88  INPUT-CHANGE-RECORD                VALUE 'C'.           CL*24
009100         88  INPUT-DELETE-RECORD                VALUE 'D'.           CL*24
009200                                                                     CL*24
009300                                                                     CL*24
009400******************************************************************   CL*24
009500*                                                                *   CL*24
009600*            INPUT/OUTPUT AREAS FOR VPARTSUP DATABASE            *   CL*24
009700*                                                                *   CL*24
009800******************************************************************   CL*24
009900 01  GENERAL-IO-AREA             PIC X(100).                         CL*24
010000 01  VPARTSUP-IO-AREAS.                                              CL*24
010100     05  FILLER                  PIC X(32)  VALUE                    CL*24
010200             '** VPARTSUP DETAIL IO AREA **'.                        CL*24
010300 01  VPARTSEG-IO-AREA.
010400     05  VPARTSEG-PART-NUMBER       PIC X(23) VALUE SPACES.
010500     05  VPARTSEG-PART-NAME         PIC X(14) VALUE SPACES.
010600     05  VPARTSEG-SPEC-NUMBER       PIC X(07) VALUE SPACES.
010700     05  VPARTSEG-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
010800     05  VPARTSEG-BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
010900     05  VPARTSEG-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
011000     05  VPARTSEG-WEEKS-LEAD-TIME   PIC 9(03) VALUE 0.
011100     05  VPARTSEG-ENGINE-MODEL      PIC X(05) VALUE SPACES.
011200     05  FILLER                     PIC X(14) VALUE SPACES.
011300
011400***INCLUDE VSUPPIO
011500 01  VSUPPSEG-IO-AREA.
011600     05  VSUPPSEG-SUPPLIER-CODE     PIC X(05) VALUE SPACES.
011700     05  VSUPPSEG-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
011800     05  VSUPPSEG-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
011900     05  VSUPPSEG-SUPPLIER-PERF     PIC 9(03) VALUE 0.
012000     05  VSUPPSEG-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
012100     05  VSUPPSEG-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
012200     05  VSUPPSEG-SUPPLIER-ACT-DATE PIC 9(05) VALUE 0.
012300     05  FILLER                     PIC X(07) VALUE SPACES.
012400
012500***INCLUDE VADDRIO
012600 01  VADDRSEG-IO-AREA.
012700     05  FILLER                     PIC X(03) VALUE SPACES.
012800     05  VADDRSEG-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
012900         88  VADDRSEG-ORDER-ADDRESS           VALUE '1'.
013000         88  VADDRSEG-SCHED-ADDRESS           VALUE '2'.
013100         88  VADDRSEG-REMIT-ADDRESS           VALUE '3'.
013200     05  VADDRSEG-ADDRESS-1         PIC X(15) VALUE SPACES.
013300     05  VADDRSEG-ADDRESS-2         PIC X(15) VALUE SPACES.
013400     05  VADDRSEG-ADDRESS-3         PIC X(15) VALUE SPACES.
013500     05  VADDRSEG-CITY              PIC X(10) VALUE SPACES.
013600     05  VADDRSEG-STATE             PIC X(02) VALUE SPACES.
013700     05  VADDRSEG-ZIP-CODE          PIC 9(05) VALUE 0.
013800     05  FILLER                     PIC X(08) VALUE SPACES.
013900
014000***INCLUDE VPOIO
014100 01  VPOSEG-IO-AREA.
014200     05  VPOSEG-PO-NUMBER           PIC X(06) VALUE SPACES.
014300     05  VPOSEG-BUYER-CODE          PIC X(03) VALUE SPACES.
014400     05  VPOSEG-QUANTITY            PIC S9(7) VALUE +0.
014500     05  VPOSEG-UNIT-PRICE          PIC S9(7)V99 VALUE +0.
014600     05  VPOSEG-ORDER-DATE          PIC 9(05) VALUE 0.
014700     05  VPOSEG-DELIVERY-DATE       PIC 9(05) VALUE 0.
014800
014900******************************************************************
015000*                                                                *
015100*            SSA AREAS FOR VPARTSUP DATABASE
015200*                                                                *
015300******************************************************************
015400 01  VPARTSUP-SSAS.
015500     05  FILLER                           PIC X(32)  VALUE
015600                 '**** VPARTSUP SSA AREA ****'.
015700***INCLUDE VPARTSSA
015800 01  SSA-VPARTSEG.
015900     05  VPARTSEG-SEG-NAME          PIC X(8)  VALUE 'VPARTSEG'.
016000     05  VPARTSEG-COMM-ASTR         PIC X     VALUE '*'.
016100     05  VPARTSEG-COMM-CODE         PIC X(3)  VALUE '---'.
016200     05  VPARTSEG-L-PAREN           PIC X     VALUE '('.
016300     05  VPARTSEG-KEY-FLD           PIC X(8)  VALUE 'VPARTKEY'.
016400     05  VPARTSEG-OPERATOR-1        PIC XX    VALUE ' ='.
016500     05  VPARTSEG-KEY               PIC X(23) VALUE SPACES.
016600     05  VPARTSEG-R-PAREN           PIC X     VALUE ')'.
016700
016800***INCLUDE VSUPPSSA
016900 01  SSA-VSUPPSEG.
017000     05  VSUPPSEG-SEG-NAME          PIC X(8)  VALUE 'VSUPPSEG'.
017100     05  VSUPPSEG-COMM-ASTR         PIC X     VALUE '*'.
017200     05  VSUPPSEG-COMM-CODE         PIC X(3)  VALUE '---'.
017300     05  VSUPPSEG-L-PAREN           PIC X     VALUE '('.
017400     05  VSUPPSEG-KEY-FLD           PIC X(8)  VALUE 'VSUPPKEY'.
017500     05  VSUPPSEG-OPERATOR-1        PIC XX    VALUE ' ='.
017600     05  VSUPPSEG-KEY               PIC X(05) VALUE SPACES.
017700     05  VSUPPSEG-R-PAREN           PIC X     VALUE ')'.
017800
017900
018000***INCLUDE VADDRSSA
018100 01  SSA-VADDRSEG.
018200     05  VADDRSEG-SEG-NAME          PIC X(8)  VALUE 'VADDRSEG'.
018300     05  VADDRSEG-COMM-ASTR         PIC X     VALUE '*'.
018400     05  VADDRSEG-COMM-CODE         PIC X(3)  VALUE '---'.
018500     05  VADDRSEG-L-PAREN           PIC X     VALUE '('.
018600     05  VADDRSEG-KEY-FLD           PIC X(8)  VALUE 'VADDRKEY'.
018700     05  VADDRSEG-OPERATOR-1        PIC XX    VALUE ' ='.
018800     05  VADDRSEG-KEY               PIC X(01) VALUE SPACES.
018900     05  VADDRSEG-R-PAREN           PIC X     VALUE ')'.
019000
019100***INCLUDE VPOSSA
019200 01  SSA-VPOSEG.
019300     05  VPOSEG-SEG-NAME            PIC X(8)  VALUE 'VPOSEG'.
019400     05  VPOSEG-COMM-ASTR           PIC X     VALUE '*'.
019500     05  VPOSEG-COMM-CODE           PIC X(3)  VALUE '---'.
019600     05  VPOSEG-L-PAREN             PIC X     VALUE '('.
019700     05  VPOSEG-KEY-FLD             PIC X(8)  VALUE 'VPONOKEY'.
019800     05  VPOSEG-OPERATOR-1          PIC XX    VALUE ' ='.
019900     05  VPOSEG-KEY                 PIC X(06) VALUE SPACES.
020000     05  VPOSEG-CONNECTOR           PIC X     VALUE ')'.
020100     05  VPOSEG-KEY-FLD-NAME-2      PIC X(8)  VALUE 'VPONOKEY'.
020200     05  VPOSEG-OPERATOR-2          PIC XX    VALUE ' ='.
020300     05  VPOSEG-KEY-2               PIC X(06) VALUE SPACES.
020400     05  VPOSEG-R-PAREN             PIC X     VALUE ')'.
020500
020600******************************************************************
020700*                                                                *
020800*            STANDARD IMS VALUES USED IN CALLS
020900*                                                                *
021000******************************************************************
021100***INCLUDE IMSVALUE
021200 01  IMS-VALUES.
021300     05  EQUAL-TO                PIC XX           VALUE ' ='.
021400     05  GREATER-THAN            PIC XX           VALUE ' >'.
021500     05  LESS-THAN               PIC XX           VALUE ' <'.
021600     05  EQUAL-OR-GREATER        PIC XX           VALUE '=>'.
021700     05  EQUAL-OR-LESS           PIC XX           VALUE '=<'.
021800     05  NOT-EQUAL               PIC XX           VALUE '='.
021900     05  NOT-GREATER             PIC XX           VALUE '>'.
022000     05  NOT-LESS                PIC XX           VALUE '<'.
022100     05  BOOLEAN-AND             PIC X           VALUE '*'.
022200     05  BOOLEAN-INDP-AND        PIC X           VALUE '#'.
022300     05  BOOLEAN-OR              PIC X           VALUE '+'.
022400     05  LEFT-PAREN              PIC X           VALUE '('.
022500     05  RIGHT-PAREN             PIC X           VALUE ')'.
022600
022700 01  MFS-VALUES.
022800   02  MFS-ATTR-VALUES.
022900     05  HEX-0080 PIC S9(8) COMP VALUE +128.
023000     05  FILLER                       REDEFINES   HEX-0080.
023100         10  FILLER                   PIC XX.
023200         10  NO-ATTR                  PIC XX.
023300     05  HEX-C080 PIC S9(8) COMP VALUE +49280.
023400     05  FILLER                       REDEFINES   HEX-C080.
023500         10  FILLER                   PIC XX.
023600         10  CURSOR-1                 PIC XX.
023700     05  HEX-C088 PIC S9(8) COMP VALUE +49288.
023800     05  FILLER                       REDEFINES   HEX-C088.
023900         10  FILLER                   PIC XX.
024000         10  CURSOR-HI                PIC XX.
024100     05  HEX-0088 PIC S9(8) COMP VALUE +136.
024200     05  FILLER                       REDEFINES   HEX-0088.
024300         10  FILLER                   PIC XX.
024400         10  HI-LITE                  PIC XX.
024500     05  HEX-00B0 PIC S9(8) COMP VALUE +176.
024600     05  FILLER                       REDEFINES   HEX-00B0.
024700         10  FILLER                   PIC XX.
024800         10  NUM-PROT-NOREPL          PIC XX.
024900     05  HEX-C0C1 PIC S9(8) COMP VALUE +49345.
025000     05  FILLER                       REDEFINES   HEX-C0C1.
025100         10  FILLER                   PIC XX.
025200         10  ALPHA-UNPROT-MOD-CUR     PIC XX.
025300     05  HEX-00C1 PIC S9(8) COMP VALUE +193.
025400     05  FILLER                       REDEFINES   HEX-00C1.
025500         10  FILLER                   PIC XX.
025600         10  ALPHA-UNPROT-MOD         PIC XX.
025700     05  HEX-00C2 PIC S9(8) COMP VALUE +194.
025800     05  FILLER                       REDEFINES   HEX-00C2.
025900         10  FILLER                   PIC XX.
026000         10  ALPHA-UNPROT-NORM        PIC XX.
026100     05  HEX-C0C2 PIC S9(8) COMP VALUE +49346.
026200     05  FILLER                       REDEFINES   HEX-C0C2.
026300         10  FILLER                   PIC XX.
026400         10  ALPHA-UNPROT-NORM-CUR    PIC XX.
026500     05  HEX-C0C7 PIC S9(8) COMP VALUE +49351.
026600     05  FILLER                       REDEFINES   HEX-C0C7.
026700         10  FILLER                   PIC XX.
026800         10  ALPHA-UNPROT-NODISP-MOD-CUR PIC XX.
026900     05  HEX-00C9 PIC S9(8) COMP VALUE +201.
027000     05  FILLER                       REDEFINES   HEX-00C9.
027100         10  FILLER                   PIC XX.
027200         10  ALPHA-UNPROT-HI-MOD      PIC XX.
027300     05  HEX-C0C9 PIC S9(8) COMP VALUE +49353.
027400     05  FILLER                       REDEFINES   HEX-C0C9.
027500         10  FILLER                   PIC XX.
027600         10  ALPHA-UNPROT-HI-MOD-CUR  PIC XX.
027700     05  HEX-00D1 PIC S9(8) COMP VALUE +209.
027800     05  FILLER                       REDEFINES   HEX-00D1.
027900         10  FILLER                   PIC XX.
028000         10  NUM-UNPROT-MOD           PIC XX.
028100     05  HEX-00D2 PIC S9(8) COMP VALUE +210.
028200     05  FILLER                       REDEFINES   HEX-00D2.
028300         10  FILLER                   PIC XX.
028400         10  NUM-UNPROT-NORM          PIC XX.
028500     05  HEX-C0D2 PIC S9(8) COMP VALUE +49362.
028600     05  FILLER                       REDEFINES   HEX-C0D2.
028700         10  FILLER                   PIC XX.
028800         10  NUM-UNPROT-NORM-CUR      PIC XX.
028900     05  HEX-00D5 PIC S9(8) COMP VALUE +213.
029000     05  FILLER                       REDEFINES   HEX-00D5.
029100         10  FILLER                   PIC XX.
029200         10  NUM-UNPROT-NDIS-MOD      PIC XX.
029300     05  HEX-00D9 PIC S9(8) COMP VALUE +217.
029400     05  FILLER                       REDEFINES   HEX-00D9.
029500         10  FILLER                   PIC XX.
029600         10  NUM-UNPROT-HI-MOD        PIC XX.
029700     05  HEX-C0D9 PIC S9(8) COMP VALUE +49369.
029800     05  FILLER                       REDEFINES   HEX-C0D9.
029900         10  FILLER                   PIC XX.
030000         10  NUM-UNPROT-HI-MOD-CUR    PIC XX.
030100     05  HEX-00F1 PIC S9(8) COMP VALUE +241.
030200     05  FILLER                       REDEFINES   HEX-00F1.
030300         10  FILLER                   PIC XX.
030400         10  NUM-PROT-MOD             PIC XX.
030500     05  HEX-E7F1 PIC S9(8) COMP VALUE +59377.
030600     05  FILLER                       REDEFINES   HEX-E7F1.
030700         10  FILLER                   PIC XX.
030800         10  REPL-NUM-PROT-MOD        PIC XX.
030900     05  HEX-00F2 PIC S9(8) COMP VALUE +242.
031000     05  FILLER                       REDEFINES   HEX-00F2.
031100         10  FILLER                   PIC XX.
031200         10  NUM-PROT-NORM            PIC XX.
031300     05  HEX-00F4 PIC S9(8) COMP VALUE +244.
031400     05  FILLER                       REDEFINES   HEX-00F4.
031500         10  FILLER                   PIC XX.
031600         10  NUM-PROT-NONDSP          PIC XX.
031700     05  HEX-00F5 PIC S9(8) COMP VALUE +245.
031800     05  FILLER                       REDEFINES   HEX-00F5.
031900         10  FILLER                   PIC XX.
032000         10  NUM-PROT-NDIS-MOD        PIC XX.
032100     05  HEX-00F8 PIC S9(8) COMP VALUE +248.
032200     05  FILLER                       REDEFINES   HEX-00F8.
032300         10  FILLER                   PIC XX.
032400         10  NUM-PROT-HI              PIC XX.
032500     05  HEX-00F9 PIC S9(8) COMP VALUE +249.
032600     05  FILLER                       REDEFINES   HEX-00F9.
032700         10  FILLER                   PIC XX.
032800         10  NUM-PROT-HI-MOD          PIC XX.
032900     05  HEX-0D3F PIC S9(8) COMP VALUE +3391.
033000     05  FILLER                       REDEFINES   HEX-0D3F.
033100         10  FILLER                   PIC XX.
033200         10  NO-TRANSMIT-ERASE.
033300             15  FILLER               PIC X.
033400             15  NO-TRANSMIT          PIC X.
033500   02  MFS-EATTR-VALUES.
033600     05  HEX-C100 PIC S9(8) COMP VALUE +49408.
033700     05  FILLER                       REDEFINES   HEX-C100.
033800         10  FILLER              PIC XX.
033900         10  EATTR-HI-NORM       PIC XX.
034000     05  EATTR-HI-BLINK          PIC XX           VALUE 'A1'.
034100     05  EATTR-HI-REVERSE        PIC XX           VALUE 'A2'.
034200     05  EATTR-HI-UNDERSCORE     PIC XX           VALUE 'A4'.
034300     05  HEX-C200 PIC S9(8) COMP VALUE +49664.
034400     05  FILLER                       REDEFINES   HEX-C200.
034500         10  FILLER              PIC XX.
034600         10  EATTR-COLOR-NORM    PIC XX.
034700     05  EATTR-COLOR-BLUE        PIC XX           VALUE 'B1'.
034800     05  EATTR-COLOR-RED         PIC XX           VALUE 'B2'.
034900     05  EATTR-COLOR-PINK        PIC XX           VALUE 'B3'.
035000     05  EATTR-COLOR-GREEN       PIC XX           VALUE 'B4'.
035100     05  EATTR-COLOR-TURQ        PIC XX           VALUE 'B5'.
035200     05  EATTR-COLOR-YELLOW      PIC XX           VALUE 'B6'.
035300     05  EATTR-COLOR-NEUTRAL     PIC XX           VALUE 'B7'.
035400
035500******************************************************************
035600*                                                                *
035700*            STANDARD CALL FUNCTIONS USED IN CALLS               *
035800*                                                                *
035900******************************************************************
036000*******IMSFUNCT REALLY IS DLIFUNCT, HERE IT IS:
036100
036200 01  DLI-CALL-FUNCTIONS.
036300     02  DB-CALL-FUNCTIONS.
036400         05  GU-FUNC           PIC X(4)    VALUE 'GU  '.
036500         05  GN-FUNC           PIC X(4)    VALUE 'GN  '.
036600         05  GHU-FUNC          PIC X(4)    VALUE 'GHU '.
036700         05  GHN-FUNC          PIC X(4)    VALUE 'GHN '.
036800         05  GNP-FUNC          PIC X(4)    VALUE 'GNP '.
036900         05  GHNP-FUNC         PIC X(4)    VALUE 'GHNP'.
037000         05  ISRT-FUNC         PIC X(4)    VALUE 'ISRT'.
037100         05  REPL-FUNC         PIC X(4)    VALUE 'REPL'.
037200         05  DLET-FUNC         PIC X(4)    VALUE 'DLET'.
037300     02  DC-CALL-FUNCTIONS.
037400         05  CMD-FUNC          PIC X(4)    VALUE 'CMD '.
037500         05  GCMD-FUNC         PIC X(4)    VALUE 'GCMD'.
037600         05  PURG-FUNC         PIC X(4)    VALUE 'PURG'.
037700         05  CHNG-FUNC         PIC X(4)    VALUE 'CHNG'.
037800
037900 01  IMS-WORK-AREA.                                                  CL*24
038000     05  PARM-CT                 PIC  S9(5) COMP SYNC VALUE +3.      CL*24
038100     05  PSBPGM-NAME             PIC  X(8) VALUE 'B99100 '.          CL*24
038200     05  DUMP-OPT                PIC  X    VALUE 'F'.                CL*24
038300     05  TIME-TO-END             PIC  X    VALUE 'N'.                CL*25
038400     05  CALL-FUNCTION           PIC  X(4) VALUE SPACES.             CL*24
038500                                                                     CL*24
038600 01  DATE-PARMS.                                                     CL*24
038700     05 DATE-JULIAN           PIC X(3).                              CL*24
038800     05 DATE-GREGORIAN.                                              CL*24
038900        10 DATE-GREG-MM       PIC X(2).                              CL*24
039000        10 DATE-GREG-DD       PIC X(2).                              CL*24
039100        10 DATE-GREG-YY       PIC X(2).                              CL*24
039200                                                                     CL*24
039300******************************************************************   CL*24
039400*                                                                *   CL*24
039500*            WORKING STORAGE HOLD AREAS                          *   CL*24
039600*                                                                *   CL*24
039700******************************************************************   CL*24
039800 01  WS1-PGM-SWITCHES.                                               CL*24
039900     05 WS1-EOF-INDICATOR        PIC X      VALUE ' '.               CL*24
040000        88 WS1-END-OF-FILE                  VALUE 'Y'.               CL*24
040100                                                                     CL*24
040200 01  WS2-HOLD-QTYS-AREA.                                             CL*24
040300     05 WS2-INPUT-COUNT        PIC S9(5) VALUE +0 COMP-3.            CL*24
040400     05 WS2-ADD-RECORD-COUNT     PIC S9(5) VALUE +0 COMP-3.          CL*24
040500     05 WS2-CHG-RECORD-COUNT     PIC S9(5) VALUE +0 COMP-3.          CL*24
040600     05 WS2-DEL-RECORD-COUNT     PIC S9(5) VALUE +0 COMP-3.          CL*24
040700     05 WS2-ERR-RECORD-COUNT     PIC S9(5) VALUE +0 COMP-3.          CL*24
040800                                                                     CL*24
040900 01  WS3-PGM-MESSAGES.                                               CL*24
041000     05  WS3-ERROR-FLAG               PIC X(01)   VALUE ' '.         CL*24
041100         88  WS3-ERROR-FOUND                      VALUE 'Y'.         CL*24
041200     05  WS3-ERROR-MESSAGE-1          PIC X(79)   VALUE              CL*24
041300        'INVALID ACTIVITY CODE ON INPUT'.                            CL*24
041400                                                                     CL*24
041500 01  WS4-EDIT-FIELDS.                                                CL*24
041600     05  WS4-EDIT-INPUT-COUNT         PIC ZZZZ9 VALUE ZEROS.         CL*24
041700     05  WS4-EDIT-ADD-COUNT           PIC ZZZZ9 VALUE ZEROS.         CL*24
041800     05  WS4-EDIT-CHG-COUNT           PIC ZZZZ9 VALUE ZEROS.         CL*24
041900     05  WS4-EDIT-DEL-COUNT           PIC ZZZZ9 VALUE ZEROS.         CL*24
042000     05  WS4-EDIT-ERR-COUNT           PIC ZZZZ9 VALUE ZEROS.         CL*24
042100                                                                     CL*24
042200 LINKAGE SECTION.                                                    CL*24
042300 01  IO-TERMINAL-PCB                   SYNC.
042400     05  IO-TERMINAL-NAME              PIC X(8).
042500     05  IO-RESERVED                   PIC XX.
042600     05  IO-STATUS-CODE                PIC XX.
042700         88  IO-CALL-SUCCESSFUL      VALUE '  '.
042800         88  IO-NOMORE-MSG-SEGMENTS  VALUE 'QD'.
042900         88  IO-NOMORE-MESSAGES      VALUE 'QC'.
043000     05  IO-PREFIX.
043100         10  IO-JULIAN-DATE           PIC S9(7) COMP-3.
043200         10  IO-TIME-OF-DAY           PIC S9(7) COMP-3.
043300         10  IO-MESSAGE-SEQ           PIC S9(3) COMP.
043400         10  FILLER                   PIC XX.
043500
043600******************************************************************
043700*  THIS ALT-IO-PCB IS USED IN THIS PROGRAM FOR MESSAGE
043800*  SWITCHING.
043900******************************************************************
044000 01  ALT-IO-PCB                   SYNC.
044100     05  ALT-IO-DESTINATION               PIC X(8).
044200     05  ALT-IO-RESERVED                  PIC XX.
044300     05  ALT-IO-STATUS-CODE               PIC XX.
044400         88  ALT-IO-CALL-SUCCESSFUL      VALUE '  '.
044500
044600**************************************************************       CL*24
044700*                                                            *       CL*24
044800*       L I N K A G E   S E C T I O N                        *       CL*24
044900*                                                            *       CL*24
045000**************************************************************       CL*24
045100                                                                     CL*24
045200******************************************************************   CL*24
045300*                                                                *   CL*24
045400*       VPARTSUP DATABASE PCB                                    *   CL*24
045500*                                                                *   CL*24
045600******************************************************************   CL*24
045700 01  VPARTSUP-PCB.
045800     05  VPARTSUP-DBD-NAME           PIC X(08).
045900     05  VPARTSUP-SEGMENT-LEVEL      PIC X(02).
046000     05  VPARTSUP-STATUS-CODE        PIC X(02).
046100         88 VPARTSUP-SUCCESSFUL-CALL      VALUE '  ' 'GA' 'GK'.
046200         88 VPARTSUP-SEGMENT-NOT-FOUND    VALUE 'GE'.
046300         88 VPARTSUP-END-OF-DB            VALUE 'GB'.
046400         88 VPARTSUP-DUPLICATE-KEY        VALUE 'II'.
046500         88 VPARTSUP-NON-FATAL-ERROR      VALUE 'II' 'GA' '  '
046600                                                'GB' 'GE' 'GK'.
046700     05  VPARTSUP-PROC-OPT           PIC X(04).
046800     05  VPARTSUP-RESERVE-DLI           PIC S9(5) COMP.
046900     05  VPARTSUP-SEGMENT-NAME           PIC X(08).
047000     05  VPARTSUP-KEY-LENGTH           PIC S9(5) COMP.
047100     05  VPARTSUP-NO-SEN-SEGS           PIC S9(5) COMP.
047200     05  VPARTSUP-KEY-FEEDBACK           PIC X(34).
047300     05  VPARTSUP-LEVEL-1-NAME           PIC X(08).
047400                                                                     CL*24
047500******************************************************************   CL*24
047600*                                                                *   CL*24
047700*       P R O C E D U R E    D I V I S I O N                     *   CL*24
047800*                                                                *   CL*24
047900******************************************************************   CL*24
048000 PROCEDURE DIVISION.                                                 CL*24
048100                                                                     CL*24
048200     ENTRY 'DLITCBL' USING   IO-TERMINAL-PCB
048300                             ALT-IO-PCB
048400                             VPARTSUP-PCB.
048500                                                                     CL*24
048600     DISPLAY '*** BEGIN PROGRAM B99100 ***'                         CL*24
048700     MOVE 'B99100'   TO MODULE-POINTER.                             CL*24
048800     DISPLAY SPACES.                                                 CL*24
048900                                                                     CL*24
049000     OPEN INPUT INPUT-ADDRESS-FILE.                                  CL*24
049100                                                                     CL*24
049200     MOVE 'CURRDATE' TO MODULE-POINTER.                              CL*24
049300*    CALL 'CURRDATE' USING DATE-PARMS.                               CL*24
049400     MOVE 'B99100'   TO MODULE-POINTER.                             CL*24
049500                                                                     CL*24
049600**************************************************************       CL*24
049700     PERFORM 000-READ-INPUT-FILE THRU 000-EXIT.                      CL*24
049800                                                                     CL*24
049900     PERFORM 005-MAINLINE THRU 005-EXIT UNTIL                        CL*24
050000         WS1-END-OF-FILE.                                            CL*24
050100**************************************************************       CL*24
050200                                                                     CL*24
050300     CLOSE INPUT-ADDRESS-FILE.                                       CL*24
050400                                                                     CL*24
050500     MOVE WS2-INPUT-COUNT      TO WS4-EDIT-INPUT-COUNT.              CL*24
050600     MOVE WS2-ADD-RECORD-COUNT TO WS4-EDIT-ADD-COUNT.                CL*24
050700     MOVE WS2-CHG-RECORD-COUNT TO WS4-EDIT-CHG-COUNT.                CL*24
050800     MOVE WS2-DEL-RECORD-COUNT TO WS4-EDIT-DEL-COUNT.                CL*24
050900     MOVE WS2-ERR-RECORD-COUNT TO WS4-EDIT-ERR-COUNT.                CL*24
051000                                                                     CL*24
051100     DISPLAY 'INPUT RECORDS READ          ' WS4-EDIT-INPUT-COUNT.    CL*24
051200     DISPLAY 'INPUT RECORDS ADDED TO DB   ' WS4-EDIT-ADD-COUNT.      CL*24
051300     DISPLAY 'INPUT RECORDS CHANGED ON DB ' WS4-EDIT-CHG-COUNT.      CL*24
051400     DISPLAY 'INPUT RECORDS DELETED ON DB ' WS4-EDIT-DEL-COUNT.      CL*24
051500     DISPLAY 'INPUT RECORDS IN ERROR      ' WS4-EDIT-ERR-COUNT.      CL*24
051600     DISPLAY SPACES.                                                 CL*24
051700     DISPLAY '*** END PROGRAM B99100 ***'.                           CL*24
051800                                                                     CL*24
051900     MOVE ZERO TO RETURN-CODE.                                       CL*24
052000     GOBACK.                                                         CL*24
052100                                                                     CL*24
052200 000-READ-INPUT-FILE.                                                CL*24
052300     MOVE 000 TO PARA-POINTER.                                       CL*24
052400                                                                     CL*24
052500     READ INPUT-ADDRESS-FILE INTO INPUT-ADDRESS-RECORD               CL*24
052600         AT END                                                      CL*24
052700         MOVE 'Y' TO WS1-EOF-INDICATOR                               CL*24
052800         GO TO 000-EXIT.                                             CL*24
052900     ADD +1 TO WS2-INPUT-COUNT.                                      CL*24
053000 000-EXIT.                                                           CL*24
053100     EXIT.                                                           CL*24
053200                                                                     CL*24
053300 005-MAINLINE.                                                       CL*24
053400     MOVE 005    TO PARA-POINTER.                                    CL*24
053500                                                                     CL*24
053600     PERFORM 010-EDIT-CHECKS THRU 010-EXIT.                          CL*24
053700                                                                     CL*24
053800     PERFORM 020-COMPARE-TO-DB-ROUTINE THRU 020-EXIT.                CL*24
053900                                                                     CL*24
054000     PERFORM 000-READ-INPUT-FILE THRU 000-EXIT.                      CL*24
054100 005-EXIT.                                                           CL*24
054200     EXIT.                                                           CL*24
054300
054400 010-EDIT-CHECKS.                                                    CL*24
054500     MOVE 010  TO PARA-POINTER.                                      CL*24
054600                                                                     CL*24
054700     MOVE ' '  TO WS3-ERROR-FLAG.                                    CL*24
054800                                                                     CL*24
054900     IF INPUT-ACTIVITY-CODE = 'A' OR 'C' OR 'D'                      CL*24
055000       THEN                                                          CL*24
055100         NEXT SENTENCE                                               CL*24
055200     ELSE                                                            CL*24
055300         DISPLAY WS3-ERROR-MESSAGE-1                                 CL*24
055400         DISPLAY INPUT-ADDRESS-RECORD                                CL*24
055500         ADD +1 TO WS2-ERR-RECORD-COUNT                              CL*24
055600         MOVE 'Y' TO WS3-ERROR-FLAG                                  CL*24
055700         GO TO 010-EXIT.                                             CL*24
055800 010-EXIT. EXIT.                                                     CL*24
055900                                                                     CL*24
056000                                                                     CL*24
056100 020-COMPARE-TO-DB-ROUTINE.                                          CL*24
056200     MOVE 020 TO PARA-POINTER.                                       CL*24
056300                                                                     CL*24
056400     IF WS3-ERROR-FOUND                                              CL*24
056500         GO TO 020-EXIT.                                             CL*24
056600                                                                     CL*24
056700     IF INPUT-ADD-RECORD                                             CL*24
056800         PERFORM 100-ADD-VADDRSEG-ROUTINE THRU 100-EXIT              CL*24
056900         GO TO 020-EXIT.                                             CL*24
057000                                                                     CL*24
057100     IF INPUT-CHANGE-RECORD                                          CL*24
057200         PERFORM 150-CHANGE-VADDRSEG-ROUTINE THRU 150-EXIT           CL*24
057300         GO TO 020-EXIT.                                             CL*24
057400                                                                     CL*24
057500     IF INPUT-DELETE-RECORD                                          CL*24
057600         PERFORM 200-DELETE-VADDRSEG-ROUTINE THRU 200-EXIT           CL*24
057700         GO TO 020-EXIT.                                             CL*24
057800 020-EXIT.                                                           CL*24
057900     EXIT.                                                           CL*24
058000                                                                     CL*24
058100 100-ADD-VADDRSEG-ROUTINE.                                           CL*24
058200     MOVE 100 TO PARA-POINTER.                                       CL*24
058300
058400     MOVE INPUT-ADDRESS-1  TO VADDRSEG-ADDRESS-1.
058500     MOVE INPUT-ADDRESS-2  TO VADDRSEG-ADDRESS-2.
058600     MOVE INPUT-ADDRESS-3  TO VADDRSEG-ADDRESS-3.
058700     MOVE INPUT-CITY       TO VADDRSEG-CITY.
058800     MOVE INPUT-STATE      TO VADDRSEG-STATE.
058900     MOVE INPUT-ZIP-CODE   TO VADDRSEG-ZIP-CODE.
059000     IF INPUT-ADDRESS-TYPE = 'O'                                     CL*24
059100         MOVE '1'              TO VADDRSEG-ADDRESS-TYPE.             CL*24
059200     IF INPUT-ADDRESS-TYPE = 'S'                                     CL*24
059300         MOVE '2'              TO VADDRSEG-ADDRESS-TYPE.             CL*24
059400     IF INPUT-ADDRESS-TYPE = 'R'                                     CL*24
059500         MOVE '3'              TO VADDRSEG-ADDRESS-TYPE.             CL*24
059600
059700     MOVE VADDRSEG-IO-AREA TO GENERAL-IO-AREA                        CL*24
059800     MOVE INPUT-PART-NUMBER    TO VPARTSEG-KEY.                      CL*24
059900     MOVE INPUT-SUPPLIER-CODE  TO VSUPPSEG-KEY.                      CL*24
060000     MOVE ' '                       TO VADDRSEG-L-PAREN.             CL*24
060100     MOVE ISRT-FUNC                 TO CALL-FUNCTION.                CL*24
060200     MOVE +6                        TO PARM-CT.                      CL*24
060300     PERFORM 1000-CALL-VPARTSUP-DB THRU 1000-EXIT.                   CL*24
060400                                                                     CL*24
060500                                                                     CL*24
060600 100-EXIT.                                                           CL*24
060700     EXIT.                                                           CL*24
060800                                                                     CL*24
060900 150-CHANGE-VADDRSEG-ROUTINE.                                        CL*24
061000     MOVE 150 TO PARA-POINTER.                                       CL*24
061100                                                                     CL*24
061200     PERFORM 300-GHU-VADDRSEG-ROUTINE THRU 300-EXIT.
061300                                                                     CL*24
061400
061500     MOVE INPUT-ADDRESS-1  TO VADDRSEG-ADDRESS-1.
061600     MOVE INPUT-ADDRESS-2  TO VADDRSEG-ADDRESS-2.
061700     MOVE INPUT-ADDRESS-3  TO VADDRSEG-ADDRESS-3.
061800     MOVE INPUT-CITY       TO VADDRSEG-CITY.
061900     MOVE INPUT-STATE      TO VADDRSEG-STATE.
062000     MOVE INPUT-ZIP-CODE   TO VADDRSEG-ZIP-CODE.
062100
062200     MOVE VADDRSEG-IO-AREA TO GENERAL-IO-AREA.
062300     MOVE REPL-FUNC        TO CALL-FUNCTION.
062400     MOVE +3               TO PARM-CT.
062500     PERFORM 1000-CALL-VPARTSUP-DB THRU 1000-EXIT.                   CL*24
062600 150-EXIT.                                                           CL*24
062700     EXIT.                                                           CL*24
062800                                                                     CL*24
062900 200-DELETE-VADDRSEG-ROUTINE.                                        CL*24
063000     MOVE 200 TO PARA-POINTER.                                       CL*24
063100                                                                     CL*24
063200     PERFORM 300-GHU-VADDRSEG-ROUTINE THRU 300-EXIT.
063300
063400     MOVE DLET-FUNC             TO CALL-FUNCTION.                     CL*2
063500     MOVE +3                   TO PARM-CT.                           CL*24
063600     PERFORM 1000-CALL-VPARTSUP-DB THRU 1000-EXIT.                   CL*24
063700 200-EXIT.                                                           CL*24
063800     EXIT.                                                           CL*24
063900                                                                     CL*24
064000 300-GHU-VADDRSEG-ROUTINE.                                           CL*24
064100     MOVE 300 TO COMM-POINTER.                                       CL*24
064200
064300     MOVE INPUT-PART-NUMBER    TO VPARTSEG-KEY.                      CL*24
064400     MOVE INPUT-SUPPLIER-CODE  TO VSUPPSEG-KEY.                      CL*24
064500     IF INPUT-ADDRESS-TYPE = 'O'                                     CL*24
064600         MOVE '1'              TO VADDRSEG-KEY.                      CL*24
064700     IF INPUT-ADDRESS-TYPE = 'S'                                     CL*24
064800         MOVE '2'              TO VADDRSEG-KEY.                      CL*24
064900     IF INPUT-ADDRESS-TYPE = 'R'                                     CL*24
065000         MOVE '3'              TO VADDRSEG-KEY.                      CL*24
065100                                                                     CL*24
065200     MOVE GHU-FUNC             TO CALL-FUNCTION.                     CL*24
065300     MOVE +6                   TO PARM-CT.                           CL*24
065400     PERFORM 1000-CALL-VPARTSUP-DB THRU 1000-EXIT.                   CL*24
065500
065600     IF VPARTSUP-SUCCESSFUL-CALL                                     CL*24
065700         MOVE GENERAL-IO-AREA  TO VADDRSEG-IO-AREA                   CL*24
065800         GO TO 300-EXIT.                                             CL*24
065900     IF VPARTSUP-SEGMENT-NOT-FOUND OR VPARTSUP-END-OF-DB             CL*24
066000         MOVE 'Y'              TO WS3-ERROR-FLAG                     CL*24
066100         GO TO 300-EXIT.                                             CL*24
066200 300-EXIT.                                                           CL*24
066300     EXIT.                                                           CL*24
066400                                                                     CL*24
066500 1000-CALL-VPARTSUP-DB.                                              CL*24
066600     MOVE 'IMS CALL' TO MODULE-POINTER.                              CL*24
066700                                                                     CL*24
066800******************************************************************   CL*24
066900*                                                                *   CL*24
067000*    VPARTSUP DATABASE CALL USING                                *   CL*24
067100*      1) PARM-CT PARAMETER                                      *   CL*24
067200*      2) GENERAL CALL-FUNCTION                                  *   CL*24
067300*      3) GENERAL I/O AREA                                       *   CL*24
067400*      4) SPECIFIC SSAS FOR EACH SEGMENT RETRIEVED               *   CL*24
067500*                                                                *   CL*24
067600******************************************************************   CL*24
067700     CALL 'CBLTDLI' USING  PARM-CT                                   CL*24
067800                           CALL-FUNCTION                             CL*24
067900                           VPARTSUP-PCB                              CL*24
068000                           GENERAL-IO-AREA                           CL*24
068100                           SSA-VPARTSEG                              CL*24
068200                           SSA-VSUPPSEG                              CL*24
068300                           SSA-VADDRSEG.                             CL*24
068400                                                                     CL*24
068500     MOVE 'B99100'  TO  MODULE-POINTER.                             CL*24
068600     IF VPARTSUP-NON-FATAL-ERROR                                     CL*24
068700         NEXT SENTENCE                                               CL*24
068800     ELSE                                                            CL*24
068900         GO TO 1001-CALL-MASTRMSG-VPARTSUP.                          CL*24
069000                                                                     CL*24
069100 1000-EXIT.                                                          CL*24
069200     EXIT.                                                           CL*24
069300                                                                     CL*24
069400******************************************************************   CL*24
069500*                                                                *   CL*24
069600*    CALL MASTRMSG ABEND                                         *   CL*24
069700*      1) PSB PROGRAM NAME                                       *   CL*24
069800*      2) VPARTSUP PCB IN LINKAGE SECTION                        *   CL*24
069900*      3) DUMP OPTION DEFINED TO VALUE 'F'                       *   CL*24
070000*                                                                *   CL*24
070100******************************************************************   CL*24
070200                                                                     CL*24
070300 1001-CALL-MASTRMSG-VPARTSUP.                                        CL*24
070400     MOVE VPARTSUP-STATUS-CODE    TO DUMP-STATUS.                    CL*24
070500     MOVE 'PROGRAM TERMINATION  ' TO DUMP-MESSAGE.                   CL*24
070600     CALL 'MASTRMSG' USING PSBPGM-NAME                               CL*24
070700                           VPARTSUP-PCB                              CL*24
070800                           DUMP-OPT.                                 CL*24
070900* MODEL PROGRAM FOR IMS BATCH CLASS
