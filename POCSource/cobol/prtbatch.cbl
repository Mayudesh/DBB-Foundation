000010 ID DIVISION.                                                     11/02/89
000030 PROGRAM-ID. PRTBATCH.                                               LV025
000040                                                                     CL*24
000070 AUTHOR.         RATIONAL SOFTWARE.                                  CL*24
000080 INSTALLATION.   IBM SWG.                                            CL*24
000090 DATE-WRITTEN.   JUNE 2010.                                          CL*24
000100                                                                     CL*24
000110******************************************************************   CL*24
000120*REMARKS.                                                            CL*24
000130*    THIS PROGRAM WILL PROVIDE BATCH MAINTENANCE OF THE              CL*24
000140*    VADDRSEG SEGMENT ON THE PARTSUPP DATABASE VIA THE               CL*24
000150*    INPUT ACTIVITY FILE.                                            CL*24
000170*    INPUT.  ADDRESS DISK INPUT FILE.                                CL*24
000180*    INPUT.  PARTSUPP DATABASE - PCB05B  PSB                        CL*24
000200*    OUTPUT. PARTSUPP DATABASE VADDRSEG & VSUPPSEG UPDATED           CL*24
000220******************************************************************   CL*24
000290*    PSB MEMBER NAME           -  PCB05B                            CL*24
000320******************************************************************   CL*24
000330 ENVIRONMENT DIVISION.                                               CL*24
000340                                                                     CL*24
000350 CONFIGURATION SECTION.                                              CL*24
000360 SOURCE-COMPUTER. IBM-3081.                                          CL*24
000370 OBJECT-COMPUTER. IBM-3081.                                          CL*24
000380                                                                     CL*24
000390 INPUT-OUTPUT SECTION.                                               CL*24
000400 FILE-CONTROL.                                                       CL*24
000410     SELECT INPUT-ADDRESS-FILE ASSIGN TO UT-S-B99100.
000430 DATA DIVISION.                                                      CL*24
000440 FILE SECTION.                                                       CL*24
000450 FD  INPUT-ADDRESS-FILE                                              CL*24
000460     RECORDING MODE IS F                                             CL*24
000470     LABEL RECORDS ARE STANDARD                                      CL*24
000480     RECORD CONTAINS 100 CHARACTERS                                  CL*24
000490     BLOCK CONTAINS 0 RECORDS                                        CL*24
000500     DATA RECORD IS INPUT-RECORD.                                    CL*24
000510 01  INPUT-RECORD                   PIC X(100).                      CL*24
000520                                                                     CL*24
000530 WORKING-STORAGE SECTION.                                            CL*24
000540 01  FILLER                    PIC X(32) VALUE                       CL*24
000550        '* WORKING STORAGE BEGINS HERE *'.                           CL*24
000560                                                                     CL*24
000580 01  FILLER                    PIC X(32) VALUE                       CL*24
000590             '****** DUMP MSG ****************'.                     CL*24
004700*****************************************************************
004800*    DUMP POINTER AREA
004900*        PARA POINTER- MOVE PARAGRAPH NUMBER TO THIS POINTER    *
005000*                      AS EACH PARAGRAPH IS ENTERED. DO NOT     *
005100*                      MOVE PARAGRAPH NUMBERS OF COMMON
005200*                      PARAGRAPHS (USE COMM POINTER).
005300*                                                               *
005400*        COMM POINTER - EACH COMMON PARAGRAPH SHOULD MOVE       *
005500*                       ITS PARAGRAPH NUMBER TO THIS POINTER    *
005600*                       AT IT INCEPTION.
005700*                                                               *
005800*****************************************************************
005900 01  DUMP-LOCATOR.
006000     05 FILLER             PIC X(32)
006100               VALUE '>>>>>>> WS DUMP POINTERS >>>>>>>'.
006200     05 FILLER             PIC X(16)   VALUE 'Z PARA POINTER'.
006300     05 PARA-POINTER       PIC X(8)    VALUE SPACES.
006400     05 FILLER             PIC X(8)    VALUE '       Z'.
006500     05 FILLER             PIC X(16)   VALUE 'Z COMM POINTER'.
006600     05 COMM-POINTER       PIC X(8)    VALUE SPACES.
006700     05 FILLER             PIC X(32)
006800                   VALUE '<<<<<<< WS DUMP POINTERS <<<<<<<'.
006600     05 MODULE-POINTER     PIC X(8)    VALUE SPACES.
006900
000600 01  DUMP-DISPLAY.                                                   CL*24
000610     05 DUMP-STATUS                     PIC X(3)  VALUE SPACES.      CL*24
000620     05 DUMP-MESSAGE                    PIC X(61) VALUE 'NO MSG'.    CL*24
000630                                                                     CL*24
000640******************************************************************   CL*24
000650*                                                                *   CL*24
000660*            INPUT FILE DESCRIPTION                              *   CL*24
000670*                                                                *   CL*24
000680******************************************************************   CL*24
000690 01  INPUT-ADDRESS-RECORD.                                           CL*24
000700     05  INPUT-PART-NUMBER          PIC X(23)   VALUE SPACES.        CL*24
000710     05  INPUT-SUPPLIER-CODE        PIC X(05)   VALUE SPACES.        CL*24
000720     05  INPUT-ADDRESS-TYPE         PIC X(01)   VALUE SPACES.        CL*24
000730     05  INPUT-ADDRESS-1            PIC X(15)   VALUE SPACES.        CL*24
000740     05  INPUT-ADDRESS-2            PIC X(15)   VALUE SPACES.        CL*24
000750     05  INPUT-ADDRESS-3            PIC X(15)   VALUE SPACES.        CL*24
000760     05  INPUT-CITY                 PIC X(10)   VALUE SPACES.        CL*24
000770     05  INPUT-STATE                PIC X(02)   VALUE SPACES.        CL*24
000780     05  INPUT-ZIP-CODE             PIC 9(05)   VALUE ZEROS.         CL*24
000790     05  FILLER                     PIC X(08)   VALUE SPACES.        CL*24
000800     05  INPUT-ACTIVITY-CODE        PIC X(01)   VALUE SPACES.        CL*24
000810         88  INPUT-ADD-RECORD                   VALUE 'A'.           CL*24
000820         88  INPUT-CHANGE-RECORD                VALUE 'C'.           CL*24
000830         88  INPUT-DELETE-RECORD                VALUE 'D'.           CL*24
000840                                                                     CL*24
000850                                                                     CL*24
000860******************************************************************   CL*24
000870*                                                                *   CL*24
000880*            INPUT/OUTPUT AREAS FOR PARTSUPP DATABASE            *   CL*24
000890*                                                                *   CL*24
000900******************************************************************   CL*24
000910 01  GENERAL-IO-AREA             PIC X(100).                         CL*24
000920 01  PARTSUPP-IO-AREAS.                                              CL*24
000930     05  FILLER                  PIC X(32)  VALUE                    CL*24
000940             '** PARTSUPP DETAIL IO AREA **'.                        CL*24
015500 01  VPARTSEG-IO-AREA.
015600     05  VPARTSEG-PART-NUMBER       PIC X(23) VALUE SPACES.
015700     05  VPARTSEG-PART-NAME         PIC X(14) VALUE SPACES.
015800     05  VPARTSEG-SPEC-NUMBER       PIC X(07) VALUE SPACES.
015900     05  VPARTSEG-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
016000     05  VPARTSEG-BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
016100     05  VPARTSEG-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
016200     05  VPARTSEG-WEEKS-LEAD-TIME   PIC 9(03) VALUE 0.
016300     05  VPARTSEG-ENGINE-MODEL      PIC X(05) VALUE SPACES.
016400     05  FILLER                     PIC X(14) VALUE SPACES.
016500
016600***INCLUDE VSUPPIO
016700 01  VSUPPSEG-IO-AREA.
016800     05  VSUPPSEG-SUPPLIER-CODE     PIC X(05) VALUE SPACES.
016900     05  VSUPPSEG-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
017000     05  VSUPPSEG-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
017100     05  VSUPPSEG-SUPPLIER-PERF     PIC 9(03) VALUE 0.
017200     05  VSUPPSEG-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
017300     05  VSUPPSEG-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
017400     05  VSUPPSEG-SUPPLIER-ACT-DATE PIC 9(05) VALUE 0.
017500     05  FILLER                     PIC X(07) VALUE SPACES.
017600
017700***INCLUDE VADDRIO
017800 01  VADDRSEG-IO-AREA.
017900     05  FILLER                     PIC X(03) VALUE SPACES.
017900     05  VADDRSEG-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
018000         88  VADDRSEG-ORDER-ADDRESS           VALUE '1'.
018100         88  VADDRSEG-SCHED-ADDRESS           VALUE '2'.
018200         88  VADDRSEG-REMIT-ADDRESS           VALUE '3'.
018300     05  VADDRSEG-ADDRESS-1         PIC X(15) VALUE SPACES.
018400     05  VADDRSEG-ADDRESS-2         PIC X(15) VALUE SPACES.
018500     05  VADDRSEG-ADDRESS-3         PIC X(15) VALUE SPACES.
018600     05  VADDRSEG-CITY              PIC X(10) VALUE SPACES.
018700     05  VADDRSEG-STATE             PIC X(02) VALUE SPACES.
018800     05  VADDRSEG-ZIP-CODE          PIC 9(05) VALUE 0.
018900     05  FILLER                     PIC X(08) VALUE SPACES.
019000
019100***INCLUDE VPOIO
019200 01  VPOSEG-IO-AREA.
019300     05  VPOSEG-PO-NUMBER           PIC X(06) VALUE SPACES.
019400     05  VPOSEG-BUYER-CODE          PIC X(03) VALUE SPACES.
019500     05  VPOSEG-QUANTITY            PIC S9(7) VALUE +0.
019600     05  VPOSEG-UNIT-PRICE          PIC S9(7)V99 VALUE +0.
019700     05  VPOSEG-ORDER-DATE          PIC 9(05) VALUE 0.
019800     05  VPOSEG-DELIVERY-DATE       PIC 9(05) VALUE 0.
019900
020000******************************************************************
020100*                                                                *
020200*            SSA AREAS FOR PARTSUPP DATABASE
020300*                                                                *
020400******************************************************************
020500 01  PARTSUPP-SSAS.
020600     05  FILLER                           PIC X(32)  VALUE
020700                 '**** PARTSUPP SSA AREA ****'.
020800***INCLUDE VPARTSSA
020900 01  SSA-VPARTSEG.
021000     05  VPARTSEG-SEG-NAME          PIC X(8)  VALUE 'VPARTSEG'.
021100     05  VPARTSEG-COMM-ASTR         PIC X     VALUE '*'.
021200     05  VPARTSEG-COMM-CODE         PIC X(3)  VALUE '---'.
021300     05  VPARTSEG-L-PAREN           PIC X     VALUE '('.
021400     05  VPARTSEG-KEY-FLD           PIC X(8)  VALUE 'VPARTKEY'.
021500     05  VPARTSEG-OPERATOR-1        PIC XX    VALUE ' ='.
021600     05  VPARTSEG-KEY               PIC X(23) VALUE SPACES.
021700     05  VPARTSEG-R-PAREN           PIC X     VALUE ')'.
021800
021900***INCLUDE VSUPPSSA
022000 01  SSA-VSUPPSEG.
022100     05  VSUPPSEG-SEG-NAME          PIC X(8)  VALUE 'VSUPPSEG'.
022200     05  VSUPPSEG-COMM-ASTR         PIC X     VALUE '*'.
022300     05  VSUPPSEG-COMM-CODE         PIC X(3)  VALUE '---'.
022400     05  VSUPPSEG-L-PAREN           PIC X     VALUE '('.
022500     05  VSUPPSEG-KEY-FLD           PIC X(8)  VALUE 'VSUPPKEY'.
022600     05  VSUPPSEG-OPERATOR-1        PIC XX    VALUE ' ='.
022700     05  VSUPPSEG-KEY               PIC X(05) VALUE SPACES.
022800     05  VSUPPSEG-R-PAREN           PIC X     VALUE ')'.
022900
023000
023100***INCLUDE VADDRSSA
023200 01  SSA-VADDRSEG.
023300     05  VADDRSEG-SEG-NAME          PIC X(8)  VALUE 'VADDRSEG'.
023400     05  VADDRSEG-COMM-ASTR         PIC X     VALUE '*'.
023500     05  VADDRSEG-COMM-CODE         PIC X(3)  VALUE '---'.
023600     05  VADDRSEG-L-PAREN           PIC X     VALUE '('.
023700     05  VADDRSEG-KEY-FLD           PIC X(8)  VALUE 'VADDRKEY'.
023800     05  VADDRSEG-OPERATOR-1        PIC XX    VALUE ' ='.
023900     05  VADDRSEG-KEY               PIC X(01) VALUE SPACES.
024000     05  VADDRSEG-R-PAREN           PIC X     VALUE ')'.
024100
024200***INCLUDE VPOSSA
024300 01  SSA-VPOSEG.
024400     05  VPOSEG-SEG-NAME            PIC X(8)  VALUE 'VPOSEG'.
024500     05  VPOSEG-COMM-ASTR           PIC X     VALUE '*'.
024600     05  VPOSEG-COMM-CODE           PIC X(3)  VALUE '---'.
024700     05  VPOSEG-L-PAREN             PIC X     VALUE '('.
024800     05  VPOSEG-KEY-FLD             PIC X(8)  VALUE 'VPONOKEY'.
024900     05  VPOSEG-OPERATOR-1          PIC XX    VALUE ' ='.
025000     05  VPOSEG-KEY                 PIC X(06) VALUE SPACES.
025100     05  VPOSEG-CONNECTOR           PIC X     VALUE ')'.
025200     05  VPOSEG-KEY-FLD-NAME-2      PIC X(8)  VALUE 'VPONOKEY'.
025300     05  VPOSEG-OPERATOR-2          PIC XX    VALUE ' ='.
025400     05  VPOSEG-KEY-2               PIC X(06) VALUE SPACES.
025500     05  VPOSEG-R-PAREN             PIC X     VALUE ')'.
025600
025700******************************************************************
025800*                                                                *
025900*            STANDARD IMS VALUES USED IN CALLS
026000*                                                                *
026100******************************************************************
026200***INCLUDE IMSVALUE
026300 01  IMS-VALUES.
026400     05  EQUAL-TO                PIC XX           VALUE ' ='.
026500     05  GREATER-THAN            PIC XX           VALUE ' >'.
026600     05  LESS-THAN               PIC XX           VALUE ' <'.
026700     05  EQUAL-OR-GREATER        PIC XX           VALUE '=>'.
026800     05  EQUAL-OR-LESS           PIC XX           VALUE '=<'.
026900     05  NOT-EQUAL               PIC XX           VALUE '='.
027000     05  NOT-GREATER             PIC XX           VALUE '>'.
027100     05  NOT-LESS                PIC XX           VALUE '<'.
027200     05  BOOLEAN-AND             PIC X           VALUE '*'.
027300     05  BOOLEAN-INDP-AND        PIC X           VALUE '#'.
027400     05  BOOLEAN-OR              PIC X           VALUE '+'.
027500     05  LEFT-PAREN              PIC X           VALUE '('.
027600     05  RIGHT-PAREN             PIC X           VALUE ')'.
027700
027800 01  MFS-VALUES.
027900   02  MFS-ATTR-VALUES.
028000     05  HEX-0080 PIC S9(8) COMP VALUE +128.
028100     05  FILLER                       REDEFINES   HEX-0080.
028200         10  FILLER                   PIC XX.
028300         10  NO-ATTR                  PIC XX.
028400     05  HEX-C080 PIC S9(8) COMP VALUE +49280.
028500     05  FILLER                       REDEFINES   HEX-C080.
028600         10  FILLER                   PIC XX.
028700         10  CURSOR-1                 PIC XX.
028800     05  HEX-C088 PIC S9(8) COMP VALUE +49288.
028900     05  FILLER                       REDEFINES   HEX-C088.
029000         10  FILLER                   PIC XX.
029100         10  CURSOR-HI                PIC XX.
029200     05  HEX-0088 PIC S9(8) COMP VALUE +136.
029300     05  FILLER                       REDEFINES   HEX-0088.
029400         10  FILLER                   PIC XX.
029500         10  HI-LITE                  PIC XX.
029600     05  HEX-00B0 PIC S9(8) COMP VALUE +176.
029700     05  FILLER                       REDEFINES   HEX-00B0.
029800         10  FILLER                   PIC XX.
029900         10  NUM-PROT-NOREPL          PIC XX.
030000     05  HEX-C0C1 PIC S9(8) COMP VALUE +49345.
030100     05  FILLER                       REDEFINES   HEX-C0C1.
030200         10  FILLER                   PIC XX.
030300         10  ALPHA-UNPROT-MOD-CUR     PIC XX.
030400     05  HEX-00C1 PIC S9(8) COMP VALUE +193.
030500     05  FILLER                       REDEFINES   HEX-00C1.
030600         10  FILLER                   PIC XX.
030700         10  ALPHA-UNPROT-MOD         PIC XX.
030800     05  HEX-00C2 PIC S9(8) COMP VALUE +194.
030900     05  FILLER                       REDEFINES   HEX-00C2.
031000         10  FILLER                   PIC XX.
031100         10  ALPHA-UNPROT-NORM        PIC XX.
031200     05  HEX-C0C2 PIC S9(8) COMP VALUE +49346.
031300     05  FILLER                       REDEFINES   HEX-C0C2.
031400         10  FILLER                   PIC XX.
031500         10  ALPHA-UNPROT-NORM-CUR    PIC XX.
031600     05  HEX-C0C7 PIC S9(8) COMP VALUE +49351.
031700     05  FILLER                       REDEFINES   HEX-C0C7.
031800         10  FILLER                   PIC XX.
031900         10  ALPHA-UNPROT-NODISP-MOD-CUR PIC XX.
032000     05  HEX-00C9 PIC S9(8) COMP VALUE +201.
032100     05  FILLER                       REDEFINES   HEX-00C9.
032200         10  FILLER                   PIC XX.
032300         10  ALPHA-UNPROT-HI-MOD      PIC XX.
032400     05  HEX-C0C9 PIC S9(8) COMP VALUE +49353.
032500     05  FILLER                       REDEFINES   HEX-C0C9.
032600         10  FILLER                   PIC XX.
032700         10  ALPHA-UNPROT-HI-MOD-CUR  PIC XX.
032800     05  HEX-00D1 PIC S9(8) COMP VALUE +209.
032900     05  FILLER                       REDEFINES   HEX-00D1.
033000         10  FILLER                   PIC XX.
033100         10  NUM-UNPROT-MOD           PIC XX.
033200     05  HEX-00D2 PIC S9(8) COMP VALUE +210.
033300     05  FILLER                       REDEFINES   HEX-00D2.
033400         10  FILLER                   PIC XX.
033500         10  NUM-UNPROT-NORM          PIC XX.
033600     05  HEX-C0D2 PIC S9(8) COMP VALUE +49362.
033700     05  FILLER                       REDEFINES   HEX-C0D2.
033800         10  FILLER                   PIC XX.
033900         10  NUM-UNPROT-NORM-CUR      PIC XX.
034000     05  HEX-00D5 PIC S9(8) COMP VALUE +213.
034100     05  FILLER                       REDEFINES   HEX-00D5.
034200         10  FILLER                   PIC XX.
034300         10  NUM-UNPROT-NDIS-MOD      PIC XX.
034400     05  HEX-00D9 PIC S9(8) COMP VALUE +217.
034500     05  FILLER                       REDEFINES   HEX-00D9.
034600         10  FILLER                   PIC XX.
034700         10  NUM-UNPROT-HI-MOD        PIC XX.
034800     05  HEX-C0D9 PIC S9(8) COMP VALUE +49369.
034900     05  FILLER                       REDEFINES   HEX-C0D9.
035000         10  FILLER                   PIC XX.
035100         10  NUM-UNPROT-HI-MOD-CUR    PIC XX.
035200     05  HEX-00F1 PIC S9(8) COMP VALUE +241.
035300     05  FILLER                       REDEFINES   HEX-00F1.
035400         10  FILLER                   PIC XX.
035500         10  NUM-PROT-MOD             PIC XX.
035600     05  HEX-E7F1 PIC S9(8) COMP VALUE +59377.
035700     05  FILLER                       REDEFINES   HEX-E7F1.
035800         10  FILLER                   PIC XX.
035900         10  REPL-NUM-PROT-MOD        PIC XX.
036000     05  HEX-00F2 PIC S9(8) COMP VALUE +242.
036100     05  FILLER                       REDEFINES   HEX-00F2.
036200         10  FILLER                   PIC XX.
036300         10  NUM-PROT-NORM            PIC XX.
036400     05  HEX-00F4 PIC S9(8) COMP VALUE +244.
036500     05  FILLER                       REDEFINES   HEX-00F4.
036600         10  FILLER                   PIC XX.
036700         10  NUM-PROT-NONDSP          PIC XX.
036800     05  HEX-00F5 PIC S9(8) COMP VALUE +245.
036900     05  FILLER                       REDEFINES   HEX-00F5.
037000         10  FILLER                   PIC XX.
037100         10  NUM-PROT-NDIS-MOD        PIC XX.
037200     05  HEX-00F8 PIC S9(8) COMP VALUE +248.
037300     05  FILLER                       REDEFINES   HEX-00F8.
037400         10  FILLER                   PIC XX.
037500         10  NUM-PROT-HI              PIC XX.
037600     05  HEX-00F9 PIC S9(8) COMP VALUE +249.
037700     05  FILLER                       REDEFINES   HEX-00F9.
037800         10  FILLER                   PIC XX.
037900         10  NUM-PROT-HI-MOD          PIC XX.
038000     05  HEX-0D3F PIC S9(8) COMP VALUE +3391.
038100     05  FILLER                       REDEFINES   HEX-0D3F.
038200         10  FILLER                   PIC XX.
038300         10  NO-TRANSMIT-ERASE.
038400             15  FILLER               PIC X.
038500             15  NO-TRANSMIT          PIC X.
038600   02  MFS-EATTR-VALUES.
038700     05  HEX-C100 PIC S9(8) COMP VALUE +49408.
038800     05  FILLER                       REDEFINES   HEX-C100.
038900         10  FILLER              PIC XX.
039000         10  EATTR-HI-NORM       PIC XX.
039100     05  EATTR-HI-BLINK          PIC XX           VALUE 'A1'.
039200     05  EATTR-HI-REVERSE        PIC XX           VALUE 'A2'.
039300     05  EATTR-HI-UNDERSCORE     PIC XX           VALUE 'A4'.
039400     05  HEX-C200 PIC S9(8) COMP VALUE +49664.
039500     05  FILLER                       REDEFINES   HEX-C200.
039600         10  FILLER              PIC XX.
039700         10  EATTR-COLOR-NORM    PIC XX.
039800     05  EATTR-COLOR-BLUE        PIC XX           VALUE 'B1'.
039900     05  EATTR-COLOR-RED         PIC XX           VALUE 'B2'.
040000     05  EATTR-COLOR-PINK        PIC XX           VALUE 'B3'.
040100     05  EATTR-COLOR-GREEN       PIC XX           VALUE 'B4'.
040200     05  EATTR-COLOR-TURQ        PIC XX           VALUE 'B5'.
040300     05  EATTR-COLOR-YELLOW      PIC XX           VALUE 'B6'.
040400     05  EATTR-COLOR-NEUTRAL     PIC XX           VALUE 'B7'.
040500
040600******************************************************************
040700*                                                                *
040800*            STANDARD CALL FUNCTIONS USED IN CALLS               *
040900*                                                                *
041000******************************************************************
041100*******IMSFUNCT REALLY IS DLIFUNCT, HERE IT IS:
041200
041300 01  DLI-CALL-FUNCTIONS.
041400     02  DB-CALL-FUNCTIONS.
041500         05  GU-FUNC           PIC X(4)    VALUE 'GU  '.
041600         05  GN-FUNC           PIC X(4)    VALUE 'GN  '.
041700         05  GHU-FUNC          PIC X(4)    VALUE 'GHU '.
041800         05  GHN-FUNC          PIC X(4)    VALUE 'GHN '.
041900         05  GNP-FUNC          PIC X(4)    VALUE 'GNP '.
042000         05  GHNP-FUNC         PIC X(4)    VALUE 'GHNP'.
042100         05  ISRT-FUNC         PIC X(4)    VALUE 'ISRT'.
042200         05  REPL-FUNC         PIC X(4)    VALUE 'REPL'.
042300         05  DLET-FUNC         PIC X(4)    VALUE 'DLET'.
042400     02  DC-CALL-FUNCTIONS.
042500         05  CMD-FUNC          PIC X(4)    VALUE 'CMD '.
042600         05  GCMD-FUNC         PIC X(4)    VALUE 'GCMD'.
042700         05  PURG-FUNC         PIC X(4)    VALUE 'PURG'.
042800         05  CHNG-FUNC         PIC X(4)    VALUE 'CHNG'.
042900
001320 01  IMS-WORK-AREA.                                                  CL*24
001330     05  PARM-CT                 PIC  S9(7) COMP SYNC VALUE +3.      CL*24
001340     05  PSBPGM-NAME             PIC  X(8) VALUE 'B99100 '.          CL*24
001350     05  DUMP-OPT                PIC  X    VALUE 'F'.                CL*24
001360     05  TIME-TO-END             PIC  X    VALUE 'N'.                CL*25
001370     05  CALL-FUNCTION           PIC  X(4) VALUE SPACES.             CL*24
001380                                                                     CL*24
001390 01  DATE-PARMS.                                                     CL*24
001400     05 DATE-JULIAN           PIC X(5).                              CL*24
001410     05 DATE-GREGORIAN.                                              CL*24
001420        10 DATE-GREG-MM       PIC X(2).                              CL*24
001430        10 DATE-GREG-DD       PIC X(2).                              CL*24
001440        10 DATE-GREG-YY       PIC X(2).                              CL*24
001450                                                                     CL*24
001460******************************************************************   CL*24
001470*                                                                *   CL*24
001480*            WORKING STORAGE HOLD AREAS                          *   CL*24
001490*                                                                *   CL*24
001500******************************************************************   CL*24
001510 01  WS1-PGM-SWITCHES.                                               CL*24
001520     05 WS1-EOF-INDICATOR        PIC X      VALUE ' '.               CL*24
001530        88 WS1-END-OF-FILE                  VALUE 'Y'.               CL*24
001540                                                                     CL*24
001550 01  WS2-HOLD-QTYS-AREA.                                             CL*24
001560     05 WS2-INPUT-COUNT        PIC S9(5) VALUE +0 COMP-3.            CL*24
001570     05 WS2-ADD-RECORD-COUNT     PIC S9(5) VALUE +0 COMP-3.          CL*24
001580     05 WS2-CHG-RECORD-COUNT     PIC S9(5) VALUE +0 COMP-3.          CL*24
001590     05 WS2-DEL-RECORD-COUNT     PIC S9(5) VALUE +0 COMP-3.          CL*24
001600     05 WS2-ERR-RECORD-COUNT     PIC S9(5) VALUE +0 COMP-3.          CL*24
001610                                                                     CL*24
001620 01  WS3-PGM-MESSAGES.                                               CL*24
001630     05  WS3-ERROR-FLAG               PIC X(01)   VALUE ' '.         CL*24
001640         88  WS3-ERROR-FOUND                      VALUE 'Y'.         CL*24
001650     05  WS3-ERROR-MESSAGE-1          PIC X(79)   VALUE              CL*24
001660        'INVALID ACTIVITY CODE ON INPUT'.                            CL*24
001670                                                                     CL*24
001680 01  WS4-EDIT-FIELDS.                                                CL*24
001690     05  WS4-EDIT-INPUT-COUNT         PIC ZZZZ9 VALUE ZEROS.         CL*24
001700     05  WS4-EDIT-ADD-COUNT           PIC ZZZZ9 VALUE ZEROS.         CL*24
001710     05  WS4-EDIT-CHG-COUNT           PIC ZZZZ9 VALUE ZEROS.         CL*24
001720     05  WS4-EDIT-DEL-COUNT           PIC ZZZZ9 VALUE ZEROS.         CL*24
001730     05  WS4-EDIT-ERR-COUNT           PIC ZZZZ9 VALUE ZEROS.         CL*24
001740                                                                     CL*24
001750 LINKAGE SECTION.                                                    CL*24
001760**************************************************************       CL*24
001770*                                                            *       CL*24
001780*       L I N K A G E   S E C T I O N                        *       CL*24
001790*                                                            *       CL*24
001800**************************************************************       CL*24
001810                                                                     CL*24
001820******************************************************************   CL*24
001830*                                                                *   CL*24
001840*       PARTSUPP DATABASE PCB                                    *   CL*24
001850*                                                                *   CL*24
001860******************************************************************   CL*24
021130 01  PARTFILE-PCB.
021140     02 PN-DBD-NAME          PICTURE X(8).
021150     02 PN-SEG-LEVEL         PICTURE XX.
021160     02 PN-STATUS-CODE       PICTURE XX.
021170     02 PN-PROC-OPTIONS      PICTURE XXXX.
021180     02 RESERVE-DLI          PICTURE S9(5) COMPUTATIONAL.
021190     02 PN-SEG-NAME-FB       PICTURE X(8).
021200     02 PN-SEG-FB-LENGTH     PICTURE S9(5) COMPUTATIONAL.
022010     02 PN-NUMB-SENS-SEGS    PICTURE S9(5) COMPUTATIONAL.
022020     02 PN-KEY-FB-AREA.
022030         03 PARTROOT-KEY     PICTURE X(17).
022040         03 STOKSTAT-KEY     PICTURE X(16).
022050         03 BACKORDR-KEY     PICTURE X(10).
022060     02 PARTROOT-NAME        PICTURE X(8).
022080     02 STOKSTAT-NAME        PICTURE X(8).
022090     02 CYCCOUNT-NAME        PICTURE X(8).
022100     02 BACKORDR-NAME        PICTURE X(8).
001880                                                                     CL*24
001890******************************************************************   CL*24
001900*                                                                *   CL*24
001910*       P R O C E D U R E    D I V I S I O N                     *   CL*24
001920*                                                                *   CL*24
001930******************************************************************   CL*24
001940 PROCEDURE DIVISION.                                                 CL*24
001950                                                                     CL*24
052600     ENTRY 'DLITCBL' USING   PARTFILE-PCB.
001970                                                                     CL*24
001980     DISPLAY '*** BEGIN PROGRAM B99100 ***'                         CL*24
001990     MOVE 'B99100'   TO MODULE-POINTER.                             CL*24
002000     DISPLAY SPACES.                                                 CL*24
002010                                                                     CL*24
002020     OPEN INPUT INPUT-ADDRESS-FILE.                                  CL*24
002030                                                                     CL*24
002040     MOVE 'CURRDATE' TO MODULE-POINTER.                              CL*24
002050*    CALL 'CURRDATE' USING DATE-PARMS.                               CL*24
002060     MOVE 'B99100'   TO MODULE-POINTER.                             CL*24
002070                                                                     CL*24
002080**************************************************************       CL*24
002090     PERFORM 000-READ-INPUT-FILE THRU 000-EXIT.                      CL*24
002100                                                                     CL*24
002110     PERFORM 005-MAINLINE THRU 005-EXIT UNTIL                        CL*24
002120         WS1-END-OF-FILE.                                            CL*24
002140**************************************************************       CL*24
002150                                                                     CL*24
002160     CLOSE INPUT-ADDRESS-FILE.                                       CL*24
002170                                                                     CL*24
002180     MOVE WS2-INPUT-COUNT      TO WS4-EDIT-INPUT-COUNT.              CL*24
002190     MOVE WS2-ADD-RECORD-COUNT TO WS4-EDIT-ADD-COUNT.                CL*24
002200     MOVE WS2-CHG-RECORD-COUNT TO WS4-EDIT-CHG-COUNT.                CL*24
002210     MOVE WS2-DEL-RECORD-COUNT TO WS4-EDIT-DEL-COUNT.                CL*24
002220     MOVE WS2-ERR-RECORD-COUNT TO WS4-EDIT-ERR-COUNT.                CL*24
002230                                                                     CL*24
002240     DISPLAY 'INPUT RECORDS READ          ' WS4-EDIT-INPUT-COUNT.    CL*24
002250     DISPLAY 'INPUT RECORDS ADDED TO DB   ' WS4-EDIT-ADD-COUNT.      CL*24
002260     DISPLAY 'INPUT RECORDS CHANGED ON DB ' WS4-EDIT-CHG-COUNT.      CL*24
002270     DISPLAY 'INPUT RECORDS DELETED ON DB ' WS4-EDIT-DEL-COUNT.      CL*24
002280     DISPLAY 'INPUT RECORDS IN ERROR      ' WS4-EDIT-ERR-COUNT.      CL*24
002290     DISPLAY SPACES.                                                 CL*24
002300     DISPLAY '*** END PROGRAM B99100 ***'.                           CL*24
002310                                                                     CL*24
002320     MOVE ZERO TO RETURN-CODE.
CL*24      CALL 'PRTUPDB'.
002330     GOBACK.                                                         CL*24
002340                                                                     CL*24
002350 000-READ-INPUT-FILE.                                                CL*24
002360     MOVE 000 TO PARA-POINTER.                                       CL*24
002370                                                                     CL*24
002380     READ INPUT-ADDRESS-FILE INTO INPUT-ADDRESS-RECORD               CL*24
002390         AT END                                                      CL*24
002400         MOVE 'Y' TO WS1-EOF-INDICATOR                               CL*24
002410         GO TO 000-EXIT.                                             CL*24
002420     ADD +1 TO WS2-INPUT-COUNT.                                      CL*24
002430 000-EXIT.                                                           CL*24
002440     EXIT.                                                           CL*24
002490                                                                     CL*24
002500 005-MAINLINE.                                                       CL*24
002510     MOVE 005    TO PARA-POINTER.                                    CL*24
002520                                                                     CL*24
002530     PERFORM 010-EDIT-CHECKS THRU 010-EXIT.                          CL*24
002540                                                                     CL*24
002550     PERFORM 020-COMPARE-TO-DB-ROUTINE THRU 020-EXIT.                CL*24
002560                                                                     CL*24
002570     PERFORM 000-READ-INPUT-FILE THRU 000-EXIT.                      CL*24
002580 005-EXIT.                                                           CL*24
002590     EXIT.                                                           CL*24

002630 010-EDIT-CHECKS.                                                    CL*24
002640     MOVE 010  TO PARA-POINTER.                                      CL*24
002650                                                                     CL*24
002660     MOVE ' '  TO WS3-ERROR-FLAG.                                    CL*24
002670                                                                     CL*24
002680     IF INPUT-ACTIVITY-CODE = 'A' OR 'C' OR 'D'                      CL*24
002690       THEN                                                          CL*24
002700         NEXT SENTENCE                                               CL*24
002710     ELSE                                                            CL*24
002720         DISPLAY WS3-ERROR-MESSAGE-1                                 CL*24
002730         DISPLAY INPUT-ADDRESS-RECORD                                CL*24
002740         ADD +1 TO WS2-ERR-RECORD-COUNT                              CL*24
002750         MOVE 'Y' TO WS3-ERROR-FLAG                                  CL*24
002760         GO TO 010-EXIT.                                             CL*24
002770 010-EXIT. EXIT.                                                     CL*24
002780                                                                     CL*24
002790                                                                     CL*24
002810 020-COMPARE-TO-DB-ROUTINE.                                          CL*24
002820     MOVE 020 TO PARA-POINTER.                                       CL*24
002830                                                                     CL*24
002840     IF WS3-ERROR-FOUND                                              CL*24
002850         GO TO 020-EXIT.                                             CL*24
002860                                                                     CL*24
002870     IF INPUT-ADD-RECORD                                             CL*24
002880         PERFORM 100-ADD-VADDRSEG-ROUTINE THRU 100-EXIT              CL*24
002890         GO TO 020-EXIT.                                             CL*24
002900                                                                     CL*24
002910     IF INPUT-CHANGE-RECORD                                          CL*24
002920         PERFORM 150-CHANGE-VADDRSEG-ROUTINE THRU 150-EXIT           CL*24
002930         GO TO 020-EXIT.                                             CL*24
002940                                                                     CL*24
002950     IF INPUT-DELETE-RECORD                                          CL*24
002960         PERFORM 200-DELETE-VADDRSEG-ROUTINE THRU 200-EXIT           CL*24
002970         GO TO 020-EXIT.                                             CL*24
002980 020-EXIT.                                                           CL*24
002990     EXIT.                                                           CL*24
003000                                                                     CL*24
003010 100-ADD-VADDRSEG-ROUTINE.                                           CL*24
003020     MOVE 100 TO PARA-POINTER.                                       CL*24

000730     MOVE INPUT-ADDRESS-1  TO VADDRSEG-ADDRESS-1.
000740     MOVE INPUT-ADDRESS-2  TO VADDRSEG-ADDRESS-2.
000750     MOVE INPUT-ADDRESS-3  TO VADDRSEG-ADDRESS-3.
000760     MOVE INPUT-CITY       TO VADDRSEG-CITY.
000770     MOVE INPUT-STATE      TO VADDRSEG-STATE.
000780     MOVE INPUT-ZIP-CODE   TO VADDRSEG-ZIP-CODE.
003430     IF INPUT-ADDRESS-TYPE = 'O'                                     CL*24
003440         MOVE '1'              TO VADDRSEG-ADDRESS-TYPE.             CL*24
003450     IF INPUT-ADDRESS-TYPE = 'S'                                     CL*24
003460         MOVE '2'              TO VADDRSEG-ADDRESS-TYPE.             CL*24
003470     IF INPUT-ADDRESS-TYPE = 'R'                                     CL*24
003480         MOVE '3'              TO VADDRSEG-ADDRESS-TYPE.             CL*24

003540     MOVE VADDRSEG-IO-AREA TO GENERAL-IO-AREA                        CL*24
003410     MOVE INPUT-PART-NUMBER    TO VPARTSEG-KEY.                      CL*24
003420     MOVE INPUT-SUPPLIER-CODE  TO VSUPPSEG-KEY.                      CL*24
003040     MOVE ' '                       TO VADDRSEG-L-PAREN.             CL*24
003050     MOVE ISRT-FUNC                 TO CALL-FUNCTION.                CL*24
003510     MOVE +6                        TO PARM-CT.                      CL*24
003520     PERFORM 1000-CALL-PARTSUPP-DB THRU 1000-EXIT.                   CL*24
003080                                                                     CL*24
003090                                                                     CL*24
003100 100-EXIT.                                                           CL*24
003110     EXIT.                                                           CL*24
003120                                                                     CL*24
003130 150-CHANGE-VADDRSEG-ROUTINE.                                        CL*24
003140     MOVE 150 TO PARA-POINTER.                                       CL*24
003150                                                                     CL*24
003390     PERFORM 300-GHU-VADDRSEG-ROUTINE THRU 300-EXIT.
003150                                                                     CL*24

000730     MOVE INPUT-ADDRESS-1  TO VADDRSEG-ADDRESS-1.
000740     MOVE INPUT-ADDRESS-2  TO VADDRSEG-ADDRESS-2.
000750     MOVE INPUT-ADDRESS-3  TO VADDRSEG-ADDRESS-3.
000760     MOVE INPUT-CITY       TO VADDRSEG-CITY.
000770     MOVE INPUT-STATE      TO VADDRSEG-STATE.
000780     MOVE INPUT-ZIP-CODE   TO VADDRSEG-ZIP-CODE.

017800     MOVE VADDRSEG-IO-AREA TO GENERAL-IO-AREA.
003500     MOVE REPL-FUNC        TO CALL-FUNCTION.
003510     MOVE +3               TO PARM-CT.
003520     PERFORM 1000-CALL-PARTSUPP-DB THRU 1000-EXIT.                   CL*24
003230 150-EXIT.                                                           CL*24
003240     EXIT.                                                           CL*24
003250                                                                     CL*24
003260 200-DELETE-VADDRSEG-ROUTINE.                                        CL*24
003270     MOVE 200 TO PARA-POINTER.                                       CL*24
003280                                                                     CL*24
003390     PERFORM 300-GHU-VADDRSEG-ROUTINE THRU 300-EXIT.

003500     MOVE DLET-FUNC             TO CALL-FUNCTION.                     CL*2
003510     MOVE +3                   TO PARM-CT.                           CL*24
003520     PERFORM 1000-CALL-PARTSUPP-DB THRU 1000-EXIT.                   CL*24
003360 200-EXIT.                                                           CL*24
003370     EXIT.                                                           CL*24
003380                                                                     CL*24
003390 300-GHU-VADDRSEG-ROUTINE.                                           CL*24
003400     MOVE 300 TO COMM-POINTER.                                       CL*24

003410     MOVE INPUT-PART-NUMBER    TO VPARTSEG-KEY.                      CL*24
003420     MOVE INPUT-SUPPLIER-CODE  TO VSUPPSEG-KEY.                      CL*24
003430     IF INPUT-ADDRESS-TYPE = 'O'                                     CL*24
003440         MOVE '1'              TO VADDRSEG-KEY.                      CL*24
003450     IF INPUT-ADDRESS-TYPE = 'S'                                     CL*24
003460         MOVE '2'              TO VADDRSEG-KEY.                      CL*24
003470     IF INPUT-ADDRESS-TYPE = 'R'                                     CL*24
QUIT80         MOVE '3'              TO VADDRSEG-KEY.                      CL*24
003490                                                                     CL*24
003500     MOVE GHU-FUNC             TO CALL-FUNCTION.                     CL*24
003510     MOVE +6                   TO PARM-CT.                           CL*24
003520     PERFORM 1000-CALL-PARTSUPP-DB THRU 1000-EXIT.                   CL*24

003530     IF PN-STATUS-CODE = '  '                                        CL*24
003540         MOVE GENERAL-IO-AREA  TO VADDRSEG-IO-AREA                   CL*24
003550         GO TO 300-EXIT.                                             CL*24
003560     IF PN-STATUS-CODE = 'GE'                                        CL*24
003570         MOVE 'Y'              TO WS3-ERROR-FLAG                     CL*24
003580         GO TO 300-EXIT.                                             CL*24
003590 300-EXIT.                                                           CL*24
003600     EXIT.                                                           CL*24
003610                                                                     CL*24
003760 1000-CALL-PARTSUPP-DB.                                              CL*24
003770     MOVE 'IMS CALL' TO MODULE-POINTER.                              CL*24
003780                                                                     CL*24
003790******************************************************************   CL*24
003800*                                                                *   CL*24
003810*    PARTSUPP DATABASE CALL USING                                *   CL*24
003820*      1) PARM-CT PARAMETER                                      *   CL*24
003830*      2) GENERAL CALL-FUNCTION                                  *   CL*24
003840*      3) GENERAL I/O AREA                                       *   CL*24
003850*      4) SPECIFIC SSAS FOR EACH SEGMENT RETRIEVED               *   CL*24
003860*                                                                *   CL*24
003870******************************************************************   CL*24
003890     CALL 'CBLTDLI' USING  PARM-CT                                   CL*24
003900                           CALL-FUNCTION                             CL*24
003910                           PARTFILE-PCB                              CL*24
003920                           GENERAL-IO-AREA                           CL*24
003930                           SSA-VPARTSEG                              CL*24
003940                           SSA-VSUPPSEG                              CL*24
003950                           SSA-VADDRSEG.                             CL*24
003960                                                                     CL*24
003970     MOVE 'B99100'  TO  MODULE-POINTER.                             CL*24
003980     IF PN-STATUS-CODE = ' '                                         CL*24
003990         NEXT SENTENCE                                               CL*24
004000     ELSE                                                            CL*24
004010         GO TO 1001-CALL-DMP-PGM-PARTSUPP.                          CL*24
004020                                                                     CL*24
004030 1000-EXIT.                                                          CL*24
004040     EXIT.                                                           CL*24
004050                                                                     CL*24
004060******************************************************************   CL*24
004070*                                                                *   CL*24
004080*    CALL DMP-PGM ABEND                                         *   CL*24
004090*      1) PSB PROGRAM NAME                                       *   CL*24
004100*      2) PARTSUPP PCB IN LINKAGE SECTION                        *   CL*24
004110*      3) DUMP OPTION DEFINED TO VALUE 'F'                       *   CL*24
004120*                                                                *   CL*24
004130******************************************************************   CL*24
004140                                                                     CL*24
004150 1001-CALL-DMP-PGM-PARTSUPP.                                        CL*24
004160     MOVE PN-STATUS-CODE     TO DUMP-STATUS.                         CL*24
004170     MOVE 'PROGRAM TERMINATION  ' TO DUMP-MESSAGE.                   CL*24
004180     CALL 'DLIABEND' USING PSBPGM-NAME                               CL*24
004190                           PARTFILE-PCB                              CL*24
004200                           DUMP-OPT.                                 CL*24
END   * MODEL PROGRAM FOR IMS BATCH CLASS
