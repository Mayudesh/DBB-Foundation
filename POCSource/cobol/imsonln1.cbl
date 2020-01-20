000100 ID DIVISION.
000300 PROGRAM-ID. IMSONLN1.
000500*REMARKS. PART / SUPPLIER ADDRESS MAINTENANCE MENU ON-LINE
      *         PROGRAM.
000700 AUTHOR.         IMS CLASS.
000800 INSTALLATION.   THE SYSTEMS GROUP.
000900 DATE-WRITTEN.   JULY 1989.
001100******************************************************************
001200*REMARKS.
001500*          THIS PROGRAM WILL PROVIDE ONLINE ACCESS TO THE TRAINING
001600*          DATA BASE.  IT WILL BRING UP THE MAIN SELECTION MENU
001700*          AND ALLOW THE USER TO ENTER THE PART NUMBER AND
001800*          SELECT 1 OF 3 OPTIONS FOR INQUIRY.  THE PROGRAM WILL
001900*          PASS CONTROL TO THE SELECTED PROGRAM BY USING
002000*          PROGRAM MESSAGE SWITCHING.
      ******************************************************************
002500           TRAN CODE                 -        VB99005
002700           PROGRAM MEMBER NAME       -        VB99005
002900           PSB NAME                  -        VB99005
003100           MFS FORMAT NAME           -        VB9905
003300******************************************************************
003400 ENVIRONMENT DIVISION.
003500
003600 CONFIGURATION SECTION.
003700 SOURCE-COMPUTER. IBM-3081.
003800 OBJECT-COMPUTER. IBM-3081.
003900
004000 DATA DIVISION.
004100 WORKING-STORAGE SECTION.
004200 01  FILLER                     PIC X(32) VALUE
004300              '* WORKING STORAGE BEGINS HERE *'.
004500 01  FILLER                     PIC X(32) VALUE
004600                   '****** DUMP MSG ****************'.
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
007000 01  DUMP-DISPLAY.
007100     05 DUMP-STATUS               PIC X(3)  VALUE SPACES.
007200     05 DUMP-MESSAGE              PIC X(61) VALUE 'NO MSG'.
007400******************************************************************
007500*                                                                *
007600*     INPUT/OUTPUT AREAS FOR DC CALLS TO AND FROM MSG QUE        *
007700*                                                                *
007800******************************************************************
007900
008000*----------------------------------------------------------------*
008100* THIS SHOULD MATCH UP WITH YOUR MID IN YOUR MFS CONTROL BLOCKS  *
008200*                                                                *
008300* REMEMBER THE LL ZZ FIELDS ARE NOT DEFINED BY THE MID
008400*----------------------------------------------------------------*
008500
008600 01  TP-INPUT-AREA.
008700     05  TP-IN-LL                PIC S9(04) COMP.
008800     05  TP-IN-ZZ                PIC S9(04) COMP.
008900     05  TP-IN-TRANCODE          PIC X(8).
009000     05  FILLER                  PIC X(1).
009100     05  TP-IN-MSG.
009200         10  TP-IN-PFKEY         PIC X(5).
009300             88 VALID-PFKEY          VALUES ARE 'PF1  ' 'PF2  '
009400                                                'PF3  '.
009500             88 PFKEY1               VALUE 'PF1  '.
009600             88 PFKEY2               VALUE 'PF2  '.
009700             88 PFKEY3               VALUE 'PF3  '.
009800             88 PFKEY1-PART      VALUE 'PF1  '.
009900             88 PFKEY2-SUP       VALUE 'PF2  '.
010000             88 PFKEY3-PO        VALUE 'PF3  '.
010100     05  TP-IN-PART-NUMBER       PIC X(23).
010200
010300
010400*----------------------------------------------------------------*
010500* THIS SHOULD MATCH UP WITH YOUR MOD IN YOUR MFS CONTROL BLOCKS  *
010600*                                                                *
010700* REMEMBER THE LL ZZ FIELDS ARE YOUR RESPONSIBILITY.  YOU MUST   *
010800*                                                                *
010900* INITILIZE THEM TO VALID VALUES BEFORE YOU INSERT THE MSG.      *
011000*----------------------------------------------------------------*
011100
011200 01  TP-OUTPUT-AREA.
011300     05  TP-OUT-LL             PIC S9(04) COMP VALUE +106.
011400     05  TP-OUT-ZZ             PIC S9(04) COMP VALUE +0.
011500     05  TP-OUT-PRT-NUM                  PIC X(23).
011500     05  TP-OUT-MSG.
011600         10  TP-OUT-ERROR-MSG            PIC X(79).
011700
011800*----------------------------------------------------------------*
011900* THIS SHOULD MATCH UP WITH YOUR TP INPUT AREA OF THE NEXT       *
012000*                                                                *
012100* PROGRAM THAT IS TO BE EXECUTED. WHEN THE NEXT PROGRAM GETS     *
012200*                                                                *
012300* CONTROL IT WILL ISSUE A GU TO GET THIS MSG OUT OF THE MSG QUE. *
012400*----------------------------------------------------------------*
012500
012600
012700 01  MSG-SWITCH-OUTPUT-AREA.
012800     05  MS-OUT-LL               PIC S9(04) COMP VALUE +46.
012900     05  MS-OUT-ZZ               PIC S9(04) COMP VALUE +0.
013000     05  MS-TRANSCODE-NAME       PIC X(8).
013100     05  FILLER                  PIC X      VALUE SPACES.
013200     05  FILLER                  PIC X(5)   VALUE 'SW   '.
013300     05  MS-OUT-PART-NUMBER      PIC X(23).
013400     05  FILLER                  PIC X(5)   VALUE SPACES.
013500
013600******************************************************************
013700*                                                                *
013800*  THIS IS THE NAME OF THE MOD IN YOUR MFS THAT WILL BE USED     *
013900*                                                                *
014000*  TO UNSTRING YOUR MSG AND FORMAT THE SCREEN.
014100*                                                                *
014200******************************************************************
014300
014400 01  MOD-NAME                 PIC X(8)   VALUE 'VB005A51'.
014500
014600******************************************************************
014700*                                                                *
014800*            INPUT/OUTPUT AREAS FOR VPARTSUP DATABASE
014900*                                                                *
015000******************************************************************
015100
015200 01  VPARTSUP-IO-AREAS.
015300     05  FILLER                       PIC X(32)  VALUE
015400             '** VPARTSUP DETAIL IO AREA **'.
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
017700***INCLUDE VADDRIO
017800 01  VADDRSEG-IO-AREA.
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
019100***INCLUDE VPOIO
019200 01  VPOSEG-IO-AREA.
019300     05  VPOSEG-PO-NUMBER           PIC X(06) VALUE SPACES.
019400     05  VPOSEG-BUYER-CODE          PIC X(03) VALUE SPACES.
019500     05  VPOSEG-QUANTITY            PIC S9(7) VALUE +0.
019600     05  VPOSEG-UNIT-PRICE          PIC S9(7)V99 VALUE +0.
019700     05  VPOSEG-ORDER-DATE          PIC 9(05) VALUE 0.
019800     05  VPOSEG-DELIVERY-DATE       PIC 9(05) VALUE 0.
020000******************************************************************
020100*                                                                *
020200*            SSA AREAS FOR VPARTSUP DATABASE
020300*                                                                *
020400******************************************************************
020500 01  VPARTSUP-SSAS.
020600     05  FILLER                           PIC X(32)  VALUE
020700                 '**** VPARTSUP SSA AREA ****'.
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
043000 01  IMS-WORK-AREA.
043100     05  PSBPGM-NAME             PIC  X(8) VALUE 'VB99005'.
043200     05  DUMP-OPT                PIC  X    VALUE 'F'.
043300     05  CALL-FUNCTION           PIC  X(4) VALUE SPACES.
043500 01  DATE-PARMS.
043600     05 DATE-JULIAN              PIC X(5).
043700     05 DATE-GREGORIAN.
043800        10 DATE-GREG-MM          PIC X(2).
043900        10 DATE-GREG-DD          PIC X(2).
044000        10 DATE-GREG-YY          PIC X(2).
044200******************************************************************
044300*                                                                *
044400*            WORKING STORAGE HOLD AREAS
044500*                                                                *
044600******************************************************************
044700 01  WS100-EDIT-ERROR-SW     PIC X          VALUE 'N'.
044800     88 EDIT-ERROR                          VALUE 'Y'.
044900 01  WS-200-ERROR-MSGS.
045000     05  WS-200-PFKEY-OUT-OF-SERV-MSG          PIC X(36) VALUE
045100         'PFKEY1 AND PFKEY2 ARE OUT OF SERVICE'.
045200     05  WS-200-PFKEY-ERROR-MSG                PIC X(38) VALUE
045300         'PFKEYS 1 2 OR 3 ARE ONLY VALID OPTIONS'.
045400     05  WS-200-NOT-FOUND-MSG                  PIC X(36) VALUE
045500         'PART NUMBER NOT FOUND ON DATA BASE  '.
045600     05  WS-200-PGM-ERROR-MSG                  PIC X(36) VALUE
045700         'ERROR PLEASE CALL PROGRAMMER        '.
045800
045900 01  OUTPUT-MSG-ERROR               PIC X(79)  VALUE SPACES.
046000
046100 01  WS-TRANSACTION                 PIC X(8)   VALUE 'TRAIN05 '.
046200
046400 LINKAGE SECTION.
046500**************************************************************
046600*                                                            *
046700*       L I N K A G E   S E C T I O N                           *
046800*                                                            *
046900**************************************************************
047100******************************************************************
047200*  THIS IO-TERMINAL-PCB IS USED IN THIS PROGRAM FOR MESSAGE      *
047300*  CALLS TO THE QUEUE.
047400******************************************************************
047500 01  IO-TERMINAL-PCB                   SYNC.
047600     05  IO-TERMINAL-NAME              PIC X(8).
047700     05  IO-RESERVED                   PIC XX.
047800     05  IO-STATUS-CODE                PIC XX.
047900         88  IO-CALL-SUCCESSFUL      VALUE '  '.
048000         88  IO-NOMORE-MSG-SEGMENTS  VALUE 'QD'.
048100         88  IO-NOMORE-MESSAGES      VALUE 'QC'.
048200     05  IO-PREFIX.
048300         10  IO-JULIAN-DATE           PIC S9(7) COMP-3.
048400         10  IO-TIME-OF-DAY           PIC S9(7) COMP-3.
048500         10  IO-MESSAGE-SEQ           PIC S9(3) COMP.
048600         10  FILLER                   PIC XX.
048800******************************************************************
048900*  THIS ALT-IO-PCB IS USED IN THIS PROGRAM FOR MESSAGE
049000*  SWITCHING.
049100******************************************************************
049200 01  ALT-IO-PCB                   SYNC.
049300     05  ALT-IO-DESTINATION               PIC X(8).
049400     05  ALT-IO-RESERVED                  PIC XX.
049500     05  ALT-IO-STATUS-CODE               PIC XX.
049600         88  ALT-IO-CALL-SUCCESSFUL      VALUE '  '.
049800******************************************************************
049900*                                                                *
050000*       VPARTSUP DATABASE PCB
050100*                                                                *
050200******************************************************************
050300 01  VPARTSUP-PCB.
050400     05  VPARTSUP-DBD-NAME           PIC X(08).
050500     05  VPARTSUP-SEGMENT-LEVEL      PIC X(02).
050600     05  VPARTSUP-STATUS-CODE        PIC X(02).
050700         88 VPARTSUP-SUCCESSFUL-CALL      VALUE '  ' 'GA' 'GK'.
050800         88 VPARTSUP-SEGMENT-NOT-FOUND    VALUE 'GE'.
050900         88 VPARTSUP-END-OF-DB            VALUE 'GB'.
051000         88 VPARTSUP-DUPLICATE-KEY        VALUE 'II'.
051100     05  VPARTSUP-PROC-OPT           PIC X(04).
051200     05  VPARTSUP-RESERVE-DLI           PIC S9(5) COMP.
051300     05  VPARTSUP-SEGMENT-NAME           PIC X(08).
051400     05  VPARTSUP-KEY-LENGTH           PIC S9(5) COMP.
051500     05  VPARTSUP-NO-SEN-SEGS           PIC S9(5) COMP.
051600     05  VPARTSUP-KEY-FEEDBACK           PIC X(34).
051700     05  VPARTSUP-LEVEL-1-NAME           PIC X(08).
051900******************************************************************
052000*                                                                *
052100*       P R O C E D U R E    D I V I S I O N
052200*                                                                *
052300******************************************************************
052400 PROCEDURE DIVISION.
052500
052600     ENTRY 'DLITCBL' USING   IO-TERMINAL-PCB
052700                             ALT-IO-PCB
052800                             VPARTSUP-PCB.
052900
053000
053100     PERFORM 000-GET-MESSAGE  THRU 000-EXIT.
053200
053300     PERFORM 005-PROCESS-MSG  THRU 005-EXIT
053400        UNTIL IO-NOMORE-MESSAGES.
053500
053600     MOVE ZERO TO RETURN-CODE.
053700     GOBACK.
053800
053900
054000******************************************************************
054100*                                                                *
054200*    IO-PCB  DC CALL USING                                       *
054300*      2) GU CALL-FUNCTION                                       *
054400*      3) TP INPUT AREA
054500*                                                                *
054600******************************************************************
054700 000-GET-MESSAGE.
054800     MOVE 000          TO PARA-POINTER.
054900
055000     CALL 'CBLTDLI' USING  GU-FUNC
055100                           IO-TERMINAL-PCB
055200                           TP-INPUT-AREA.
055300
055400     IF IO-CALL-SUCCESSFUL
055500         NEXT SENTENCE
055600     ELSE
055700         IF IO-NOMORE-MESSAGES
055800             NEXT SENTENCE
055900         ELSE
056000             GO TO 1001-CALL-IBMABND-IO-TERM.
056100
056200 000-EXIT.
056300     EXIT.
056500
056600******************************************************************
056700*                                                                *
056800*    IO-PCB  DC CALL USING                                       *
056900*      1) INSERT        CALL-FUNCTION
057000
057100*      2) TP-OUTPUT-AREA
057200*      3) MOD-NAME                                               *
057300******************************************************************
057400 002-SEND-MESSAGE.
057500     MOVE 002           TO PARA-POINTER.
057600
057700     CALL 'CBLTDLI' USING  ISRT-FUNC
057800                           IO-TERMINAL-PCB
057900                           TP-OUTPUT-AREA
058000                           MOD-NAME.
058100
058200
058300     IF IO-CALL-SUCCESSFUL
058400         NEXT SENTENCE
058500     ELSE
058600         GO TO 1001-CALL-IBMABND-IO-TERM.
058700
058800 002-EXIT.
058900     EXIT.
059100******************************************************************
059200*                                                                *
059300*    005-PROCESS-MSG
059400*      1) VERIFY PART NUMBER EXIST ON DATA BASE
059500*      2) IF PART NUMBER IS OK IT WILL GIVE CONTROL TO NEXT PGM. *
059600*      3) IF PART NUMBER IS NOT OK IT WILL WRITE OUT A MSG ON    *
059700*         TERM.
059800******************************************************************
059900
060000 005-PROCESS-MSG.
060100
060200     MOVE 005    TO PARA-POINTER.
062100     MOVE 'N' TO WS100-EDIT-ERROR-SW.
060300
060400     PERFORM 010-EDIT-INPUT-MSG         THRU 010-EXIT.
060500
060600     IF EDIT-ERROR
060700        THEN
060800            PERFORM 030-ERROR-MSG THRU 030-EXIT
060900        ELSE
061000            PERFORM 020-MSGSW THRU 020-EXIT.
061100
061200     PERFORM 000-GET-MESSAGE  THRU 000-EXIT.
061300
061400 005-EXIT.
061500     EXIT.
061700 010-EDIT-INPUT-MSG.
061800     MOVE 010 TO PARA-POINTER.
061900
062000     IF PFKEY2 OR PFKEY1
062100        MOVE 'Y' TO WS100-EDIT-ERROR-SW
062200        MOVE WS-200-PFKEY-OUT-OF-SERV-MSG TO OUTPUT-MSG-ERROR
062300        GO TO 010-EXIT.
062400
062500     IF VALID-PFKEY
062600        THEN
062700            PERFORM 015-VERIFY-PART-NUMBER THRU 015-EXIT
062800        ELSE
062900            MOVE 'Y' TO WS100-EDIT-ERROR-SW
063000            MOVE WS-200-PFKEY-ERROR-MSG TO OUTPUT-MSG-ERROR.
063100 010-EXIT.
063200     EXIT.
063300
063400 015-VERIFY-PART-NUMBER.
063500
063600     MOVE 015  TO PARA-POINTER.
063700
064000     MOVE TP-IN-PART-NUMBER    TO VPARTSEG-KEY.
064100
064200     CALL 'CBLTDLI' USING  GU-FUNC
064300                           VPARTSUP-PCB
064400                           VPARTSEG-IO-AREA
064500                           SSA-VPARTSEG.
064600
064700     IF VPARTSUP-SUCCESSFUL-CALL
064800         NEXT SENTENCE
064900     ELSE
065000         MOVE 'Y' TO WS100-EDIT-ERROR-SW
065100         IF VPARTSUP-SEGMENT-NOT-FOUND
065200              MOVE WS-200-NOT-FOUND-MSG TO OUTPUT-MSG-ERROR
065300         ELSE
065400              GO TO 1001-CALL-IBMABND-VPARTSUP.
065500
065600 015-EXIT.
065700     EXIT.
065900 020-MSGSW.
066000     MOVE 020 TO PARA-POINTER.
066100
066200     CALL 'CBLTDLI' USING  CHNG-FUNC
066300                           ALT-IO-PCB
066400                           WS-TRANSACTION.
066500
066600     IF ALT-IO-CALL-SUCCESSFUL
066700         NEXT SENTENCE
066800     ELSE
066900       GO TO 1001-CALL-IBMABND-ALT-PCB.
067000
067100     MOVE TP-IN-PART-NUMBER TO MS-OUT-PART-NUMBER.
067200     MOVE WS-TRANSACTION TO MS-TRANSCODE-NAME.
067300     CALL 'CBLTDLI' USING  ISRT-FUNC
067400                           ALT-IO-PCB
067500                           MSG-SWITCH-OUTPUT-AREA.
067600
067700     IF ALT-IO-CALL-SUCCESSFUL
067800         NEXT SENTENCE
067900     ELSE
068000       GO TO 1001-CALL-IBMABND-ALT-PCB.
068100
068200 020-EXIT.
068300     EXIT.
068500 030-ERROR-MSG.
068600     MOVE 030    TO PARA-POINTER.
068700
068800     MOVE OUTPUT-MSG-ERROR  TO TP-OUT-ERROR-MSG.
068900
069000*>>> PUT IN YOUR MFS OUTPUT MOD-NAME HERE
069100     MOVE 'IMSONLN2 '      TO MOD-NAME.
069200
069300     PERFORM 002-SEND-MESSAGE THRU 002-EXIT.
069400
069500 030-EXIT.
069600     EXIT.
069800******************************************************************
069900*                                                                *
070000*    CALL IBMABND ABEND
070100*      1) PSB PROGRAM NAME                                       *
070200*      2) IO         PCB IN LINKAGE SECTION
070300*      3) DUMP OPTION DEFINED TO VALUE 'F'                       *
070400*                                                                *
070500******************************************************************
070600
070700 1001-CALL-IBMABND-IO-TERM.
070800     MOVE IO-STATUS-CODE          TO DUMP-STATUS.
070900     MOVE 'PROGRAM TERMINATION  ' TO DUMP-MESSAGE.
071000     CALL 'IBMABND' USING PSBPGM-NAME
071100                           IO-TERMINAL-PCB
071200                           DUMP-OPT.
071300******************************************************************
071400*                                                                *
071500*    CALL IBMABND ABEND
071600*      1) PSB PROGRAM NAME                                       *
071700*      2) ALT-IO         PCB IN LINKAGE SECTION
071800*      3) DUMP OPTION DEFINED TO VALUE 'F'                       *
071900*                                                                *
072000******************************************************************
072100
072200 1001-CALL-IBMABND-ALT-PCB.
072300     MOVE ALT-IO-STATUS-CODE    TO DUMP-STATUS.
072400     MOVE 'PROGRAM TERMINATION  ' TO DUMP-MESSAGE.
072500     CALL 'IBMABND' USING PSBPGM-NAME
072600                           ALT-IO-PCB
072700                           DUMP-OPT.
072800******************************************************************
072900*                                                                *
073000*    CALL IBMABND ABEND
073100*      1) PSB PROGRAM NAME                                       *
073200*      2) VPARTSUP PCB IN LINKAGE SECTION                        *
073300*      3) DUMP OPTION DEFINED TO VALUE 'F'                       *
073400*                                                                *
073500******************************************************************
073600
073700 1001-CALL-IBMABND-VPARTSUP.
073800     MOVE VPARTSUP-STATUS-CODE        TO DUMP-STATUS.
073900     MOVE 'PROGRAM TERMINATION  ' TO DUMP-MESSAGE.
074000     CALL 'IBMABND' USING PSBPGM-NAME
074100                           VPARTSUP-PCB
074200                           DUMP-OPT.

