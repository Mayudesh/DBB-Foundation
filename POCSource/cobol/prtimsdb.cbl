000010 ID DIVISION.                                                     11/02/89
000030 PROGRAM-ID. PRTIMSDB.                                               LV025
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
000430 DATA DIVISION.                                                      CL*24
000440 FILE SECTION.                                                       CL*24
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
006900
000600 01  DUMP-DISPLAY.                                                   CL*24
000610     05 PARTSUPP-FUNC              PIC X(4)  VALUE 'GN'.             CL*24
000620     05 GENERAL-IO-AREA            PIC X(111) VALUE ' '.             CL*24
042900
001320 01  IMS-WORK-AREA.                                                  CL*24
001330     05  PARM-CT                 PIC  S9(7) COMP SYNC VALUE +3.      CL*24
001340     05  PSBPGM-NAME             PIC  X(8) VALUE 'B99100 '.          CL*24
001350     05  DUMP-OPT                PIC  X    VALUE 'F'.                CL*24
001360     05  TIME-TO-END             PIC  X    VALUE 'N'.                CL*25
001370     05  CALL-FUNCTION           PIC  X(4) VALUE SPACES.             CL*24
001380                                                                     CL*24
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
002000     DISPLAY SPACES.                                                 CL*24
002010                                                                     CL*24
002080**************************************************************       CL*24
003520     PERFORM 1000-CALL-PARTSUPP-DB THRU 1000-EXIT UNTIL              CL*24
002120         PN-STATUS-CODE = 'GB'.
002140**************************************************************       CL*24
002330     GOBACK.                                                         CL*24
002340                                                                     CL*24
003760 1000-CALL-PARTSUPP-DB.                                              CL*24
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
003890     CALL 'CBLTDLI' USING  PARTSUPP-FUNC,                            CL*24
003910                           PARTFILE-PCB,
003920                            GENERAL-IO-AREA.                         CL*24
003960                                                                     CL*24
003980     IF  PN-STATUS-CODE = '  '
003990         DISPLAY GENERAL-IO-AREA                                     CL*24
004000     ELSE                                                            CL*24
004010         DISPLAY 'BAD DB PROBLEM'                                   CL*24
               GOBACK.
004030 1000-EXIT.                                                          CL*24
004040     EXIT.                                                           CL*24
004050                                                                     CL*24
