       IDENTIFICATION DIVISION.
       PROGRAM-ID.  IMSPROG1.
      *AUTHOR.  IBM.
      *REMARKS.
      *         DZRO PROGRAM

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  J              PIC S9(4) COMP VALUE ZERO.
       77  IDX1           PIC S9(4) COMP VALUE ZERO.
       77  IDX2           PIC S9(4) COMP VALUE ZERO.
       77  IDX3           PIC S9(4) COMP VALUE ZERO.
       77  IDX4           PIC S9(4) COMP VALUE ZERO.
       77  IDX5           PIC S9(4) COMP VALUE ZERO.
       77  RJ             PIC X(08) VALUE 'RJ'.
       77  RTIBM       PIC X(08) VALUE 'IMS90'.
       77  RTIBM1       PIC X(08) VALUE 'IMS91'.
       77  RTIBM2       PIC X(08) VALUE 'IMS92'.
       77  TBL-PRINTER    PIC X(08) VALUE '      '.
       77  ABEND          PIC X(08) VALUE 'ABEND'.
       77  MAINT-STATUS1   PIC X.
       77  READ-STATUS1   PIC X.
       77  DOF-SWITCH    PIC X.
       77  DATE-SW        PIC X.
       77  REPL-SW        PIC X.
       77  CHANGE-ADD-SW  PIC X.
       77  EOP-SW         PIC X.
       77  LENGTH-ERROR   PIC X.
       77  FIRST-TIME     PIC X.
       77  FIRST-INSERT   PIC X.
       77  DATA-PRESENT   PIC X.
       77  SCREEN2-AGAIN  PIC X.
       77  SCREEN2-FULL   PIC X.
       77  DUMVAL         PIC X.
       77  CRT-LENGTH     PIC S9(4) COMP VALUE +1800.
       77  LINE-CNT       PIC S9(4) COMP VALUE ZERO.
       77  SCREEN-CNT     PIC S9(4) COMP VALUE ZERO.
      ******************************************************************
      ******************************************************************
       01  RDZ01TIO.
           04  LL01-IN   PIC S9(4) COMP.
           04  Z001      PIC X(01).
           04  Z0012     PIC X(01).
           04  TIOAIO    PIC X(3000).
      ******************************************************************
      ** DZRIBM2 ********************************************************
      ******************************************************************
      ******************************************************************
       01  DZRIBM2-SCREEN.
           03  DZRIBM2-DATA.
               05 DZRIBM2-TRANSCODE       PIC X(79).
               05 DZRIBM2-PROGRAM-NO      PIC X(08).
               05 DZRIBM2-TYPEC           PIC X(01).
               05 DZRIBM2-TYPEA           PIC X(01).
               05 DZRIBM2-TYPE            PIC X(07).
               05 DZRIBM2-TABLE-IDC       PIC X(01).
               05 DZRIBM2-TABLE-IDA       PIC X(01).
               05 DZRIBM2-TABLE-ID        PIC X(32).
               05 DZRIBM2-ERR-MSG         PIC X(35).
               05 DZRIBM2-LAST-KEYC       PIC X(01).
               05 DZRIBM2-LAST-KEYA       PIC X(01).
               05 DZRIBM2-LAST-KEY        PIC X(20).
               05 DZRIBM2-PROCESS-IND     PIC X(01).
               05 DZRIBM2-STATUS          PIC X(01).
               05 DZRIBM2-ISRT-ROOT       PIC X(01).
               05 DZRIBM2-EXPAND          PIC X(13).
               05 DZRIBM2-DESCC           PIC X(01).
               05 DZRIBM2-DESCA           PIC X(01).
               05 DZRIBM2-DESC            PIC X(50).
               05 DZRIBM2-ELE-KEY-CNTC    PIC X(01).
               05 DZRIBM2-ELE-KEY-CNTA    PIC X(01).
               05 DZRIBM2-ELE-KEY-CNTX    PIC X(02).
               05 DZRIBM2-ELE-KEY-CNT REDEFINES DZRIBM2-ELE-KEY-CNTX
                                         PIC 9(02).
               05 DZRIBM2-ELE-CHRC        PIC X(01).
               05 DZRIBM2-ELE-CHRA        PIC X(01).
               05 DZRIBM2-ELE-CHRX        PIC X(02).
               05 DZRIBM2-ELE-CHR REDEFINES DZRIBM2-ELE-CHRX
                                         PIC 9(02).
               05 DZRIBM2-ACT-DATEC       PIC X(01).
               05 DZRIBM2-ACT-DATEA       PIC X(01).
               05 DZRIBM2-ACT-DATE        PIC X(08).
               05 DZRIBM2-TBL-PROTC       PIC X(01).
               05 DZRIBM2-TBL-PROTA       PIC X(01).
               05 DZRIBM2-TBL-PROT        PIC X(01).
               05 DZRIBM2-TBL-PSWC        PIC X(01).
               05 DZRIBM2-TBL-PSWA        PIC X(01).
               05 DZRIBM2-TBL-PSW         PIC X(08).
               05 DZRIBM2-ELE-NUM-KEYC    PIC X(01).
               05 DZRIBM2-ELE-NUM-KEYA    PIC X(01).
               05 DZRIBM2-ELE-NUM-KEY     PIC X(01).
               05 DZRIBM2-ELE-NUM-VALC    PIC X(01).
               05 DZRIBM2-ELE-NUM-VALA    PIC X(01).
               05 DZRIBM2-ELE-NUM-VAL     PIC X(01).
           03  DZRIBM2-MATRIX.
               05  CRT-LINE OCCURS 14 TIMES.
                   07  DZRIBM2-FUNCC      PIC X(01).
                   07  DZRIBM2-FUNCA      PIC X(01).
                   07  DZRIBM2-FUNC       PIC X(01).
                   07  DZRIBM2-ELE-KEYC   PIC X(01).
                   07  DZRIBM2-ELE-KEYA   PIC X(01).
                   07  DZRIBM2-ELE-KEY    PIC X(20).
                   07  DZRIBM2-ELE-DATAC  PIC X(01).
                   07  DZRIBM2-ELE-DATAA  PIC X(01).
                   07  DZRIBM2-ELE-DATA   PIC X(50).
           03  DZRIBM2-NEXT-FUNC.
               05  DZRIBM2-RETURNC        PIC X(01).
               05  DZRIBM2-RETURNA        PIC X(01).
               05  DZRIBM2-RETURN         PIC X(01).
               05  DZRIBM2-CONTC          PIC X(01).
               05  DZRIBM2-CONTA          PIC X(01).
               05  DZRIBM2-CONT           PIC X(01).
               05  DZRIBM2-ABORTC         PIC X(01).
               05  DZRIBM2-ABORTA         PIC X(01).
               05  DZRIBM2-ABORT          PIC X(01).
               05  DZRIBM2-NEXT-KEYC      PIC X(01).
               05  DZRIBM2-NEXT-KEYA      PIC X(01).
               05  DZRIBM2-NEXT-KEY       PIC X(20).
      ******************************************************************
      ** DZRIBM1 ********************************************************
      ******************************************************************
      ******************************************************************
       01  DZRIBM1-SCREEN.
           03  DZRIBM1-DATA.
               05 DZRIBM1-TRANSCODE   PIC X(79).
               05 DZRIBM1-PROGRAM-NO  PIC X(08).
               05 DZRIBM1-ERR-MSG     PIC X(35).
               05 DZRIBM1-FUNCTIONC   PIC X(01).
               05 DZRIBM1-FUNCTIONA   PIC X(01).
               05 DZRIBM1-FUNCTION    PIC X(01).
               05 DZRIBM1-TABLE-IDC   PIC X(01).
               05 DZRIBM1-TABLE-IDA   PIC X(01).
               05 DZRIBM1-TABLE-ID    PIC X(32).
               05 DZRIBM1-RTZ5C       PIC X(01).
               05 DZRIBM1-RTZ5A       PIC X(01).
               05 DZRIBM1-RTZ5        PIC X(20).
               05 DZRIBM1-PASSWORDC   PIC X(01).
               05 DZRIBM1-PASSWORDA   PIC X(01).
               05 DZRIBM1-PASSWORD    PIC X(08).
       01  GENERAL-END-SCREEN.
           03  ENDING-TRANCODEC    PIC X(01) VALUE SPACES.
           03  ENDING-TRANCODEA    PIC X(01) VALUE SPACES.
           03  ENDING-TRANCODE     PIC X(79) VALUE SPACES.
           03  END-LINE1           PIC X(60) VALUE
           '      * * * I B M  SUPPORT SYSTEMS * * *'.
           03  END-LINE2           PIC X(60) VALUE ' '.
           03  END-LINE3           PIC X(60) VALUE
           '            TRANCODE - MFDZRO - ENDED   '.
           03  END-LINE4           PIC X(60) VALUE ' '.
           03  END-LINE5           PIC X(60) VALUE ' '.
           03  END-LINE6           PIC X(60) VALUE ' '.
       01  FILLER3                 PIC X(4000).
       01  QUEUE-AREA.
           03  S-DZRIBM2-MATRIX.
               05  S-CRT-LINE OCCURS 14 TIMES.
                   07  S-M2-FUNCC      PIC X(01).
                   07  S-M2-FUNCA      PIC X(01).
                   07  S-M2-FUNC       PIC X(01).
                   07  S-M2-ELE-KEYC   PIC X(01).
                   07  S-M2-ELE-KEYA   PIC X(01).
                   07  S-M2-ELE-KEY    PIC X(20).
                   07  S-M2-ELE-DATAC  PIC X(01).
                   07  S-M2-ELE-DATAA  PIC X(01).
                   07  S-M2-ELE-DATA   PIC X(50).
      ******************************************************************
      ******************************************************************
      ******************************************************************
       01  DMHDRS.
           03  DMHDRS-KEY-GROUP.
               05  DMHDRS-KEY-SEQ          PIC X(08) VALUE LOW-VALUES.
               05  DMHDRS-KEY              PIC X(32).
           03  DMHDRS-TABLE-DESCRIPTOR.
               05  FILLER                    PIC X(50).
           03  DMHDRS-EXPANSION            PIC X(29).
           03  DMHDRS-TABLE-DELETE-IND     PIC X(01) VALUE SPACE.
           03  DMHDRS-TABLE-CHARACTERISTICS.
               05  DMHDRS-KEY-CHAR-CNT     PIC S9(3) COMP-3.
               05  DMHDRS-ELE-CHAR-CNT     PIC S9(3) COMP-3.
               05  DMHDRS-MAX-ELE-CNT      PIC S9(3) COMP-3 VALUE +0.
               05  DMHDRS-PASSWORD-PROT    PIC X(01) VALUE 'N'.
               05  DMHDRS-PASSWORD         PIC X(08) VALUE SPACES.
               05  DMHDRS-NUMERIC-KEY      PIC X(01) VALUE '0'.
               05  DMHDRS-NUMERIC-ELE      PIC X(01) VALUE '0'.
           03  DMHDRS-LAST-ACT-DATE        PIC 9(6) COMP-3.
           03  FILLER                        PIC X(03).
      ******************************************************************
      * DMELS *******************************************************
      ******************************************************************
       01  DMELS.
           03  DMELS-KEY-GROUP.
               05  DMELS-KEY-EXPANSION    PIC X(10) VALUE LOW-VALUES.
               05  DMELS-KEY              PIC X(20).
           03  DMELS-ELE-DATA.
               05  FILLER                    PIC X(133) VALUE SPACES.
           03  DMELS-TABLE-NUMERIC.
               05  FILLER                    PIC S9(15) COMP-3 VALUE +0.
           03  DMELS-EXPANSION.
               05  FILLER                    PIC X(53).
       01  SSA-PRT.
           03  HDRS-SEGNAME        PIC X(08) VALUE 'DMHDRS'.
           03  HDRS-COM            PIC X     VALUE '*'.
           03  HDRS-COM1           PIC X     VALUE '-'.
           03  HDRS-COM2           PIC X     VALUE '-'.
           03  HDRS-QUALIFY        PIC X     VALUE '('.
           03  HDRS-KEYFLD         PIC X(8)  VALUE 'SEQFIELD'.
           03  HDRS-RLO1           PIC XX    VALUE ' ='.
           03  HDRS-FLDVALUE0.
               05  HDRS-FLDVALUE2  PIC X(08) VALUE LOW-VALUES.
               05  HDRS-FLDVALUE1  PIC X(32).
           03  HDRS-ENDQUALIFY PIC X     VALUE ')'.
       01  SSA-PRT2.
           03  RTZ5-SEGNAME    PIC X(08) VALUE 'DMELS'.
           03  RTZ5-COM        PIC X     VALUE '*'.
           03  RTZ5-COM1       PIC X     VALUE '-'.
           03  RTZ5-COM2       PIC X     VALUE '-'.
           03  RTZ5-QUALIFY    PIC X     VALUE '('.
           03  RTZ5-KEYFLD     PIC X(8)  VALUE 'SEQFIELD'.
           03  RTZ5-RLO1       PIC XX    VALUE ' ='.
           03  RTZ5-FLDVALUE0.
               05  RTZ5-FLDVALUE2  PIC X(10) VALUE LOW-VALUES.
               05  RTZ5-FLDVALUE1  PIC X(20).
           03  RTZ5-ENDQUALIFY PIC X     VALUE ')'.
       01  IMS-FUNCTIONS.
           03  GU                      PIC X(4)      VALUE 'GU'.
           03  GN                      PIC X(4)      VALUE 'GN'.
           03  GNP                     PIC X(4)      VALUE 'GNP'.
           03  ISRT                    PIC X(4)      VALUE 'ISRT'.
           03  GHNP                    PIC X(4)      VALUE 'GHNP'.
           03  REPL                    PIC X(4)      VALUE 'REPL'.
           03  DLET                    PIC X(4)      VALUE 'DLET'.
           03  GHN                     PIC X(4)      VALUE 'GHN'.
           03  CHNG                    PIC X(4)      VALUE 'CHNG'.
           03  PURG                    PIC X(4)      VALUE 'PURG'.
           03  GHU                     PIC X(4)      VALUE 'GHU '.
       01  IMS-RETURN-CODES.
           03  BLNKS                      PIC XX        VALUE '  '.
           03  GE                      PIC XX        VALUE 'GE'.
           03  GB                      PIC XX        VALUE 'GB'.
           03  II                      PIC XX        VALUE 'II'.
           03  AI                      PIC XX        VALUE 'AI'.
           03  QC                      PIC XX        VALUE 'QC'.
           03  QD                      PIC XX        VALUE 'QD'.
       01  IMS-ATTRIBUTES-PRINT.
           03  UMDAN                   PIC X VALUE 'A'.
           03  UMDAD                   PIC X VALUE 'E'.
           03  ST0-3                   PIC X VALUE 'I'.
           03  UMDNH                   PIC X VALUE 'R'.
           03  PNMDD                   PIC X VALUE 'U'.
           03  PMDDD                   PIC X VALUE 'V'.
           03  PMDAN                   PIC X VALUE '3'.
           03  PNMDH                   PIC X VALUE 'Y'.
           03  CURSORPOS               PIC X VALUE '{'.
       01  PROGRAM-FIELDS.
           03  MSS-WORK-AREA.
               05  MSS-IN-LENGTH PIC S9(8) VALUE ZERO COMP.
               05  MSS-RETURN        PIC X VALUE 'X'.
           03  CURR-YMD.
               05  CURR-YMD-YY     PIC XX.
               05  CURR-YMD-MM     PIC XX.
               05  CURR-YMD-DD     PIC XX.
           03  CURR-YMD9  REDEFINES CURR-YMD PIC 9(6).
           03  DATE-YMD.
               05  DATE-YMD-YY     PIC XX.
               05  DATE-YMD-MM     PIC XX.
               05  DATE-YMD-DD     PIC XX.
           03  DATE-YMD9  REDEFINES DATE-YMD PIC 9(6).
           03  GREG-DATE.
               05  GREG-MM         PIC XX.
               05  FILLER          PIC X   VALUE '/'.
               05  GREG-DD         PIC XX.
               05  FILLER          PIC X   VALUE '/'.
               05  GREG-YY         PIC XX.
           03  9000-ABEND-CODE     PIC 9(8) COMP.
           03  GROUP-KEY-AREA      PIC X(50).
           03  FILLER REDEFINES GROUP-KEY-AREA.
               05  KEY-MATRIX OCCURS 50 TIMES PIC X(01).
           03  ERROR-MESSAGES.
               05  FILLER PIC X(35) VALUE
                   'NOT NUMERIC'.
               05  FILLER PIC X(35) VALUE
                   'INVALID VALUE'.
               05  FILLER PIC X(35) VALUE
                   'FIELD IS TOO LONG'.
               05  FILLER PIC X(35) VALUE
                   'FIELD REQUIRES INPUT OF DATA'.
               05  FILLER PIC X(35) VALUE
                   'RECORD NOT FOUND ON FILE'.
               05  FILLER PIC X(35) VALUE
                   'RECORD ALREADY EXISTS ON FILE'.
               05  FILLER PIC X(35) VALUE
                   'TOO SMALL MUST BE > THAN PREVIOUS'.
               05  FILLER PIC X(35) VALUE
                   'PUT "D" IN RETURN TO CONFIRM DELETE'.
               05  FILLER PIC X(35) VALUE
                   'PASSWORD IS INVALID'.
               05  FILLER PIC X(35) VALUE
                   'PRINTER ID INVALID'.
               05  FILLER PIC X(35) VALUE
                   'IMS LTERM ID ERROR'.
           03  FILLER REDEFINES ERROR-MESSAGES.
               05  ERROR-MESSAGE OCCURS 11 TIMES PIC X(35).
       LINKAGE SECTION.
       01  PCB-LT.
           02  PCBNAME         PIC X(8).
           02  FILLER          PIC X(2).
           02  STATUS-PCB         PIC X(2).
           02  PCBDATE         PIC 9(7)    COMP-3.
           02  PCBTIME         PIC 9(7)    COMP-3.
           02  PCBINSEQ        PIC S9(3)   COMP.
           02  FILLER          PIC X(2).
           02  PCBDOFNM        PIC X(8).
           02  PCBUSERID       PIC X(8).
       01  ALT-PCB.
           03  ALT-PCBNAME              PIC X(8).
           03  FILLER                   PIC XX.
           03  ALT-STATUS               PIC XX.
           03  ALT-DATE                 PIC S9(7) COMP-3.
           03  ALT-TIME                 PIC S9(7) COMP-3.
           03  ALT-SEQNO                PIC S9(3) COMP.
           03  FILLER                   PIC XX.
           03  ALT-DOFNAME              PIC X(8).
       01  DZRO-PCB.
           03  DZRO-DBD-NAME   PIC X(08).
           03  DZRO-LEVEL-NO   PIC XX.
           03  DZRO-STATUS     PIC XX.
           03  DZRO-PROCOPTS   PIC XXXX.
           03  DZRO-RESVD-DLI  PIC XXXX.
           03  DZRO-SEGMENT    PIC X(08).
           03  DZRO-FILLER     PIC X(8).
           03  DZRO-KEYFEEDBCK PIC X(100).
       PROCEDURE DIVISION.
           ENTRY 'DLITCBL' USING
                           PCB-LT
                           ALT-PCB
                           DZRO-PCB.
           MOVE SPACES TO DZRIBM1-PROGRAM-NO.
           PERFORM C-010-INITIALIZE THRU C-010-EXIT.
           PERFORM C-020-GET-MESSAGE THRU C-020-EXIT.
           PERFORM A-010-MAINLINE THRU A-010-EXIT
               UNTIL (STATUS-PCB NOT = BLNKS).
           GOBACK.
       A-010-MAINLINE.
           IF DZRIBM1-PROGRAM-NO NOT = 'RTIBM1' AND NOT = 'RTIBM2'
               MOVE '1' TO FIRST-TIME
               MOVE 'RTIBM1' TO DZRIBM1-PROGRAM-NO
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
           ELSE
               PERFORM A-030-PROCESS-TRAN THRU A-030-EXIT.
       A-010-EXIT.
           PERFORM C-010-INITIALIZE THRU C-010-EXIT.
           PERFORM C-020-GET-MESSAGE THRU C-020-EXIT.
       A-020-FIRST-TIME.
           IF FIRST-TIME = '1'
               MOVE 0 TO FIRST-TIME
               PERFORM C-021-FMT-EMPTY-SCREEN THRU C-021-EXIT
               MOVE CURSORPOS TO DZRIBM1-FUNCTIONC.
           IF IDX5 NOT = 0
               MOVE ERROR-MESSAGE (IDX5) TO DZRIBM1-ERR-MSG
           ELSE
               MOVE SPACES TO DZRIBM1-ERR-MSG.
           MOVE 'RTIBM1' TO DZRIBM1-PROGRAM-NO
           MOVE DZRIBM1-SCREEN TO TIOAIO
           CALL 'CBLTDLI' USING
               ISRT
               PCB-LT
               RDZ01TIO
               RTIBM1.
            IF STATUS-PCB = BLNKS
                 NEXT SENTENCE
            ELSE
                 MOVE 3505 TO 9000-ABEND-CODE
                 PERFORM C-099-ABEND.
       A-020-EXIT.
           EXIT.
       A-022-FIRST-TIME.
           IF IDX5 NOT = 0
               MOVE ERROR-MESSAGE (IDX5) TO DZRIBM2-ERR-MSG
           ELSE
               MOVE SPACES TO DZRIBM2-ERR-MSG.
           MOVE 'MFDZRO' TO DZRIBM2-TRANSCODE.
           MOVE 'RTIBM2' TO DZRIBM2-PROGRAM-NO.
           MOVE DZRIBM2-SCREEN TO TIOAIO.
           CALL 'CBLTDLI' USING
               ISRT
               PCB-LT
               RDZ01TIO
               RTIBM2.
           IF STATUS-PCB = BLNKS
                NEXT SENTENCE
           ELSE
                MOVE 3506 TO 9000-ABEND-CODE
                PERFORM C-099-ABEND.
       A-022-EXIT.
           EXIT.
       A-030-PROCESS-TRAN.
           IF DZRIBM1-PROGRAM-NO = 'RTIBM1'
               PERFORM A-035-DOF-MENU THRU A-035-EXIT
           ELSE
               PERFORM B-010-PROCESS-SCREEN2 THRU B-010-EXIT.
       A-030-EXIT.
           EXIT.
       A-035-DOF-MENU.
           MOVE '0' TO DOF-SWITCH.
           MOVE '0' TO FIRST-TIME.
           IF DZRIBM2-STATUS = '1'
               PERFORM B-010-PROCESS-SCREEN2 THRU B-010-EXIT
               GO TO A-035-EXIT.
           IF DZRIBM1-FUNCTION NOT = 'A' AND NOT = 'C' AND NOT = 'D'
            AND NOT = 'I' AND NOT = 'M' AND NOT = 'E' AND NOT = 'P'
               MOVE '1' TO DOF-SWITCH
               MOVE ST0-3 TO DZRIBM1-FUNCTIONA
               MOVE CURSORPOS TO DZRIBM1-FUNCTIONC
               IF IDX5 = 0
                   MOVE 2 TO IDX5.
           IF DZRIBM1-FUNCTION NOT = 'M' AND NOT = 'E' AND NOT = 'P'
               IF DZRIBM1-TABLE-ID = SPACES
                   MOVE '1' TO DOF-SWITCH
                   MOVE ST0-3 TO DZRIBM1-TABLE-IDA
                   MOVE CURSORPOS TO DZRIBM1-TABLE-IDC
                   IF IDX5 = 0
                       MOVE 4 TO IDX5.
           IF DOF-SWITCH = '1'
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO A-035-EXIT.
           IF DZRIBM1-FUNCTION = 'E'
               PERFORM C-100-RETURN THRU C-100-EXIT
               GO TO A-035-EXIT.
           PERFORM A-036-GET-HEADER THRU A-036-EXIT.
           IF DZRIBM1-FUNCTION NOT = 'A'
               IF DZRO-STATUS NOT = BLNKS
                   MOVE '1' TO DOF-SWITCH
                   MOVE CURSORPOS TO DZRIBM1-TABLE-IDC
                   MOVE ST0-3 TO DZRIBM1-TABLE-IDA
                   IF IDX5 = 0
                       MOVE 5 TO IDX5.
           IF DOF-SWITCH = '1'
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO A-035-EXIT.
           MOVE '1' TO FIRST-TIME.
           MOVE '1' TO DZRIBM2-STATUS.
           MOVE DZRIBM1-TABLE-ID TO DZRIBM2-TABLE-ID.
           MOVE DZRIBM1-FUNCTION TO DZRIBM2-PROCESS-IND.
           IF DZRIBM1-FUNCTION = 'A'
               MOVE 'ADD' TO DZRIBM2-TYPE.
           IF DZRIBM1-FUNCTION = 'C'
               MOVE 'CHANGE' TO DZRIBM2-TYPE.
           IF DZRIBM1-FUNCTION = 'D'
               MOVE 'DELETE' TO DZRIBM2-TYPE.
           IF DZRIBM1-FUNCTION = 'I'
               MOVE 'INQUIRY' TO DZRIBM2-TYPE.
           IF DZRIBM1-FUNCTION = 'A'
               PERFORM C-027-UNPROT-RTZ5 THRU C-027-EXIT
                VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 14
               IF DZRO-STATUS = BLNKS
                   MOVE '1' TO DOF-SWITCH
                   MOVE 6 TO IDX5
                   MOVE CURSORPOS TO DZRIBM1-TABLE-IDC
                   MOVE ST0-3 TO DZRIBM1-TABLE-IDA
                   PERFORM A-020-FIRST-TIME THRU A-020-EXIT
                   GO TO A-035-EXIT
               ELSE
                   MOVE '1' TO DZRIBM2-ISRT-ROOT
                   MOVE CURSORPOS TO DZRIBM2-DESCC
           ELSE
               MOVE '0' TO DZRIBM2-ISRT-ROOT
               MOVE CURSORPOS TO DZRIBM2-FUNCC (1)
               PERFORM A-060-LOAD-DZRIBM2 THRU A-060-EXIT
               IF SCREEN2-FULL = '0'
                   COMPUTE IDX1 = IDX1 - 1
                   PERFORM C-024-UNPROT-RTZ5 THRU C-024-EXIT
                    VARYING IDX2 FROM IDX1 BY 1 UNTIL IDX2 > 14.
           IF DZRIBM1-FUNCTION = 'I'
               PERFORM C-025-PROT-INQ THRU C-025-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               MOVE LOW-VALUES TO DZRIBM2-FUNCC (1)
               MOVE CURSORPOS TO DZRIBM2-RETURNC
               MOVE PMDAN TO DZRIBM2-DESCA
               MOVE PMDAN TO DZRIBM2-ELE-KEY-CNTA
               MOVE PMDAN TO DZRIBM2-ELE-CHRA
               MOVE PMDAN TO DZRIBM2-ACT-DATEA
               MOVE PMDAN TO DZRIBM2-TBL-PROTA
               MOVE PMDDD TO DZRIBM2-TBL-PSWA
               MOVE PMDAN TO DZRIBM2-ELE-NUM-KEYA
               MOVE PMDAN TO DZRIBM2-ELE-NUM-VALA.
           IF DZRIBM1-FUNCTION = 'A'
               PERFORM C-026-PROT-ISRT THRU C-026-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14.
           IF DZRIBM1-FUNCTION = 'D' OR 'C'
               IF DMHDRS-PASSWORD-PROT = 'Y'
                   IF DZRIBM1-PASSWORD NOT = DMHDRS-PASSWORD
                       MOVE CURSORPOS TO DZRIBM1-PASSWORDC
                       MOVE 9 TO IDX5
                       MOVE '0' TO FIRST-TIME
                       PERFORM A-020-FIRST-TIME THRU A-020-EXIT
                       GO TO A-035-EXIT.
           IF DZRIBM1-FUNCTION = 'D'
               PERFORM C-025-PROT-INQ THRU C-025-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               MOVE CURSORPOS TO DZRIBM2-RETURNC
               MOVE PMDAN TO DZRIBM2-DESCA
               MOVE PMDAN TO DZRIBM2-ELE-KEY-CNTA
               MOVE PMDAN TO DZRIBM2-ELE-CHRA
               MOVE PMDAN TO DZRIBM2-ACT-DATEA
               MOVE PMDAN TO DZRIBM2-TBL-PROTA
               MOVE PMDDD TO DZRIBM2-TBL-PSWA
               MOVE PMDAN TO DZRIBM2-ELE-NUM-KEYA
               MOVE PMDAN TO DZRIBM2-ELE-NUM-VALA
               MOVE LOW-VALUES TO DZRIBM2-FUNCC (1)
               MOVE CURSORPOS TO DZRIBM2-RETURNC
               MOVE 8 TO IDX5.
           PERFORM A-022-FIRST-TIME THRU A-022-EXIT.
       A-035-EXIT.
           EXIT.
       A-036-GET-HEADER.
           MOVE DZRIBM1-TABLE-ID TO HDRS-FLDVALUE1.
           PERFORM A-045-GHU-HDRS THRU A-045-EXIT.
       A-036-EXIT.
           EXIT.
       A-045-GHU-HDRS.
           MOVE '-' TO HDRS-COM1.
           CALL 'CBLTDLI' USING GHU
                                DZRO-PCB
                                DMHDRS
                                SSA-PRT.
           IF DZRO-STATUS NOT = BLNKS AND NOT = GE
               MOVE 3701 TO 9000-ABEND-CODE
               PERFORM C-099-ABEND THRU C-099-EXIT.
       A-045-EXIT.
           EXIT.
       A-060-LOAD-DZRIBM2.
           MOVE DMHDRS-TABLE-DESCRIPTOR TO DZRIBM2-DESC.
           MOVE DMHDRS-KEY-CHAR-CNT TO DZRIBM2-ELE-KEY-CNT.
           MOVE DMHDRS-ELE-CHAR-CNT TO DZRIBM2-ELE-CHR.
           MOVE DMHDRS-LAST-ACT-DATE TO DATE-YMD9.
           MOVE DATE-YMD-YY TO GREG-YY.
           MOVE DATE-YMD-MM TO GREG-MM.
           MOVE DATE-YMD-DD TO GREG-DD.
           MOVE GREG-DATE   TO DZRIBM2-ACT-DATE.
           MOVE DMHDRS-PASSWORD-PROT TO DZRIBM2-TBL-PROT.
           MOVE DMHDRS-PASSWORD TO DZRIBM2-TBL-PSW.
           MOVE DMHDRS-NUMERIC-KEY TO DZRIBM2-ELE-NUM-KEY.
           MOVE DMHDRS-NUMERIC-ELE TO DZRIBM2-ELE-NUM-VAL.
           MOVE '0' TO FIRST-TIME.
           IF DZRIBM1-FUNCTION = 'A' OR 'D'
               GO TO A-060-EXIT.
           MOVE '1' TO SCREEN2-FULL.
           IF DZRIBM1-RTZ5 = SPACES
               MOVE ' =' TO RTZ5-RLO1
               MOVE ' ' TO RTZ5-COM1
               MOVE '1' TO FIRST-TIME
               PERFORM A-070-GNP-RTZ5 THRU A-070-EXIT
               VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14 OR
               DZRO-STATUS NOT = BLNKS
               PERFORM A-070-GNP-RTZ5 THRU A-070-EXIT
           ELSE
               MOVE '-' TO RTZ5-COM1
               MOVE '=>' TO RTZ5-RLO1
               MOVE DZRIBM1-RTZ5 TO RTZ5-FLDVALUE1
               MOVE '1' TO FIRST-TIME
               PERFORM A-075-GNP-RTZ5 THRU A-075-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14 OR
               DZRO-STATUS NOT = BLNKS.
       A-060-EXIT.
           EXIT.
       A-065-UNPROT-DZRIBM2.
           MOVE PMDAN TO DZRIBM2-TABLE-IDA.
           MOVE UMDAN TO DZRIBM2-DESCA.
           MOVE UMDAN TO DZRIBM2-ELE-KEY-CNTA.
           MOVE UMDAN TO DZRIBM2-ELE-CHRA.
           MOVE UMDAN TO DZRIBM2-ACT-DATEA.
           MOVE UMDAN TO DZRIBM2-TBL-PROTA.
           MOVE UMDAD TO DZRIBM2-TBL-PSWA.
           MOVE UMDAN TO DZRIBM2-ELE-NUM-KEYA.
           MOVE UMDAN TO DZRIBM2-ELE-NUM-VALA.
       A-065-EXIT.
           EXIT.
       A-070-GNP-RTZ5.
           MOVE ' ' TO RTZ5-COM1.
           CALL 'CBLTDLI' USING GNP
                                DZRO-PCB
                                DMELS
                                SSA-PRT
                                SSA-PRT2.
           IF IDX1 = 15
               IF DZRO-STATUS  = GE
                   MOVE SPACES TO DZRIBM2-NEXT-KEY
                   GO TO A-070-EXIT
               ELSE
                   MOVE DMELS-KEY TO DZRIBM2-NEXT-KEY
                   GO TO A-070-EXIT.
           IF DZRO-STATUS  = GE
               MOVE '0' TO SCREEN2-FULL
               GO TO A-070-EXIT.
           IF DZRO-STATUS NOT = BLNKS AND NOT = GE
               MOVE 3702 TO 9000-ABEND-CODE
               PERFORM C-099-ABEND THRU C-099-EXIT.
           PERFORM A-080-BUILD-RTZ5-DZRIBM2 THRU A-080-EXIT.
       A-070-EXIT.
           EXIT.
       A-075-GNP-RTZ5.
           CALL 'CBLTDLI' USING GNP
                                DZRO-PCB
                                DMELS
                                SSA-PRT
                                SSA-PRT2.
           IF FIRST-TIME = '1'
               MOVE '0' TO FIRST-TIME
               MOVE ' =' TO RTZ5-RLO1
               MOVE SPACE TO RTZ5-COM1.
           IF IDX1 = 15
               IF DZRO-STATUS  = GE
                   MOVE SPACES TO DZRIBM2-NEXT-KEY
                   GO TO A-075-EXIT
               ELSE
                   MOVE DMELS-KEY TO DZRIBM2-NEXT-KEY
                   GO TO A-075-EXIT.
           IF DZRO-STATUS NOT = BLNKS AND NOT = GE
               MOVE 3702 TO 9000-ABEND-CODE
               PERFORM C-099-ABEND THRU C-099-EXIT.
           IF DZRO-STATUS  = GE
               MOVE '0' TO SCREEN2-FULL
               MOVE SPACES TO DZRIBM2-NEXT-KEY
               GO TO A-075-EXIT.
           MOVE DMELS-KEY TO DZRIBM2-NEXT-KEY.
           PERFORM A-080-BUILD-RTZ5-DZRIBM2 THRU A-080-EXIT.
       A-075-EXIT.
           EXIT.
       A-080-BUILD-RTZ5-DZRIBM2.
           MOVE DMELS-KEY TO DZRIBM2-LAST-KEY.
           MOVE DMELS-KEY TO DZRIBM2-ELE-KEY (IDX1).
           MOVE DMELS-ELE-DATA TO DZRIBM2-ELE-DATA (IDX1).
       A-080-EXIT.
           EXIT.
       B-010-PROCESS-SCREEN2.
           MOVE LOW-VALUES TO DZRIBM1-DATA.
           IF DZRIBM2-PROCESS-IND = 'A'
               PERFORM B-020-ADD-DZRIBM2 THRU B-020-EXIT.
           IF DZRIBM2-PROCESS-IND = 'C'
               PERFORM B-050-CHG THRU B-050-EXIT.
           IF DZRIBM2-PROCESS-IND = 'D'
               PERFORM B-060-DEL THRU B-060-EXIT.
           IF DZRIBM2-PROCESS-IND = 'I'
               PERFORM B-080-DISPLAY THRU B-080-EXIT.
       B-010-EXIT.
           EXIT.
       B-020-ADD-DZRIBM2.
           IF DZRIBM2-CONT < 'A' AND
              DZRIBM2-RETURN < 'A' AND
              DZRIBM2-ABORT < 'A'
               MOVE 'X' TO DZRIBM2-CONT.
           IF DZRIBM2-CONT = 'D'
               MOVE ' ' TO DZRIBM2-CONT
               PERFORM B-022-DUPLICATE THRU B-022-EXIT
                VARYING J FROM 2 BY 1 UNTIL J > 14
               PERFORM A-022-FIRST-TIME THRU A-022-EXIT
               GO TO B-020-EXIT.
           IF  DZRIBM2-ABORT NOT = SPACE AND NOT = '?'
               MOVE '1' TO FIRST-TIME
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO B-020-EXIT.
           IF DZRIBM2-ISRT-ROOT = '1'
               PERFORM B-029-DOF-ROOT-DATA THRU B-029-EXIT
               IF DOF-SWITCH = '1'
                   MOVE 1 TO IDX1
                   PERFORM C-027-UNPROT-RTZ5 THRU C-027-EXIT
                    VARYING IDX2 FROM IDX1 BY 1 UNTIL IDX2 > 14
                   PERFORM A-022-FIRST-TIME THRU A-022-EXIT
                   GO TO B-020-EXIT.
           IF DZRIBM2-ISRT-ROOT = '1'
               MOVE '0' TO DZRIBM2-ISRT-ROOT
               MOVE '1' TO DATE-SW
               PERFORM B-055-REPL-ACT-DATE THRU B-055-EXIT
               PERFORM B-025-ISRT-ROOT THRU B-025-EXIT
           ELSE
               MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1
               PERFORM A-045-GHU-HDRS THRU A-045-EXIT.
           IF DZRIBM2-ISRT-ROOT = '0'
               PERFORM B-029-DOF-ROOT-DATA THRU B-029-EXIT
               IF DOF-SWITCH = '1'
                   PERFORM A-022-FIRST-TIME THRU A-022-EXIT
                   GO TO B-020-EXIT
               ELSE
                   PERFORM B-026-REPL-ROOT THRU B-026-EXIT.
           PERFORM B-040-ADD THRU B-040-EXIT
            VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14.
           IF SCREEN2-AGAIN = '1'
               MOVE 1 TO IDX1
               COMPUTE IDX3 = IDX3 + 1
               PERFORM C-027-UNPROT-RTZ5 THRU C-027-EXIT
                VARYING IDX2 FROM IDX3 BY 1 UNTIL IDX2 > 14
               PERFORM C-026-PROT-ISRT THRU C-026-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               MOVE QUEUE-AREA TO DZRIBM2-MATRIX
               PERFORM A-022-FIRST-TIME THRU A-022-EXIT
               GO TO B-020-EXIT.
           IF DZRIBM2-RETURN NOT = SPACE OR DZRIBM2-ABORT NOT = SPACE
               MOVE '1' TO FIRST-TIME
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO B-020-EXIT.
           IF DZRIBM2-CONT NOT = SPACE
               MOVE LOW-VALUES TO DZRIBM2-MATRIX
               MOVE 1 TO IDX1
               PERFORM C-027-UNPROT-RTZ5 THRU C-027-EXIT
                VARYING IDX2 FROM IDX1 BY 1 UNTIL IDX2 > 14
               PERFORM C-026-PROT-ISRT THRU C-026-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14.
           MOVE CURSORPOS TO DZRIBM2-ELE-KEYC (1).
           PERFORM A-022-FIRST-TIME THRU A-022-EXIT.
       B-020-EXIT.
           EXIT.
       B-022-DUPLICATE.
           MOVE UMDAN TO DZRIBM2-ELE-KEYA (J).
           MOVE LOW-VALUES TO DZRIBM2-ELE-KEYC(J).
           MOVE UMDAN TO DZRIBM2-ELE-DATAA (J).
           MOVE LOW-VALUES TO DZRIBM2-ELE-DATAC(J).
           MOVE UMDAN TO DZRIBM2-FUNCA (J).
           MOVE LOW-VALUES TO DZRIBM2-FUNCC(J).
           MOVE DZRIBM2-ELE-KEY (1) TO DZRIBM2-ELE-KEY (J).
           MOVE DZRIBM2-ELE-DATA (1) TO DZRIBM2-ELE-DATA (J).
           MOVE 'A' TO DZRIBM2-FUNC (J).
       B-022-EXIT.
           EXIT.
       B-025-ISRT-ROOT.
           MOVE DZRIBM2-TABLE-ID    TO DMHDRS-KEY.
           MOVE DZRIBM2-TABLE-ID    TO HDRS-FLDVALUE1.
           MOVE DZRIBM2-DESC        TO DMHDRS-TABLE-DESCRIPTOR.
           MOVE DZRIBM2-ELE-KEY-CNT TO DMHDRS-KEY-CHAR-CNT.
           MOVE DZRIBM2-ELE-CHR     TO DMHDRS-ELE-CHAR-CNT.
           MOVE DZRIBM2-TBL-PROT    TO DMHDRS-PASSWORD-PROT.
           MOVE DZRIBM2-TBL-PSW     TO DMHDRS-PASSWORD.
           MOVE DZRIBM2-ELE-NUM-KEY TO DMHDRS-NUMERIC-KEY.
           MOVE DZRIBM2-ELE-NUM-VAL TO DMHDRS-NUMERIC-ELE.
           MOVE SPACE TO HDRS-COM1.
           CALL 'CBLTDLI' USING ISRT
                                DZRO-PCB
                                DMHDRS
                                SSA-PRT.
           IF DZRO-STATUS NOT = BLNKS
               MOVE 3702 TO 9000-ABEND-CODE
               PERFORM C-099-ABEND THRU C-099-EXIT.
       B-025-EXIT.
           EXIT.
       B-026-REPL-ROOT.
           IF DATE-SW = '1'
               MOVE CURR-YMD9 TO DMHDRS-LAST-ACT-DATE.
           MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1.
           PERFORM A-045-GHU-HDRS THRU A-045-EXIT.
            IF (DZRIBM2-DESC = DMHDRS-TABLE-DESCRIPTOR AND
                DZRIBM2-ELE-KEY-CNT = DMHDRS-KEY-CHAR-CNT AND
                 DZRIBM2-ELE-CHR = DMHDRS-ELE-CHAR-CNT AND
                   DZRIBM2-TBL-PROT = DMHDRS-PASSWORD-PROT AND
                    DZRIBM2-TBL-PSW = DMHDRS-PASSWORD AND
                     DZRIBM2-ELE-NUM-KEY = DMHDRS-NUMERIC-KEY AND
                      DZRIBM2-ELE-NUM-VAL = DMHDRS-NUMERIC-ELE) AND
                       DATE-SW = '0'
                       NEXT SENTENCE
                   ELSE
                       MOVE DZRIBM2-DESC TO DMHDRS-TABLE-DESCRIPTOR
                       MOVE DZRIBM2-ELE-KEY-CNT TO DMHDRS-KEY-CHAR-CNT
                       MOVE DZRIBM2-ELE-CHR TO DMHDRS-ELE-CHAR-CNT
                       MOVE CURR-YMD9   TO DMHDRS-LAST-ACT-DATE
                       MOVE CURR-YMD-YY TO GREG-YY
                       MOVE CURR-YMD-MM TO GREG-MM
                       MOVE CURR-YMD-DD TO GREG-DD
                       MOVE GREG-DATE   TO DZRIBM2-ACT-DATE
                       MOVE DZRIBM2-TBL-PROT TO DMHDRS-PASSWORD-PROT
                       MOVE DZRIBM2-TBL-PSW TO DMHDRS-PASSWORD
                       MOVE DZRIBM2-ELE-NUM-KEY TO DMHDRS-NUMERIC-KEY
                       MOVE DZRIBM2-ELE-NUM-VAL TO DMHDRS-NUMERIC-ELE
                       MOVE SPACE TO HDRS-COM1
                       CALL 'CBLTDLI' USING REPL
                                            DZRO-PCB
                                            DMHDRS
                                            SSA-PRT
                       IF DZRO-STATUS NOT = BLNKS
                           MOVE 3703 TO 9000-ABEND-CODE
                           PERFORM C-099-ABEND THRU C-099-EXIT.
       B-026-EXIT.
           EXIT.
       B-028-ISRT-RTZ5.
           IF DZRIBM2-ELE-KEY (IDX1) = SPACES
               GO TO B-028-EXIT.
           MOVE '-' TO HDRS-COM1.
           MOVE ' ' TO RTZ5-COM1.
           MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1.
           MOVE DZRIBM2-ELE-KEY (IDX1) TO DMELS-KEY.
           MOVE DZRIBM2-ELE-DATA (IDX1) TO DMELS-ELE-DATA.
           MOVE ' ' TO RTZ5-COM1.
           CALL 'CBLTDLI' USING ISRT
                                DZRO-PCB
                                DMELS
                                SSA-PRT
                                SSA-PRT2.
           IF DZRO-STATUS NOT = BLNKS AND NOT = II
               MOVE 3704 TO 9000-ABEND-CODE
               PERFORM C-099-ABEND THRU C-099-EXIT.
       B-028-EXIT.
           EXIT.
       B-029-DOF-ROOT-DATA.
           IF DZRIBM2-DESC = SPACES OR DZRIBM2-DESC = '?????'
               MOVE ALL '?' TO DZRIBM2-DESC
               MOVE '1' TO DOF-SWITCH
               MOVE ST0-3 TO DZRIBM2-DESCA
               MOVE CURSORPOS TO DZRIBM2-DESCC
               IF IDX5 = 0
                   MOVE 4 TO IDX5.
           MOVE SPACE TO MSS-RETURN.
           MOVE 2 TO MSS-IN-LENGTH.
           CALL RJ USING
             MSS-RETURN
             DZRIBM2-ELE-KEY-CNT
             MSS-IN-LENGTH.
           IF MSS-RETURN = SPACE
               IF DZRIBM2-ELE-KEY-CNT < 1 OR > 20
                   MOVE ST0-3 TO DZRIBM2-ELE-KEY-CNTA
                   MOVE CURSORPOS TO DZRIBM2-ELE-KEY-CNTC
                   MOVE '1' TO DOF-SWITCH
                   IF IDX5 = 0
                       MOVE 2 TO IDX5
                   ELSE
                       NEXT SENTENCE
               ELSE
                   NEXT SENTENCE
           ELSE
               MOVE ST0-3 TO DZRIBM2-ELE-KEY-CNTA
               MOVE CURSORPOS TO DZRIBM2-ELE-KEY-CNTC
               MOVE '1' TO DOF-SWITCH
               IF IDX5 = 0
                   MOVE 1 TO IDX5.
           MOVE SPACE TO MSS-RETURN.
           MOVE 2 TO MSS-IN-LENGTH.
           CALL RJ USING
             MSS-RETURN
             DZRIBM2-ELE-CHR
             MSS-IN-LENGTH.
           IF MSS-RETURN = SPACE
               IF DZRIBM2-ELE-CHR > 50
                   MOVE ST0-3 TO DZRIBM2-ELE-CHRA
                   MOVE CURSORPOS TO DZRIBM2-ELE-CHRC
                   MOVE '1' TO DOF-SWITCH
                   IF IDX5 = 0
                       MOVE 2 TO IDX5
                   ELSE
                       NEXT SENTENCE
               ELSE
                   NEXT SENTENCE
           ELSE
               MOVE ST0-3 TO DZRIBM2-ELE-CHRA
               MOVE CURSORPOS TO DZRIBM2-ELE-CHRC
               MOVE '1' TO DOF-SWITCH
               IF IDX5 = 0
                   MOVE 1 TO IDX5.
           IF DZRIBM2-TBL-PROT NOT = 'Y' AND NOT = 'N' AND NOT = ' '
               MOVE '1' TO DOF-SWITCH
               MOVE ST0-3 TO DZRIBM2-TBL-PROTA
               MOVE CURSORPOS TO DZRIBM2-TBL-PROTC
               IF IDX5 = 0
                   MOVE 2 TO IDX5.
           IF DZRIBM2-TBL-PROT = SPACES
               MOVE 'N' TO DZRIBM2-TBL-PROT.
           IF DZRIBM2-TBL-PROT = 'Y'
               IF DZRIBM2-TBL-PSW = SPACES
                   MOVE '1' TO DOF-SWITCH
                   MOVE UMDAD TO DZRIBM2-TBL-PSWA
                   MOVE CURSORPOS TO DZRIBM2-TBL-PSWC
                   IF IDX5 = 0
                       MOVE 4 TO IDX5.
           IF DZRIBM2-ELE-NUM-KEY NOT = 'Y' AND NOT = 'N' AND NOT = ' '
               MOVE '1' TO DOF-SWITCH
               MOVE ST0-3 TO DZRIBM2-ELE-NUM-KEYA
               MOVE CURSORPOS TO DZRIBM2-ELE-NUM-KEYC
               IF IDX5 = 0
                   MOVE 2 TO IDX5.
           IF DZRIBM2-ELE-NUM-VAL NOT = 'Y' AND NOT = 'N' AND NOT = ' '
               MOVE '1' TO DOF-SWITCH
               MOVE ST0-3 TO DZRIBM2-ELE-NUM-VALA
               MOVE CURSORPOS TO DZRIBM2-ELE-NUM-VALC
               IF IDX5 = 0
                   MOVE 2 TO IDX5.
           IF DZRIBM2-ISRT-ROOT = '1'
               GO TO B-029-EXIT.
           IF DOF-SWITCH = '1'
               GO TO B-029-EXIT.
           IF DZRIBM2-ELE-KEY-CNT < DMHDRS-KEY-CHAR-CNT
               MOVE ST0-3 TO DZRIBM2-ELE-KEY-CNTA
               MOVE CURSORPOS TO DZRIBM2-ELE-KEY-CNTC
               MOVE '1' TO DOF-SWITCH
               IF IDX5 = 0
                   MOVE 7 TO IDX5.
           IF DZRIBM2-ELE-CHR < DMHDRS-ELE-CHAR-CNT
               MOVE ST0-3 TO DZRIBM2-ELE-CHRA
               MOVE CURSORPOS TO DZRIBM2-ELE-CHRC
               MOVE '1' TO DOF-SWITCH
               IF IDX5 = 0
                   MOVE 7 TO IDX5.
       B-029-EXIT.
           EXIT.
       B-040-ADD.
           MOVE UMDAN TO DZRIBM2-ELE-KEYA (IDX1).
           MOVE 'A' TO DZRIBM2-FUNC (IDX1).
           MOVE PMDAN TO DZRIBM2-FUNCA (IDX1).
           IF DZRIBM2-ELE-KEY (IDX1) = SPACES
               GO TO B-040-EXIT.
           PERFORM B-056-CHECK-KEY-CNT THRU B-056-EXIT.
           IF LENGTH-ERROR = '1'
               MOVE '1' TO DOF-SWITCH
               MOVE CURSORPOS TO DZRIBM2-ELE-KEYC (IDX1)
               MOVE ST0-3 TO DZRIBM2-ELE-KEYA (IDX1)
               IF IDX5 = 0
                   MOVE 3 TO IDX5.
           PERFORM B-058-CHECK-CHR-CNT THRU B-058-EXIT.
           IF LENGTH-ERROR = '1'
               MOVE '1'       TO DOF-SWITCH
               MOVE CURSORPOS TO DZRIBM2-ELE-DATAC (IDX1)
               MOVE ST0-3     TO DZRIBM2-ELE-DATAA (IDX1)
               IF IDX5 = 0
                   MOVE 3 TO IDX5.
           IF DOF-SWITCH = '0'
              PERFORM B-028-ISRT-RTZ5 THRU B-028-EXIT
               IF DZRO-STATUS = II
                   MOVE '1' TO DOF-SWITCH
                   MOVE CURSORPOS TO DZRIBM2-FUNCC (IDX1)
                   MOVE ST0-3 TO DZRIBM2-FUNCA (IDX1)
                   COMPUTE IDX3 = IDX3 + 1
                   MOVE CRT-LINE (IDX1) TO S-CRT-LINE (IDX3)
                   MOVE '1' TO SCREEN2-AGAIN
                   IF IDX5 = 0
                       MOVE 6 TO IDX5
                   ELSE
                       NEXT SENTENCE
              ELSE
                   MOVE '0' TO DOF-SWITCH
           ELSE
              COMPUTE IDX3 = IDX3 + 1
              MOVE CRT-LINE (IDX1) TO S-CRT-LINE (IDX3)
              MOVE '1' TO SCREEN2-AGAIN
              MOVE '0' TO DOF-SWITCH.
       B-040-EXIT.
           EXIT.
       B-050-CHG.
           IF DZRIBM2-CONT = 'D'
               MOVE ' ' TO DZRIBM2-CONT
               PERFORM B-022-DUPLICATE THRU B-022-EXIT
                VARYING J FROM 2 BY 1 UNTIL J > 14
               PERFORM A-022-FIRST-TIME THRU A-022-EXIT
               GO TO B-050-EXIT.
           MOVE '0' TO DOF-SWITCH.
           MOVE '0' TO CHANGE-ADD-SW.
           MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1.
           PERFORM A-045-GHU-HDRS THRU A-045-EXIT.
           IF DZRIBM2-CONT < 'A' AND
              DZRIBM2-RETURN < 'A' AND
              DZRIBM2-ABORT < 'A'
               MOVE 'X' TO DZRIBM2-CONT.
           IF DZRIBM2-ABORT NOT = SPACE AND NOT = '?'
               MOVE '1' TO FIRST-TIME
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO B-050-EXIT.
           PERFORM B-029-DOF-ROOT-DATA THRU B-029-EXIT.
           IF DOF-SWITCH = '1'
               PERFORM A-022-FIRST-TIME THRU A-022-EXIT
               GO TO B-050-EXIT
           ELSE
               MOVE '0' TO DATE-SW
               PERFORM B-052-UPDATE THRU B-052-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               PERFORM B-055-REPL-ACT-DATE THRU B-055-EXIT
               PERFORM B-026-REPL-ROOT THRU B-026-EXIT.
           IF SCREEN2-AGAIN = '1'
               MOVE 1 TO IDX1
               COMPUTE IDX3 = IDX3 + 1
               PERFORM C-024-UNPROT-RTZ5 THRU C-024-EXIT
                VARYING IDX2 FROM IDX3 BY 1 UNTIL IDX2 > 14
               MOVE QUEUE-AREA TO DZRIBM2-MATRIX
               PERFORM A-022-FIRST-TIME THRU A-022-EXIT
               GO TO B-050-EXIT.
           IF DZRIBM2-RETURN NOT = SPACE OR DZRIBM2-ABORT NOT = SPACE
               MOVE '1' TO FIRST-TIME
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO B-050-EXIT.
           IF DZRIBM2-CONT NOT = SPACE
               IF CHANGE-ADD-SW = '1'
                   MOVE LOW-VALUES TO DZRIBM2-MATRIX
                   MOVE 1 TO IDX1
                   PERFORM C-027-UNPROT-RTZ5 THRU C-027-EXIT
                    VARYING IDX2 FROM IDX1 BY 1 UNTIL IDX2 > 14
                   PERFORM C-024-UNPROT-RTZ5 THRU C-024-EXIT
                    VARYING IDX2 FROM 1 BY 1 UNTIL IDX2 > 14
                   MOVE CURSORPOS TO DZRIBM2-ELE-KEYC (1)
                   PERFORM A-022-FIRST-TIME THRU A-022-EXIT
                   GO TO B-050-EXIT.
           IF DZRIBM2-CONT NOT = SPACE
               IF DZRIBM2-NEXT-KEY NOT = SPACES
                   MOVE DZRIBM2-NEXT-KEY TO DZRIBM2-LAST-KEY.
           IF DZRIBM2-CONT NOT = SPACE
               MOVE LOW-VALUES TO DZRIBM2-MATRIX
               PERFORM C-023-PROT-RTZ5 THRU C-023-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               MOVE '1' TO FIRST-TIME
               MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1
               PERFORM A-045-GHU-HDRS THRU A-045-EXIT
               MOVE DZRIBM2-NEXT-KEY TO RTZ5-FLDVALUE1
               MOVE '=>' TO RTZ5-RLO1
               MOVE '-' TO RTZ5-COM1
               MOVE '1' TO FIRST-TIME
                           SCREEN2-FULL
               PERFORM A-075-GNP-RTZ5 THRU A-075-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14 OR
               DZRO-STATUS NOT = BLNKS
               PERFORM A-075-GNP-RTZ5 THRU A-075-EXIT
               PERFORM B-051-DASH-CHANGE THRU B-051-EXIT
           ELSE
               MOVE LOW-VALUES TO DZRIBM2-MATRIX
               MOVE SPACES TO DZRIBM2-NEXT-KEY
               PERFORM C-023-PROT-RTZ5 THRU C-023-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               MOVE '1' TO FIRST-TIME
               MOVE CURSORPOS TO DZRIBM2-FUNCC (1)
               PERFORM A-022-FIRST-TIME THRU A-022-EXIT.
       B-050-EXIT.
           EXIT.
       B-051-DASH-CHANGE.
           IF DZRO-STATUS = GE OR GB
               MOVE SPACES TO DZRIBM2-NEXT-KEY
               MOVE CURSORPOS TO DZRIBM2-FUNCC (1)
           ELSE
               MOVE DMELS-KEY TO DZRIBM2-NEXT-KEY
               MOVE CURSORPOS TO DZRIBM2-FUNCC (1).
           IF SCREEN2-FULL = '0'
               COMPUTE IDX1 = IDX1 - 1
               PERFORM C-024-UNPROT-RTZ5 THRU C-024-EXIT
                VARYING IDX2 FROM IDX1 BY 1 UNTIL IDX2 > 14.
           PERFORM A-022-FIRST-TIME THRU A-022-EXIT.
       B-051-EXIT.
           EXIT.
       B-052-UPDATE.
           IF DZRIBM2-FUNC (IDX1) = 'I'
               GO TO B-052-EXIT.
           IF DZRIBM2-FUNC (IDX1) = SPACES
               GO TO B-052-EXIT.
           IF DZRIBM2-ELE-KEY (IDX1) = SPACES
               GO TO B-052-EXIT.
           IF DZRIBM2-FUNC (IDX1) = 'C' OR 'A'
               PERFORM B-056-CHECK-KEY-CNT THRU B-056-EXIT
               IF LENGTH-ERROR = '1'
                   MOVE '1' TO DOF-SWITCH
                   MOVE CURSORPOS TO DZRIBM2-ELE-KEYC (IDX1)
                   MOVE ST0-3 TO DZRIBM2-ELE-KEYA (IDX1)
                   IF IDX5 = 0
                       MOVE 3 TO IDX5.
           IF DZRIBM2-FUNC (IDX1) = 'C' OR 'A'
               PERFORM B-058-CHECK-CHR-CNT THRU B-058-EXIT
               IF LENGTH-ERROR = '1'
                   MOVE '1' TO DOF-SWITCH
                   MOVE CURSORPOS TO DZRIBM2-ELE-DATAC (IDX1)
                   MOVE ST0-3 TO DZRIBM2-ELE-DATAA (IDX1)
                   IF IDX5 = 0
                       MOVE 3 TO IDX5.
           IF DZRIBM2-FUNC (IDX1) NOT = 'A' AND NOT = 'C' AND NOT = 'D'
               MOVE '1' TO DOF-SWITCH
               MOVE CURSORPOS TO DZRIBM2-FUNCC (IDX1)
               MOVE ST0-3 TO DZRIBM2-FUNCA (IDX1)
               IF IDX5 = 0
                   MOVE 2 TO IDX5.
           IF DOF-SWITCH = '1'
               COMPUTE IDX3 = IDX3 + 1
               MOVE CRT-LINE (IDX1) TO S-CRT-LINE (IDX3)
               MOVE '1' TO SCREEN2-AGAIN
               MOVE '0' TO DOF-SWITCH
               GO TO B-052-EXIT.
           IF DZRIBM2-FUNC (IDX1) = 'C'
               PERFORM B-054-REPL-DLET THRU B-054-EXIT.
           IF DZRIBM2-FUNC (IDX1) = 'D'
               PERFORM B-054-REPL-DLET THRU B-054-EXIT.
           IF DZRIBM2-FUNC (IDX1) = 'A'
               PERFORM B-053-ISRT THRU B-053-EXIT.
           IF DOF-SWITCH = '1'
               COMPUTE IDX3 = IDX3 + 1
               MOVE CRT-LINE (IDX1) TO S-CRT-LINE (IDX3)
               MOVE '1' TO SCREEN2-AGAIN
               MOVE '0' TO DOF-SWITCH
               GO TO B-052-EXIT.
       B-052-EXIT.
           EXIT.
       B-053-ISRT.
           MOVE '-' TO HDRS-COM1.
           MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1.
           MOVE DZRIBM2-ELE-KEY (IDX1) TO RTZ5-FLDVALUE1.
           PERFORM B-028-ISRT-RTZ5 THRU B-028-EXIT.
           MOVE '1' TO CHANGE-ADD-SW.
           IF DZRO-STATUS NOT = II
               MOVE '1' TO DATE-SW.
           IF DZRO-STATUS = II
               MOVE '1' TO DOF-SWITCH
               MOVE CURSORPOS TO DZRIBM2-FUNCC (IDX1)
               MOVE ST0-3 TO DZRIBM2-FUNCA (IDX1)
               IF IDX5 = 0
                   MOVE 6 TO IDX5.
       B-053-EXIT.
           EXIT.
       B-054-REPL-DLET.
           MOVE '-' TO HDRS-COM1
           MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1
           MOVE DZRIBM2-ELE-KEY (IDX1) TO RTZ5-FLDVALUE1
           PERFORM B-070-GHU-RTZ5 THRU B-070-EXIT
           IF DZRO-STATUS NOT = BLNKS
               MOVE '1' TO DOF-SWITCH
               MOVE CURSORPOS TO DZRIBM2-FUNCC (IDX1)
               MOVE ST0-3 TO DZRIBM2-FUNCA (IDX1)
               IF IDX5 = 0
                   MOVE 5 TO IDX5
                   GO TO B-054-EXIT
               ELSE
                   GO TO B-054-EXIT.
           IF DZRIBM2-FUNC (IDX1) = 'C'
               MOVE '-' TO HDRS-COM1
               MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1
               MOVE DZRIBM2-ELE-KEY (IDX1) TO RTZ5-FLDVALUE1
               MOVE ' ' TO RTZ5-COM1
               MOVE DZRIBM2-ELE-DATA (IDX1) TO DMELS-ELE-DATA
               CALL 'CBLTDLI' USING REPL
                                    DZRO-PCB
                                    DMELS
               IF DZRO-STATUS = BLNKS
                   MOVE '1' TO DATE-SW
               ELSE
                   IF DZRO-STATUS NOT = GE
                       MOVE 3801 TO 9000-ABEND-CODE
                       PERFORM C-099-ABEND THRU C-099-EXIT.
           IF DZRIBM2-FUNC (IDX1) = 'D'
               MOVE '-' TO HDRS-COM1
               MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1
               MOVE DZRIBM2-ELE-KEY (IDX1) TO RTZ5-FLDVALUE1
               PERFORM B-070-GHU-RTZ5 THRU B-070-EXIT
               MOVE ' ' TO RTZ5-COM1
               CALL 'CBLTDLI' USING DLET
                                    DZRO-PCB
                                    DMELS
               IF DZRO-STATUS = BLNKS
                   MOVE '1' TO DATE-SW
               ELSE
                   MOVE 3751 TO 9000-ABEND-CODE
                   PERFORM C-099-ABEND THRU C-099-EXIT.
       B-054-EXIT.
           EXIT.
       B-055-REPL-ACT-DATE.
           IF DATE-SW = '0'
               GO TO B-055-EXIT.
           MOVE CURR-YMD9 TO DMHDRS-LAST-ACT-DATE.
       B-055-EXIT.
           EXIT.
       B-056-CHECK-KEY-CNT.
           MOVE '0' TO LENGTH-ERROR.
           IF DMHDRS-KEY-CHAR-CNT = 0
               GO TO B-056-EXIT.
           MOVE DZRIBM2-ELE-KEY (IDX1) TO GROUP-KEY-AREA.
           MOVE DMHDRS-KEY-CHAR-CNT TO IDX2.
           COMPUTE IDX2 = IDX2 + 1.
           PERFORM B-057-VALIDATE-KEY-LENGTH THRU B-057-EXIT
            VARYING IDX4 FROM IDX2 BY 1 UNTIL IDX4 > 20.
       B-056-EXIT.
           EXIT.
       B-057-VALIDATE-KEY-LENGTH.
           IF KEY-MATRIX (IDX4) > SPACE
               MOVE '1' TO LENGTH-ERROR.
       B-057-EXIT.
           EXIT.
       B-058-CHECK-CHR-CNT.
           MOVE '0' TO LENGTH-ERROR.
           MOVE DZRIBM2-ELE-DATA (IDX1) TO GROUP-KEY-AREA.
           MOVE DMHDRS-ELE-CHAR-CNT TO IDX2.
           COMPUTE IDX2 = IDX2 + 1.
           PERFORM B-059-VALIDATE-ELE-LENGTH THRU B-059-EXIT
               VARYING IDX4 FROM IDX2 BY 1 UNTIL IDX4 > 50.
       B-058-EXIT.
           EXIT.
       B-059-VALIDATE-ELE-LENGTH.
           IF KEY-MATRIX (IDX4) > SPACE
               MOVE '1' TO LENGTH-ERROR.
       B-059-EXIT.
           EXIT.
       B-060-DEL.
           IF DZRIBM2-CONT < 'A' AND
              DZRIBM2-RETURN < 'A' AND
              DZRIBM2-ABORT < 'A'
               MOVE 'X' TO DZRIBM2-CONT.
           IF DZRIBM2-ABORT NOT = SPACE AND NOT = '?'
               MOVE '1' TO FIRST-TIME
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO B-060-EXIT.
           IF DZRIBM2-RETURN NOT = 'D'
               MOVE CURSORPOS TO DZRIBM2-RETURNC
               MOVE 8 TO IDX5
               PERFORM A-022-FIRST-TIME THRU A-022-EXIT
               GO TO B-060-EXIT.
           MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1.
           PERFORM A-045-GHU-HDRS THRU A-045-EXIT.
           MOVE ' ' TO HDRS-COM1.
           CALL 'CBLTDLI' USING DLET
                                DZRO-PCB
                                DMHDRS.
           IF DZRO-STATUS NOT = BLNKS AND NOT = GE
               MOVE 3901 TO 9000-ABEND-CODE
               PERFORM C-099-ABEND THRU C-099-EXIT.
           MOVE 'X' TO DZRIBM2-RETURN.
           IF DZRIBM2-RETURN NOT = SPACE OR DZRIBM2-ABORT NOT = SPACE
               MOVE '1' TO FIRST-TIME
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO B-060-EXIT.
       B-060-EXIT.
           EXIT.
       B-070-GHU-RTZ5.
           MOVE '-' TO RTZ5-COM1.
           CALL 'CBLTDLI' USING GHU
                                DZRO-PCB
                                DMELS
                                SSA-PRT
                                SSA-PRT2.
           IF DZRO-STATUS NOT = BLNKS AND NOT = GE
               MOVE 3702 TO 9000-ABEND-CODE
               PERFORM C-099-ABEND THRU C-099-EXIT.
       B-070-EXIT.
           EXIT.
       B-080-DISPLAY.
           IF DZRIBM2-CONT < 'A' AND
              DZRIBM2-RETURN < 'A' AND
              DZRIBM2-ABORT < 'A'
               MOVE 'X' TO DZRIBM2-CONT.
           IF DZRIBM2-ABORT NOT = SPACE AND NOT = '?'
               MOVE '1' TO FIRST-TIME
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO B-080-EXIT.
           MOVE PMDAN TO DZRIBM2-DESCA.
           MOVE PMDAN TO DZRIBM2-ELE-KEY-CNTA.
           MOVE PMDAN TO DZRIBM2-ELE-CHRA.
           MOVE PMDAN TO DZRIBM2-TBL-PROTA.
           MOVE PMDAN TO DZRIBM2-ACT-DATEA.
           MOVE UMDAD TO DZRIBM2-TBL-PSWA.
           MOVE PMDAN TO DZRIBM2-ELE-NUM-KEYA.
           MOVE PMDAN TO DZRIBM2-ELE-NUM-VALA.
           IF DZRIBM2-RETURN NOT = SPACE OR DZRIBM2-ABORT NOT = SPACE
               MOVE '1' TO FIRST-TIME
               PERFORM A-020-FIRST-TIME THRU A-020-EXIT
               GO TO B-080-EXIT.
           IF DZRIBM2-CONT NOT = SPACE
               IF DZRIBM2-NEXT-KEY NOT = SPACES
                   MOVE DZRIBM2-NEXT-KEY TO DZRIBM2-LAST-KEY.
           IF DZRIBM2-CONT NOT = SPACE
               MOVE PMDAN TO DZRIBM2-TABLE-IDA
               MOVE PMDAN TO DZRIBM2-DESCA
               MOVE PMDAN TO DZRIBM2-ELE-KEY-CNTA
               MOVE PMDAN TO DZRIBM2-ELE-CHRA
               MOVE PMDAN TO DZRIBM2-ACT-DATEA
               MOVE PMDAN TO DZRIBM2-TBL-PROTA
               MOVE PMDDD TO DZRIBM2-TBL-PSWA
               MOVE PMDAN TO DZRIBM2-ELE-NUM-KEYA
               MOVE PMDAN TO DZRIBM2-ELE-NUM-VALA
               MOVE LOW-VALUES TO DZRIBM2-MATRIX
               PERFORM C-025-PROT-INQ THRU C-025-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               MOVE '1' TO FIRST-TIME
               MOVE DZRIBM2-TABLE-ID TO HDRS-FLDVALUE1
               PERFORM A-045-GHU-HDRS THRU A-045-EXIT
               MOVE DZRIBM2-NEXT-KEY TO RTZ5-FLDVALUE1
               MOVE '=>' TO RTZ5-RLO1
               MOVE '1' TO FIRST-TIME
               MOVE '-' TO RTZ5-COM1
               PERFORM A-075-GNP-RTZ5 THRU A-075-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14 OR
               DZRO-STATUS NOT = BLNKS
               PERFORM A-075-GNP-RTZ5 THRU A-075-EXIT
               MOVE CURSORPOS TO DZRIBM2-RETURNC
               IF DZRO-STATUS = GE OR GB
                   MOVE SPACES TO DZRIBM2-NEXT-KEY
                   PERFORM A-022-FIRST-TIME THRU A-022-EXIT
               ELSE
                   MOVE DMELS-KEY TO DZRIBM2-NEXT-KEY
                   PERFORM A-022-FIRST-TIME THRU A-022-EXIT
           ELSE
               MOVE LOW-VALUES TO DZRIBM2-MATRIX
               MOVE SPACES TO DZRIBM2-LAST-KEY
               MOVE SPACES TO DZRIBM2-NEXT-KEY
               PERFORM C-023-PROT-RTZ5 THRU C-023-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               PERFORM C-025-PROT-INQ THRU C-025-EXIT
                VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14
               MOVE '1' TO FIRST-TIME
               MOVE LOW-VALUES TO DZRIBM2-FUNCC (1)
               MOVE CURSORPOS TO DZRIBM2-RETURNC
               PERFORM A-022-FIRST-TIME THRU A-022-EXIT.
       B-080-EXIT.
           EXIT.
       C-010-INITIALIZE.
           MOVE '1' TO FIRST-TIME.
           MOVE '1' TO FIRST-INSERT.
           MOVE '1' TO DATA-PRESENT.
           MOVE '0' TO DATE-SW.
           MOVE SPACES TO DZRIBM1-SCREEN.
           MOVE SPACES TO DZRIBM2-SCREEN.
           MOVE LOW-VALUES TO QUEUE-AREA.
           MOVE '0' TO DOF-SWITCH.
           MOVE '0' TO REPL-SW.
           MOVE '0' TO CHANGE-ADD-SW.
           MOVE '0' TO EOP-SW.
           MOVE '0' TO LENGTH-ERROR.
           MOVE '0' TO SCREEN2-AGAIN.
           MOVE '1' TO SCREEN2-FULL.
           MOVE '0' TO EOP-SW.
           MOVE ' ' TO DZRIBM2-STATUS.
           ACCEPT CURR-YMD FROM DATE.
           MOVE ZERO TO IDX5.
           MOVE ZERO TO IDX3.
           MOVE ZERO TO IDX1.
           MOVE ZERO TO IDX2.
           MOVE ZERO TO IDX4.
       C-010-EXIT.
           EXIT.
       C-020-GET-MESSAGE.
           CALL 'CBLTDLI' USING
               GU
               PCB-LT
               RDZ01TIO.
           IF STATUS-PCB NOT = BLNKS AND NOT = QC AND NOT = QD
               MOVE 3514 TO 9000-ABEND-CODE
               PERFORM C-099-ABEND.
           MOVE LOW-VALUES TO DZRIBM1-SCREEN.
           MOVE LOW-VALUES TO DZRIBM2-SCREEN.
           MOVE TIOAIO TO DZRIBM1-SCREEN.
           MOVE TIOAIO TO DZRIBM2-SCREEN.
           MOVE 'MFDZRO' TO DZRIBM1-TRANSCODE
           PERFORM C-021-FMT-EMPTY-SCREEN THRU C-021-EXIT.
           PERFORM C-022-FMT-EMPTY-SCREEN THRU C-022-EXIT.
           MOVE CRT-LENGTH TO LL01-IN.
       C-020-EXIT.
           EXIT.
       C-021-FMT-EMPTY-SCREEN.
           MOVE 'MFDZRO' TO DZRIBM1-TRANSCODE
           MOVE LOW-VALUES TO DZRIBM1-FUNCTIONC
                              DZRIBM1-PASSWORDC
                              DZRIBM1-TABLE-IDC
                              DZRIBM1-RTZ5C.
           MOVE UMDAN TO DZRIBM1-FUNCTIONA
                         DZRIBM1-TABLE-IDA
                         DZRIBM1-RTZ5A.
           MOVE UMDAD TO DZRIBM1-PASSWORDA.
       C-021-EXIT.
           EXIT.
       C-022-FMT-EMPTY-SCREEN.
           MOVE PMDDD TO DZRIBM2-LAST-KEYA.
           MOVE UMDAN TO DZRIBM2-NEXT-KEYA.
           MOVE LOW-VALUES TO
               DZRIBM2-TYPEC
               DZRIBM2-TABLE-IDC
               DZRIBM2-DESCC
               DZRIBM2-LAST-KEYC
               DZRIBM2-ELE-KEY-CNTC
               DZRIBM2-ELE-CHRC
               DZRIBM2-ACT-DATEC
               DZRIBM2-TBL-PROTC
               DZRIBM2-TBL-PSWC
               DZRIBM2-ELE-NUM-KEYC
               DZRIBM2-ELE-NUM-VALC
               DZRIBM2-RETURNC
               DZRIBM2-CONTC
               DZRIBM2-NEXT-KEYC
               DZRIBM2-ABORTC.
           MOVE UMDAN TO
               DZRIBM2-DESCA
               DZRIBM2-ELE-KEY-CNTA
               DZRIBM2-ELE-CHRA
               DZRIBM2-ACT-DATEA
               DZRIBM2-TBL-PROTA
               DZRIBM2-TBL-PSWA
               DZRIBM2-ELE-NUM-KEYA
               DZRIBM2-ELE-NUM-VALA
               DZRIBM2-RETURNA
               DZRIBM2-CONTA
               DZRIBM2-ABORTA.
           MOVE PMDAN TO
                         DZRIBM2-TYPEA
                         DZRIBM2-ACT-DATEA
                         DZRIBM2-TABLE-IDA
                         DZRIBM2-ELE-NUM-KEYA
                         DZRIBM2-ELE-NUM-VALA.
           MOVE PMDDD TO DZRIBM2-LAST-KEYA.
           MOVE UMDAD TO DZRIBM2-TBL-PSWA.
           PERFORM C-023-PROT-RTZ5 THRU C-023-EXIT
            VARYING IDX1 FROM 1 BY 1 UNTIL IDX1 > 14.
           MOVE CRT-LENGTH TO LL01-IN.
       C-022-EXIT.
           EXIT.
       C-023-PROT-RTZ5.
           MOVE LOW-VALUES TO
               DZRIBM2-FUNCC (IDX1)
               DZRIBM2-ELE-KEYC (IDX1)
               DZRIBM2-ELE-DATAC (IDX1)
               S-M2-FUNCC (IDX1)
               S-M2-ELE-KEYC (IDX1)
               S-M2-ELE-DATAC (IDX1).
           MOVE PMDAN TO
               DZRIBM2-ELE-KEYA (IDX1)
               S-M2-ELE-KEYA (IDX1).
           MOVE UMDAN TO DZRIBM2-ELE-DATAA (IDX1).
           MOVE UMDAN TO S-M2-ELE-DATAA (IDX1).
           MOVE UMDAN TO DZRIBM2-FUNCA (IDX1).
           MOVE UMDAN TO S-M2-FUNCA (IDX1).
       C-023-EXIT.
           EXIT.
       C-024-UNPROT-RTZ5.
           MOVE UMDAN TO
               DZRIBM2-FUNCA (IDX2)
               DZRIBM2-ELE-KEYA (IDX2)
               DZRIBM2-ELE-DATAA (IDX2)
               S-M2-FUNCA (IDX2)
               S-M2-ELE-KEYA (IDX2)
               S-M2-ELE-DATAA (IDX2).
           MOVE '-' TO DZRIBM2-FUNC (IDX2)
                       S-M2-FUNC (IDX2).
       C-024-EXIT.
           EXIT.
       C-025-PROT-INQ.
           MOVE PMDAN TO
               DZRIBM2-FUNCA (IDX1)
               DZRIBM2-ELE-KEYA (IDX1)
               DZRIBM2-ELE-DATAA (IDX1)
               S-M2-FUNCA (IDX1)
               S-M2-ELE-KEYA (IDX1)
               S-M2-ELE-DATAA (IDX1).
           MOVE SPACES TO DZRIBM2-FUNC (IDX1).
       C-025-EXIT.
           EXIT.
       C-026-PROT-ISRT.
           MOVE PMDAN TO
               DZRIBM2-FUNCA (IDX1)
               S-M2-FUNCA (IDX1).
       C-026-EXIT.
           EXIT.
       C-027-UNPROT-RTZ5.
           MOVE UMDAN TO
               DZRIBM2-ELE-KEYA (IDX2)
               DZRIBM2-ELE-DATAA (IDX2)
               S-M2-ELE-KEYA (IDX2)
               S-M2-ELE-DATAA (IDX2).
           MOVE 'A' TO DZRIBM2-FUNC (IDX2)
                       S-M2-FUNC (IDX2).
           MOVE PMDAN TO
               S-M2-FUNCA (IDX2)
               DZRIBM2-FUNCA (IDX2).
       C-027-EXIT.
           EXIT.
       C-099-ABEND.
           DISPLAY 'PROGRAM ABEND ' 9000-ABEND-CODE.
           DISPLAY PCB-LT.
           DISPLAY DZRO-PCB.
           DISPLAY 'HIT ENTER TO CONTINUE'.
           ACCEPT DUMVAL.
           GOBACK.
       C-099-EXIT.
           EXIT.
       C-100-RETURN.
           MOVE GENERAL-END-SCREEN TO TIOAIO.
           CALL 'CBLTDLI' USING
            ISRT
            PCB-LT
            RDZ01TIO
            RTIBM
            IF STATUS-PCB = BLNKS
                 NEXT SENTENCE
            ELSE
                 MOVE 3555 TO 9000-ABEND-CODE
                 PERFORM C-099-ABEND.
           GOBACK.
       C-100-EXIT.
           EXIT.
       PROGRAM-TERMINATOR.
