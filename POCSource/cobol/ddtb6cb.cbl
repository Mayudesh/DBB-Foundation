       IDENTIFICATION DIVISION.
      *------------------------
       PROGRAM-ID. DDBPT6CB.
      ***************************************************************
      *   MODULE NAME = DDBPT6CB
      *
      *   DESCRIPTIVE NAME = D-DB SAMPLE APPLICATION
      *                      REORDER POINT PROCESSING
      *                      i5/OS
      *                      COBOL
      *
      *   FUNCTION =  THIS MODULE PROCESSES THE PART_STOCK TABLE AND
      *               FOR EACH PART BELOW THE ROP (REORDER POINT)
      *               CHECKS THE EXISTING ORDERS AND SHIPMENTS,
      *               CREATES A SUPPLY ORDER AND PRINTS A REPORT.
      *
      *      DEPENDENCIES = NONE
      *
      *      INPUT = PARAMETERS EXPLICITLY PASSED TO THIS FUNCTION:
      *
      *              LOCAL-DB       LOCAL DB NAME
      *              REMOTE-DB      REMOTE DB NAME
      *
      *   TABLES =  PART-STOCK       - LOCAL
      *             PART_ORDER       - REMOTE
      *             PART_ORDLN       - REMOTE
      *             SHIPMENTLN       - REMOTE
      *
      *   CRTSQLCBL  SPECIAL PARAMETERS
      *    PGM(DDBPT6CB) RDB(remotedbname) OPTION(*APOST *APOSTSQL)
      *
      *   INVOKE BY : CALL DDBPT6CB PARM(localdbname remotedbname)
      *
      ***************************************************************
        ENVIRONMENT DIVISION.
      *---------------------
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT RELAT  ASSIGN TO PRINTER-QPRINT.
        DATA DIVISION.
      *--------------
        FILE SECTION.
      *-------------
        FD  RELAT
            RECORD CONTAINS 33 CHARACTERS
            LABEL RECORDS ARE OMITTED
            DATA RECORD IS REPREC.
        01  REPREC          PIC  X(33).
        WORKING-STORAGE SECTION.
      *------------------------
      *    PRINT LINE  DEFINITIONS
        01  LINE0           PIC  X(33) VALUE  SPACES.
        01  LINE1           PIC  X(33) VALUE  SPACES.


       01  LINE2.
          05  FILLER        PIC  X(18) VALUE  '   ORDER NUMBER = '.
          05  MASK0         PIC  ZZZ9.
          05  FILLER        PIC  X(11) VALUE  SPACES.
        01  LINE3           PIC  X(33) VALUE
            '---------------------------------'.
        01  LINE4           PIC  X(33) VALUE
            '   LINE     PART         QTY     '.
        01  LINE5           PIC  X(33) VALUE
            '  NUMBER   NUMBER     REQUESTED  '.
        01  LINE6.
          05  FILLER        PIC  XXXX  VALUE SPACES.
          05  MASK1         PIC  ZZZ9.
          05  FILLER        PIC  XXXX  VALUE SPACES.
          05  PART-TABLE    PIC  XXXXX.
          05  FILLER        PIC  XXXX  VALUE SPACES.
          05  MASK2         PIC  Z,ZZZ,ZZZ.ZZ.
        01  LINE7.
          05  FILLER        PIC  X(26) VALUE
            'NUMBER OF LINES CREATED = '.
          05  MASK3         PIC  ZZZ9.
          05  FILLER        PIC  XXX   VALUE  SPACES.
        01  LINE8           PIC  X(33) VALUE
            '********* END OF PROGRAM ********'.
      *    MISCELLANEOUS DEFINITIONS
        01  WHAT-TIME       PIC  X     VALUE '1'.
            88  FIRST-TIME             VALUE '1'.
        01  CONTL           PIC  S9999 COMP-4 VALUE ZEROS.
        01  CONTD           PIC  S9999 COMP-4 VALUE ZEROS.
        01  RTCODE1         PIC  S9999 COMP-4 VALUE ZEROS.
        01  RTCODE2         PIC  S9999 COMP-4.
        01  NEXT-NUM        PIC  S9999 COMP-4.
        01  IND-NULL        PIC  S9999 COMP-4.
        01  LOC-TABLE       PIC  X(16).
        01  ORD-TABLE       PIC  S9999 COMP-4.
        01  ORL-TABLE       PIC  S9999 COMP-4.
        01  QUANT-TABLE     PIC  S9(9) COMP-4.
        01  QTY-TABLE       PIC  S9(9) COMP-4.
        01  ROP-TABLE       PIC  S9(9) COMP-4.
        01  EOQ-TABLE       PIC  S9(9) COMP-4.
        01  QTY-REQ         PIC  S9(9) COMP-4.
        01  QTY-REC         PIC  S9(9) COMP-4.
      * CONSTANT  FOR LOCATION NUMBER
        01  XPARM.
            05   LOC        PIC  X(4)  VALUE 'SQLA'.
      * DEFINITIONS FOR ERROR MESSAGE HANDLING
       01  ERROR-MESSAGE.
           05   MSG-ID.
           10   MSG-ID-1     PIC  X(2)
                VALUE 'SQ'.
           10   MSG-ID-2     PIC 99999.
           EXEC SQL INCLUDE SQLCA    END-EXEC.

       LINKAGE SECTION.
      *----------------
       01  LOCAL-DB        PIC  X(18).
       01  REMOTE-DB       PIC  X(18).

       PROCEDURE DIVISION USING LOCAL-DB REMOTE-DB.
      *------------------
      *****************************
      *    SQL CURSOR DECLARATION *
      *****************************
      * RE-POSITIONABLE CURSOR : POSITION AFTER LAST PART_NUM
           EXEC SQL DECLARE NEXT_PART CURSOR FOR
                SELECT PART_NUM,
                       PART_QUANT,
                       PART_ROP,
                       PART_EOQ
                FROM   PART_STOCK
                WHERE  PART_ROP > PART_QUANT
                  AND  PART_NUM > :PART-TABLE
                ORDER BY PART_NUM ASC
           END-EXEC.
      * CURSOR FOR ORDER LINES
           EXEC SQL DECLARE NEXT_ORDER_LINE CURSOR FOR
                SELECT A.ORDER_NUM,
                       ORDER_LINE,
                       QUANT_REQ
                FROM   PART_ORDLN A,
                       PART_ORDER B
                WHERE  PART_NUM  = :PART-TABLE
                AND    LINE_STAT  <> 'C'
                AND    A.ORDER_NUM = B.ORDER_NUM
                AND    ORDER_TYPE  = 'R'
           END-EXEC.
      ******************************
      *    SQL RETURN CODE HANDLING*
      ******************************
           EXEC SQL WHENEVER SQLERROR GO TO DB-ERROR END-EXEC.
           EXEC SQL WHENEVER SQLWARNING CONTINUE  END-EXEC.

       MAIN-PROGRAM-PROC.
      *------------------
           PERFORM START-UP THRU START-UP-EXIT.
           PERFORM MAIN-PROC THRU MAIN-EXIT UNTIL RTCODE1 = 100.
       END-OF-PROGRAM.

           EXEC SQL CONNECT RESET END-EXEC.
      ****
           CLOSE RELAT.
           GOBACK.
       MAIN-PROGRAM-EXIT. EXIT.
      *------------------

       START-UP.
      *----------
           OPEN OUTPUT RELAT.
      ****
           EXEC SQL COMMIT END-EXEC.
      ****
           PERFORM CLEAN-UP THRU CLEAN-UP-EXIT.
      ********************************
      *    CONNECT TO LOCAL DATABASE *
      ********************************
      ****
      *    EXEC SQL CONNECT TO 'DB2DBASE' END-EXEC.
      ****
       START-UP-EXIT. EXIT.
      *------------
           EJECT
       MAIN-PROC.
      *---------
           EXEC SQL OPEN NEXT_PART END-EXEC.
           EXEC SQL
                FETCH NEXT_PART
                INTO  :PART-TABLE,
                      :QUANT-TABLE,
                      :ROP-TABLE,
                      :EOQ-TABLE
           END-EXEC.
           IF SQLCODE = 100
              MOVE 100 TO RTCODE1
              PERFORM TRAILER-PROC THRU TRAILER-EXIT
           ELSE
              MOVE 0 TO RTCODE2
              MOVE 0 TO QTY-REQ
              MOVE 0 TO QTY-REC
      * --- IMPLICIT "CLOSE" CAUSED BY COMMIT ---
      ****
              EXEC SQL COMMIT END-EXEC
      ****
      *********************************
      *    CONNECT TO REMOTE DATABASE *
      *********************************


      *       EXEC SQL CONNECT TO 'DB2DBASE' END-EXEC
      ****
              EXEC SQL OPEN NEXT_ORDER_LINE END-EXEC
              PERFORM UNTIL RTCODE2 = 100
                 EXEC SQL
                      FETCH NEXT_ORDER_LINE
                      INTO  :ORD-TABLE,
                            :ORL-TABLE,
                            :QTY-TABLE
                 END-EXEC
                 IF SQLCODE = 100
                    MOVE 100 TO RTCODE2
                    EXEC SQL CLOSE NEXT_ORDER_LINE END-EXEC
                 ELSE
                    ADD QTY-TABLE TO QTY-REQ
                    EXEC SQL
                         SELECT SUM(QUANT_RECV)
                         INTO   :QTY-TABLE:IND-NULL
                         FROM   SHIPMENTLN
                         WHERE  ORDER_LOC  = :LOC
                         AND    ORDER_NUM  = :ORD-TABLE
                         AND    ORDER_LINE = :ORL-TABLE
                    END-EXEC
                    IF IND-NULL NOT < 0
                       ADD QTY-TABLE TO QTY-REC
                    END-IF
                 END-IF
              END-PERFORM
              IF ROP-TABLE > QUANT-TABLE + QTY-REQ - QTY-REC
                 PERFORM ORDER-PROC THRU ORDER-EXIT
              END-IF
           END-IF.
      ****
           EXEC SQL COMMIT END-EXEC.
      ****
      **********************************
      *    RECONNECT TO LOCAL DATABASE *
      **********************************
      ****
      *    EXEC SQL CONNECT TO 'DB2DBASE' END-EXEC.
      ****
       MAIN-EXIT. EXIT.
      *---------------
       ORDER-PROC.
      *----------
           IF FIRST-TIME
              MOVE '2' TO WHAT-TIME
              PERFORM CREATE-ORDER-PROC THRU CREATE-ORDER-EXIT.
           ADD 1 TO CONTL.


           EXEC SQL
                INSERT
                INTO    PART_ORDLN
                       (ORDER_NUM,
                        ORDER_LINE,
                        PART_NUM,
                        QUANT_REQ,
                        LINE_STAT)
                VALUES (:NEXT-NUM,
                        :CONTL,
                        :PART-TABLE,
                        :EOQ-TABLE,
                        'O')
           END-EXEC.
           PERFORM DETAIL-PROC THRU DETAIL-EXIT.
       ORDER-EXIT. EXIT.
      *----------------

       CREATE-ORDER-PROC.
      *------------------
      *GET NEXT ORDER NUMBER
           EXEC SQL
                SELECT (MAX(ORDER_NUM) + 1)
                INTO   :NEXT-NUM:IND-NULL
                FROM   PART_ORDER
           END-EXEC.
           IF IND-NULL < 0
             MOVE 1 TO NEXT-NUM.
           EXEC SQL
                INSERT
                INTO    PART_ORDER
                       (ORDER_NUM,
                        ORIGIN_LOC,
                        ORDER_TYPE,
                        ORDER_STAT,
                        CREAT_TIME)
                VALUES (:NEXT-NUM,
                        :LOC, 'R', 'O',
                        CURRENT TIMESTAMP)
              END-EXEC.
           MOVE NEXT-NUM TO MASK0.
           PERFORM HEADER-PROC THRU HEADER-EXIT.
       CREATE-ORDER-EXIT. EXIT.
      *------------------

       DB-ERROR.
      *--------
           PERFORM ERROR-MSG-PROC THRU ERROR-MSG-EXIT.
      ***********************
      *    ROLLBACK THE LUW *

      ***********************
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
      ****
           EXEC SQL ROLLBACK WORK END-EXEC.
      ****
           PERFORM END-OF-PROGRAM THRU MAIN-PROGRAM-EXIT.
      * -- NEXT LINE INCLUDED TO RESET THE "GO TO" DEFAULT --
           EXEC SQL WHENEVER SQLERROR GO TO DB-ERROR END-EXEC.

       ERROR-MSG-PROC.
      *----------
           MOVE  SQLCODE   TO  MSG-ID-2.
           DISPLAY 'SQL STATE =' SQLSTATE ' SQLCODE =' MSG-ID-2.
      * -- ADD HERE ANY ADDITIONAL ERROR MESSAGE HANDLING --
       ERROR-MSG-EXIT. EXIT.
      *----------------

      *******************
      * REPORT PRINTING *
      *******************
       HEADER-PROC.
      *-----------
           WRITE REPREC FROM LINE1 AFTER ADVANCING PAGE.
           WRITE REPREC FROM LINE2 AFTER ADVANCING 3 LINES.
           WRITE REPREC FROM LINE3 AFTER ADVANCING 2 LINES.
           WRITE REPREC FROM LINE4 AFTER ADVANCING 1 LINES.
           WRITE REPREC FROM LINE5 AFTER ADVANCING 1 LINES.
           WRITE REPREC FROM LINE3 AFTER ADVANCING 1 LINES.
           WRITE REPREC FROM LINE0 AFTER ADVANCING 1 LINES.
       HEADER-EXIT. EXIT.
      *-----------------
       DETAIL-PROC.
      *-----------
           ADD 1 TO CONTD.
           IF CONTD > 50
              MOVE 1 TO CONTD
              PERFORM HEADER-PROC THRU HEADER-EXIT
           END-IF
           MOVE CONTL     TO MASK1.
           MOVE EOQ-TABLE TO MASK2.
           WRITE REPREC FROM LINE6 AFTER ADVANCING 1 LINES.
       DETAIL-EXIT. EXIT.
      *-----------------
       TRAILER-PROC.
      *------------
           MOVE CONTL TO MASK3.
           WRITE REPREC FROM LINE3 AFTER ADVANCING 2 LINES.
           WRITE REPREC FROM LINE7 AFTER ADVANCING 2 LINES.
           WRITE REPREC FROM LINE3 AFTER ADVANCING 2 LINES.
           WRITE REPREC FROM LINE8 AFTER ADVANCING 1 LINES.
       TRAILER-EXIT. EXIT.
      *------------------

      ********************************************************
      * THIS PARAGRAPH IS ONLY REQUIRED IN A TEST ENVIRONMENT*
      * TO RESET THE DATA TO PERMIT RE-RUNNING OF THE TEST   *
      ********************************************************
       CLEAN-UP.
      *---------
      *********************************
      *    CONNECT TO REMOTE DATABASE *
      *********************************
      ****
           EXEC SQL CONNECT TO 'DB2DBASE' END-EXEC.
      ****
      *---------------------DELETE ORDER ROWS FOR RERUNABILITY
              EXEC SQL
                   DELETE
                   FROM    PART_ORDLN
                   WHERE   ORDER_NUM IN
                              (SELECT  ORDER_NUM
                               FROM    PART_ORDER
                               WHERE   ORDER_TYPE = 'R')
              END-EXEC.
              EXEC SQL
                   DELETE
                   FROM    PART_ORDER
                   WHERE   ORDER_TYPE = 'R'
              END-EXEC.
      ****
           EXEC SQL COMMIT END-EXEC.
      ****
       CLEAN-UP-EXIT. EXIT.
      *-------------
