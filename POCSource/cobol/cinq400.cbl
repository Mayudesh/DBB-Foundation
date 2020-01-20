       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CMEN400.
       AUTHOR.        .
       DATE-COMPILED. .

      ***************************************************************
      * MAIN MENU PROGRAM FOR SYSTEMS GROUP PC CICS CLASS           *
      ***************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  WS-CICS-WORK-VARIABLES.
           03  WS-RESP                     PIC S9(08)  COMP VALUE ZEROS.
           03  WS-LENGTH                   PIC S9(08)  COMP VALUE ZEROS.
           03  WS-COMMAREA                 PIC X(01)  VALUE SPACE.
           03  WS-LOGOFF-MESSAGE           PIC X(35)  VALUE
               '*** APPLICATION COMPLETED. BYE-BYE.'.
           03  WS-LOGOFF-MESSAGE-LEN       PIC S9(08)  COMP VALUE +35.
       01  WS-ERR-LINE.
           03  FILLER                      PIC X(23) VALUE
               '*** ABEND *** TRAN ID: '.
           03  WS-ERR-TRAN-ID              PIC X(04) VALUE SPACES.
           03  FILLER                      PIC X(10) VALUE
               ' PROGRAM: '.
           03  WS-ERR-PROG-ID              PIC X(08) VALUE SPACES.
           03  FILLER                      PIC X(16) VALUE
               ' LAST FUNCTION: '.
           03  WS-ERR-FUNCTION             PIC 9(04) VALUE ZEROS.
           03  FILLER                      PIC X(09) VALUE
               ' RESULT: '.
           03  WS-ERR-RESULT               PIC 9(04) VALUE ZEROS.
           03  FILLER                      PIC X(02) VALUE
               '  '.
       01  WS-ERR-LINE2                    PIC X(80) VALUE SPACES.

       01  WS-WORKING-VARIABLES.
           03  WS-SELECTION                PIC X(01) VALUE SPACES.
               88  WS-SELECTION-VALID      VALUE 'A', 'B', 'C', 'X'.
           03  WS-TIME                     PIC S9(08) COMP VALUE +0.

      *--- COPYLIB CONTAINING PFKEY DEFINITIONS
       COPY DFHAID.
      *--- COPY BMS COPYLIBS HERE.
       COPY BMEN400.

       LINKAGE SECTION.
       01  DFHCOMMAREA              PIC X(01).

       PROCEDURE DIVISION.

       0000-INITIAL-LOOP.

      *--- IF CLEAR KEY IS PRESSED, SEND LOGOFF MESSAGE AND RETURN TO
      *---     CICS.
           IF (EIBAID = DFHCLEAR)

      *==> ISSUE A SEND TEXT VERB HERE TO SEND THE MESSAGE IN THE
      *==>  VARIABLE WS-LOGOFF-MESSAGE WITH A LENGTH OF
      *==>  WS-LOGOFF-MESSAGE-LEN FOLLOWED BY A RETURN TO CICS
      *==>                     OR
      *==>  COPY IN CMENCPY1.SRC HERE NOW

                    END-IF.

      *--- BASIC PSUDOCONVERSATIONAL LOOP
      *---    CHECK COMMAREA LENGTH TO SEE IF THIS IS FIRST TIME IN
      *---    IF THIS IS FIRST TIME IN DO SEND LOOP
      *---    OTHERWISE, DO RECEIVE LOOP
           IF (EIBCALEN > 0)
              MOVE DFHCOMMAREA          TO WS-COMMAREA
              PERFORM 0100-RECEIVE-LOOP THRU 0100-EXIT
           ELSE
              PERFORM 0200-SEND-LOOP    THRU 0200-EXIT
           END-IF.

      *--- RETURN TO CICS WITH TRANSACTION ID AND COMMAREA
           EXEC CICS RETURN
                     TRANSID('M400')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(1)
           END-EXEC.

       0000-EXIT.
            EXIT.

       0100-RECEIVE-LOOP.

      *==> CODE THE CICS COMMAND NECESSARY TO RECEIVE THE MENU MAP
      *==>  FROM THE TERMINAL. NOTE: CODE A RESP OPTION AND MAKE
      *==>  THE FIELD FOR THE RESP EQUAL TO WS-RESP
      *==>                  OR
      *==>  COPY THE CMENCPY2.SRC HERE

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'ERROR: NO DATA WAS ENTERED OR UPDATED'
                    TO MSGO
               PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
               GO TO 0100-EXIT
           END-IF.

      *--- APPLICATION PROCESSING GOES HERE

           IF (SELECTL > 0)
              MOVE SELECTI   TO WS-SELECTION
              IF WS-SELECTION-VALID
                 NEXT SENTENCE
              ELSE
                 MOVE LOW-VALUES                TO BMEN400O
                 MOVE WS-SELECTION              TO SELECTO
                 MOVE 'INVALID SELECTION, TRY AGAIN.'
                      TO MSGO
                 PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
                 GO TO 0100-EXIT
           ELSE
              MOVE LOW-VALUES                TO BMEN400O
              MOVE 'YOU MUST MAKE A SELECTION HERE'
                   TO MSGO
              PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
              GO TO 0100-EXIT
           END-IF.

           IF (WS-SELECTION = 'X')
              EXEC CICS SEND TEXT
                        FROM(WS-LOGOFF-MESSAGE)
                        LENGTH(WS-LOGOFF-MESSAGE-LEN)
                        ERASE
              END-EXEC
              EXEC CICS RETURN
              END-EXEC
           ELSE
           IF (WS-SELECTION = 'A')
              EXEC CICS XCTL
                        PROGRAM('CADD400')
                        RESP(WS-RESP)
              END-EXEC
              IF WS-RESP NOT = DFHRESP(NORMAL)
                 MOVE 'ERROR: XCTL TO CADD400 FAILED'
                      TO MSGO
                 PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
                 GO TO 0100-EXIT
              END-IF
           ELSE
           IF (WS-SELECTION = 'B')
              EXEC CICS XCTL
                        PROGRAM('CINQ400')
                        RESP(WS-RESP)
              END-EXEC
              IF WS-RESP NOT = DFHRESP(NORMAL)
                 MOVE 'ERROR: XCTL TO CINQ400 FAILED'
                      TO MSGO
                 PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
                 GO TO 0100-EXIT
              END-IF
           ELSE
           IF (WS-SELECTION = 'C')
              EXEC CICS XCTL
                        PROGRAM('CDLT400')
                        RESP(WS-RESP)
              END-EXEC
              IF WS-RESP NOT = DFHRESP(NORMAL)
                 MOVE 'ERROR: XCTL TO CDLT400 FAILED'
                      TO MSGO
                 PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
                 GO TO 0100-EXIT
              END-IF
           END-IF.

           MOVE LOW-VALUES                TO BMEN400O.
           MOVE '***  TASK COMPLETED. NEXT OPTION.'
                   TO MSGO.
           PERFORM 0875-SEND-APPL-SCREEN      THRU 0875-EXIT.

       0100-EXIT.
            EXIT.

       0200-SEND-LOOP.
           EXEC CICS ASKTIME
                     ABSTIME(WS-TIME)
           END-EXEC.

      *==> CODE THE COMMAND NECESSARY TO FORMAT THE ABSOLUTE TIME
      *==>  RETRIEVED FROM THE ASKTIME CICS COMMAND ABOVE INTO
      *==>  THE DATE AND TIME (DATE IN MM/DD/YY FORMAT AND TIME IN
      *==>  HH:MM:SS FORMAT) AND MAKE SURE THE RESULT IS IN THE
      *==>  OUTPUT SCREEN FIELDS FOR DATE AND TIME ON THE MENU MAP
      *==>                OR
      *==>  COPY THE CODE FOR THIS PROCEDURE IN FROM THE CMENCPY3.SRC
      *==>  FILE HERE.

      *==> NOTE: YOU WILL HAVE TO COPY THIS CODE TO TWO OTHER LOCATIONS
      *==>  FURTHER DOWN IN THE SOURCE CODE.

           MOVE -1   TO SELECTL.
           EXEC CICS SEND
                     MAP('BMEN400')
                     MAPSET('BMEN400')
                     FROM(BMEN400O)
                     ERASE
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
              NEXT SENTENCE
           ELSE
           IF WS-RESP = DFHRESP(MAPFAIL)
              MOVE SPACES       TO WS-ERR-LINE2
              MOVE ' 0200: SEND MAP; CONDITION MAPFAIL' TO
                   WS-ERR-LINE2
              GO TO 9999-ABEND-ROUTINE
           END-IF.

       0200-EXIT.
            EXIT.

       0850-SEND-ERROR-SCREEN.

           EXEC CICS ASKTIME
                     ABSTIME(WS-TIME)
           END-EXEC.

      *==>  COPY THE CODE FOR THE TIME PROCEDURE YOU WROTE ABOVE TO
      *==>  THIS SPOT IN THE CODE AS WELL.

           MOVE -1   TO SELECTL.
           EXEC CICS SEND
                     MAP('BMEN400')
                     MAPSET('BMEN400')
                     FROM(BMEN400O)
                     FREEKB
                     ALARM
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
              NEXT SENTENCE
           ELSE
           IF WS-RESP = DFHRESP(MAPFAIL)
              MOVE SPACES       TO WS-ERR-LINE2
              MOVE ' 0850: SEND MAP; CONDITION MAPFAIL' TO
                   WS-ERR-LINE2
              GO TO 9999-ABEND-ROUTINE
           END-IF.

       0850-EXIT.
            EXIT.

       0875-SEND-APPL-SCREEN.

           EXEC CICS ASKTIME
                     ABSTIME(WS-TIME)
           END-EXEC.

      *==>  COPY THE CODE FOR THE TIME PROCEDURE YOU WROTE ABOVE TO
      *==>  THIS SPOT IN THE CODE AS WELL.

           MOVE -1   TO SELECTL.
           EXEC CICS SEND
                     MAP('BMEN400')
                     MAPSET('BMEN400')
                     FROM(BMEN400O)
                     FREEKB
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
              NEXT SENTENCE
           ELSE
           IF WS-RESP = DFHRESP(MAPFAIL)
              MOVE SPACES       TO WS-ERR-LINE2
              MOVE ' 0875: SEND MAP; CONDITION MAPFAIL' TO
                   WS-ERR-LINE2
              GO TO 9999-ABEND-ROUTINE
           END-IF.

       0875-EXIT.
            EXIT.

       9999-ABEND-ROUTINE.
           MOVE EIBTRNID       TO WS-ERR-TRAN-ID.
           MOVE 'MENU'         TO WS-ERR-PROG-ID.
           MOVE EIBFN          TO WS-ERR-FUNCTION.
           EXEC CICS SEND TEXT
                     FROM(WS-ERR-LINE)
                     LENGTH(80)
                     ERASE
           END-EXEC.
           EXEC CICS SEND TEXT
                     FROM(WS-ERR-LINE2)
                     LENGTH(80)
           END-EXEC.
           EXEC CICS RETURN
           END-EXEC.
