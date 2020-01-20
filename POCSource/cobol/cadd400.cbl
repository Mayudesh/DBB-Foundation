       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CADD400.
       AUTHOR.        Jon Sayles.
       DATE-COMPILED. .

      ***************************************************************
      * RECORD ADD PROGRAM FOR SYSTEMS GROUP PC CICS CLASS          *
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
               88  WS-SELECTION-VALID      VALUE 'A', 'D', 'U', 'X'.
           03  WS-TIME                     PIC S9(08) COMP VALUE +0.

      *--- COPYLIB CONTAINING PFKEY DEFINITIONS
       COPY DFHAID.
      *--- COPY BMS COPYLIBS HERE.
       COPY BADD400.
      *--- COPY DATASET COPYLIB HERE.
       COPY PERSON.

       LINKAGE SECTION.
       01  DFHCOMMAREA              PIC X(01).

       PROCEDURE DIVISION.

       0000-INITIAL-LOOP.

              EXEC CICS HANDLE CONDITION
                        ERROR(9999-ABEND-ROUTINE)
              END-EXEC.
      *--- IF CLEAR KEY IS PRESSED, SEND LOGOFF MESSAGE AND RETURN TO
      *---     CICS.
           IF (EIBAID = DFHCLEAR)
              EXEC CICS SEND TEXT
                        FROM(WS-LOGOFF-MESSAGE)
                        LENGTH(WS-LOGOFF-MESSAGE-LEN)
                        ERASE
              END-EXEC
              EXEC CICS RETURN
              END-EXEC
           END-IF.

      *--- IF PF3 KEY IS PRESSED, TRANSFER CONTROL BACK TO MAIN MENU
      *---     PROGRAM.
           IF (EIBAID = DFHPF3)
              EXEC CICS XCTL
                        PROGRAM('CMEN400')
                        RESP(WS-RESP)
              END-EXEC
              IF WS-RESP NOT = DFHRESP(NORMAL)
                 MOVE 'ERROR: XCTL TO MENU400 FAILED'
                      TO MSGO
                 PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
                 GO TO 0000-EXIT
              END-IF
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
                     TRANSID('A400')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(1)
           END-EXEC.

       0000-EXIT.
            EXIT.

       0100-RECEIVE-LOOP.

           EXEC CICS RECEIVE MAP('BADD400')
                     MAPSET('BADD400')
                     INTO(BADD400I)
                     RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'ERROR: NO DATA WAS ENTERED OR UPDATED'
                    TO MSGO
               PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
               GO TO 0100-EXIT
           END-IF.

      *--- APPLICATION PROCESSING GOES HERE

           IF (PERSONNL > 0)
              NEXT SENTENCE
           ELSE
              MOVE LOW-VALUES                TO MSGO
              MOVE 'INVALID PERSON NUMBER. PLEASE TRY AGAIN.'
                      TO MSGO
              PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
              GO TO 0100-EXIT
           END-IF.

           INITIALIZE PERSON-MASTER-RECORD.
           MOVE PERSONNI            TO PERSON-NUMBER.
           MOVE FNAMEI              TO PERSON-FIRST-NAME.
           MOVE LNAMEI              TO PERSON-LAST-NAME.
           MOVE STREETI             TO PERSON-STREET-ADDRESS.
           MOVE CITYI               TO PERSON-CITY-ADDRESS.
           MOVE STATEI              TO PERSON-STATE-ADDRESS.
           MOVE SALARYI             TO PERSON-SALARY.

      *==> CODE THE CICS COMMAND REQUIRED TO DO THE RECORD WRITE. THE
      *==>  FILE NAME IS PERSONAL, THE RECORD NAME IS
      *==>  PERSON-MASTER-RECORD WITH A LENGTH OF 80. THE KEY IS IN
      *==>  POSITIONS 1 - 10 AND MAKE SURE YOU CODE A RESP WITH THE
      *==>  TARGET FIELD BEING WS-RESP
      *==>               OR
      *==>  COPY THE CADDCPY1.SRC FILE IN HERE NOW.

           IF WS-RESP = DFHRESP(NORMAL)
              NEXT SENTENCE
           ELSE
           IF WS-RESP = DFHRESP(DUPREC) OR
              WS-RESP = DFHRESP(DUPKEY)
              MOVE LOW-VALUES                TO MSGO
              MOVE 'RECORD ALREADY EXISTS. PLEASE TRY AGAIN.'
                      TO MSGO
              PERFORM 0850-SEND-ERROR-SCREEN THRU 0850-EXIT
              GO TO 0100-EXIT
           ELSE
              MOVE SPACES       TO WS-ERR-LINE2
              MOVE ' 0000: DATASET WRITE; NOT NORMAL' TO
                   WS-ERR-LINE2
              GO TO 9999-ABEND-ROUTINE
           END-IF.

           MOVE LOW-VALUES                TO BADD400O.
           MOVE '***  RECORD WRITTEN. '
                   TO MSGO.
           PERFORM 0875-SEND-APPL-SCREEN      THRU 0875-EXIT.

       0100-EXIT.
            EXIT.

       0200-SEND-LOOP.

           EXEC CICS ASKTIME
                     ABSTIME(WS-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                     ABSTIME(WS-TIME)
                     MMDDYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC.
           MOVE -1   TO PERSONNL.
           EXEC CICS SEND
                     MAP('BADD400')
                     MAPSET('BADD400')
                     FROM(BADD400O)
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
           EXEC CICS FORMATTIME
                     ABSTIME(WS-TIME)
                     MMDDYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC.
           MOVE -1   TO PERSONNL.
           EXEC CICS SEND
                     MAP('BADD400')
                     MAPSET('BADD400')
                     FROM(BADD400O)
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
           EXEC CICS FORMATTIME
                     ABSTIME(WS-TIME)
                     MMDDYY(DATEO)
                     DATESEP('/')
                     TIME(TIMEO)
                     TIMESEP(':')
           END-EXEC.
           MOVE -1   TO PERSONNL.
           EXEC CICS SEND
                     MAP('BADD400')
                     MAPSET('BADD400')
                     FROM(BADD400O)
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
           MOVE 'ADD '         TO WS-ERR-PROG-ID.
           MOVE EIBFN          TO WS-ERR-FUNCTION.
           MOVE EIBRESP        TO WS-ERR-RESULT.
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
