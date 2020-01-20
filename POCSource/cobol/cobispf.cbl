 CBL NOLIB,APOST,NODECK,OBJECT,NOSEQ,DYNAM
 CBL NOMAP,NOLIST,NOOFFSET,NOXREF
       Identification Division.
         Program-ID. CB2TSOEV.
         Author. Gilbert Saint-Flour.
      ******************************************************************
      *                                                                *
      *   MODULE NAME = COB2TSO                                        *
      *                                                                *
      *   DESCRIPTIVE NAME = Issue TSO commands from a COBOL program.  *
      *                                                                *
      *   FUNCTION = This sample program demonstrates how to invoke    *
      *              TSO commands from a COBOL program using           *
      *              standard TSO services as documented in the        *
      *              TSO/E Programming Services manual.                *
      *                                                                *
      *              Most TSO commands, including CLISTs and REXX      *
      *              execs can be executed using this technique.       *
      *              TSO commands which require authorization          *
      *              (such as OUTPUT, SEND, TRANSMIT and RECEIVE)      *
      *              will not work.                                    *
      *                                                                *
      *   Origin = http://gsf-soft.com/Freeware/                       *
      *                                                                *
      ******************************************************************
       Data Division.
        Working-Storage Section.
         01 Filler.
           05 ws-dummy        Pic s9(8) Comp.
           05 ws-return-code  Pic s9(8) Comp.
           05 ws-reason-code  Pic s9(8) Comp.
           05 ws-info-code    Pic s9(8) Comp.
           05 ws-cppl-address Pic s9(8) Comp.
           05 ws-flags        Pic X(4) Value X'00010001'.
           05 ws-buffer       Pic X(256).
           05 ws-length       Pic s9(8) Comp Value 256.

       Procedure Division.
      *----------------------------------------------------------------*
      *          Call IKJTSOEV to create the TSO/E environment         *
      *----------------------------------------------------------------*
           CALL 'IKJTSOEV' Using ws-dummy
                                 ws-return-code
                                  ws-reason-code
                                 ws-info-code
                                 ws-cppl-address.
           IF ws-return-code > zero
             DISPLAY 'IKJTSOEV Failed, Return-code=' ws-return-code
                                     ' Reason-code=' ws-reason-code
                                     ' Info-code='   ws-info-code
             MOVE ws-return-code to Return-code
             STOP RUN.
      *----------------------------------------------------------------*
      *          Build the TSO/E command in ws-buffer                  *
      *----------------------------------------------------------------*

           MOVE 'ALLOCATE DD(SYSPUNCH) SYSOUT HOLD' to ws-buffer.

      *----------------------------------------------------------------*
      *   Call the TSO/E Service Routine to execute the TSO/E command  *
      *----------------------------------------------------------------*
           CALL 'IKJEFTSR' Using ws-flags
                                 ws-buffer
                                 ws-length
                                 ws-return-code
                                 ws-reason-code
                                 ws-dummy.
           IF ws-return-code > zero
             DISPLAY 'IKJEFTSR Failed, Return-code=' ws-return-code
                                     ' Reason-code=' ws-reason-code
             MOVE ws-return-code to Return-code
             STOP RUN.

      *----------------------------------------------------------------*
      *          Check that the ALLOCATE command worked                *
      *----------------------------------------------------------------*
           DISPLAY 'ALLOCATE Worked ! ' Upon Syspunch.

           STOP RUN.
