       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. ADSORT.                                              00020000
       ENVIRONMENT DIVISION.                                            00030000
       DATA DIVISION.                                                   00040000
       WORKING-STORAGE SECTION.                                         00050000
                                                                        00060000
       01  WORK-VARIABLES.                                              00070000
           05  INSERT-NUM     PIC S9(9) COMP SYNC.                      00080000
                                                                        00090000
       01  ARRAY-SUBSCRIPTS.                                            00100000
           05  MOVE-FROM      PIC S9(18) COMP SYNC.                     00110000
           05  INSERT-TO      PIC S9(8) COMP SYNC.                      00120000
                                                                        00130000
       LINKAGE SECTION.                                                 00140000
       01  ARRAY-SIZE         PIC S9(8) COMP.                           00150000
       01  ARRAY-OF-NUMBERS.                                            00160000
           05  NUM            PIC S9(8) COMP                            00170000
                     OCCURS 0 TO 1000 TIMES DEPENDING ON ARRAY-SIZE.    00180000
                                                                        00190000
       PROCEDURE DIVISION USING ARRAY-SIZE, ARRAY-OF-NUMBERS.           00200000
                                                                        00210000
                                                                        00220000
           PERFORM VARYING MOVE-FROM FROM 3 BY 2                        00230000
             UNTIL ( MOVE-FROM > ARRAY-SIZE )                           00240000
               MOVE NUM(MOVE-FROM) TO INSERT-NUM                        00260000
      *          ***WORK BACKWARDS THROUGH ARRAY, FIND WHERE TO INSERT  00270000
               COMPUTE INSERT-TO = MOVE-FROM - 1                        00280000
               PERFORM  UNTIL (   (NUM(INSERT-TO) <= INSERT-NUM)        00290000
      *          ***WORK BACKWARDS THROUGH ARRAY, FIND WHERE TO INSERT  00270000
                               OR (INSERT-TO <= 0) )                    00300000
                   MOVE NUM(INSERT-TO) TO NUM(INSERT-TO + 1)            00310000
                   COMPUTE INSERT-TO = INSERT-TO - 1                    00320000
               END-PERFORM                                              00330000
      *          ***INSERT THIS ITEM INTO CORRECT SLOT                  00340000
               MOVE INSERT-NUM     TO NUM(INSERT-TO + 1)                00350000
           END-PERFORM.                                                 00360000
           GOBACK.                                                      00370000
