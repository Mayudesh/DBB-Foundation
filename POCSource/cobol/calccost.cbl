       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID.  CALCCOST.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.
      *   (C)

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PATERR
           ASSIGN TO UT-S-PATERR
             ACCESS MODE IS SEQUENTIAL
             FILE STATUS IS OFCODE.

           SELECT PATINS
                  ASSIGN       to PATINS
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is DYNAMIC
                  RECORD KEY   is PATIENT-INS-KEY
                  FILE STATUS  is PATINS-STATUS.

           SELECT PRSNMSTR
                  ASSIGN       to PRSNMSTR
                  ORGANIZATION is INDEXED
                  ACCESS MODE  is DYNAMIC
                  RECORD KEY   is PRSN-KEY
                  FILE STATUS  is PRSN-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  PATERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 1133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INPATIENT-DAILY-REC-ERR.
       01  INPATIENT-DAILY-REC-ERR.
           05  ERR-MSG-PAT                  PIC X(40).
           05  REST-OF-PAT-REC              PIC X(993).

       FD  PATINS
           DATA RECORD IS PATINS-REC.
       01  PATINS-REC.
           05 PATIENT-INS-KEY      PIC X(06).
           05 FILLER               PIC X(696).

       FD  PRSNMSTR
           DATA RECORD IS PRSNMSTR-REC.
       01  PRSNMSTR-REC.
           05 PRSN-KEY      PIC X(06).
           05 FILLER           PIC X(794).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  PATMSTR-STATUS          PIC X(2).
               88 RECORD-FOUND    VALUE "00".
           05  PRSN-STATUS          PIC X(2).
               88 PRSN-FOUND    VALUE "00".
           05  PATINS-STATUS          PIC X(2).
               88 PATINS-FOUND    VALUE "00".
               88 PATINS-OPEN     VALUE "00".
           05  OFCODE                  PIC X(2).
              88 CODE-WRITE    VALUE SPACES.

       01  MISC-FIELDS.
           05 TEMP-COST                   PIC S9(9)V99 COMP-3.
           05 PARA-NAME                   PIC X(40).
           05 TEMP-AMOUNT-TOTAL           PIC S9(9)V99 COMP-3.
           05 PLAN-FOUND-SW               PIC X(1) VALUE "N".
              88 PLAN-FOUND VALUE "Y".
           05 PROVIDER-FOUND-SW           PIC X(1) VALUE "N".
              88 PROVIDER-FOUND VALUE "Y".
           05 ERROR-FOUND-SW              PIC X(1) VALUE " ".
              88 ERROR-FOUND   VALUE "Y".
           05 CALC-CALL-RET-CODE          PIC S9(4) VALUE 0.
           05 ABEND-REASON                PIC X(50).
           05 WS-ANCILLARY-CHARGES        PIC S9(9)V99 COMP-3.
           05 WS-ANCILLARY-CHARGES        PIC S9(9)V99 COMP-3.
           05 WS-LAB-CHARGES              PIC S9(9)V99 COMP-3.
           05 WS-EQUIP-CHARGES            PIC S9(9)V99 COMP-3.
           05 ROW-SUB                     PIC 9(3) VALUE 0.
           05 PRIMARY-PHYS-NETWORK        PIC X(1) VALUE "N".
              88 PRIMARY-PHYS-IN-NETWORK VALUE "Y".
           05 LAB-PHYS-SW                 PIC X(1) VALUE "N".
              88 LAB-IN-NETWORK VALUE "Y".
           05 PHYS-FOUND-SW               PIC X(1) VALUE "N".
              88 PHYSICIAN-FOUND VALUE "Y".
           05 EQUIP-PHYS-SW               PIC X(1) VALUE "N".
              88 EQUIP-IN-NETWORK VALUE "Y".


       COPY HLTHPLAN.

       01  CALC-COSTS-REC.
           05  CALC-TYPE-SW               PIC X.
               88 LAB-TEST VALUE "L".
               88 EQUIPMENT VALUE "E".
           05  PATIENT-ID                 PIC X(8)  VALUE SPACES.
           05  LAB-TEST-ID                PIC X(8)  VALUE SPACES.
           05  PATIENT-DEDUCTIBLE-REM     PIC 9(4) COMP  VALUE 0.
           05  PATIENT-COPAY              PIC 9(3) COMP-3   VALUE 0.
           05  REIMBURSE-PCT              PIC 9(3) COMP-3   VALUE 0.
           05  PROCEDURE-BASE-COST    PIC 9(7)V99 COMP-3   VALUE 0.
           05  ANCILLARY-COSTS        PIC 9(5)V99 COMP-3   VALUE 0.
           05  VENIPUNCTURE-COSTS     PIC 9(5)V99 COMP-3  VALUE 0.
           05  NET-PATIENT-COSTS      PIC 9(7)V99 COMP-3 VALUE 0.
           05  VENIPUNCTURE-COSTS     PIC 9(7)V99 COMP-3   VALUE 0.
           05  PHYS-ID-TEMP           PIC X(8) VALUE SPACES.
           05  STATE-FACTOR           PIC 999 VALUE 0.

       COPY PATINS.
       COPY PROVIDER.
       COPY PATPERSN.
       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       COPY PATMSTR.

       01  RETURN-CD                      PIC 9(4) COMP.

       PROCEDURE DIVISION USING PATIENT-MASTER-REC, RETURN-CD.
      *
      * For each patient
      *    call clclbcst for each lab and equipment cost
      *    for the primary care physician
      *        read provider to get in/out of network
      *        factor into the total amount
      *
      *

           PERFORM 000-SETUP-RTN THRU 000-EXIT.

           IF NOT PATINS-OPEN
                 MOVE -2 TO RETURN-CD
                 GO TO 1000-ERROR-RTN.

           PERFORM 100-GET-PLAN-DATA THRU 100-EXIT.

           IF NOT PLAN-FOUND
               MOVE -1 TO RETURN-CD
               GO TO 1000-ERROR-RTN.

           PERFORM 200-GET-PRIMARY-PROVIDER THRU 200-EXIT.

           IF PROVIDER-FOUND
               IF NETWORK-FLAG = "Y"
                   MOVE "Y" TO PRIMARY-PHYS-NETWORK
                ELSE
                   MOVE "N" TO PRIMARY-PHYS-NETWORK
           ELSE
               MOVE -1 TO RETURN-CD
               GO TO 1000-ERROR-RTN.

           IF PROVIDER-FOUND
               PERFORM 300-CALCULATE-TREATMENT-COSTS THRU 300-EXIT
               PERFORM 400-CALCULATE-EQUIPMENT-COSTS THRU 400-EXIT
               PERFORM 500-COMPUTE-TOTAL-AMOUNT THRU 500-EXIT
           END-IF

           MOVE ZERO TO RETURN-CD.
           GOBACK.

       000-SETUP-RTN.
           DISPLAY '000-SETUP-RTN'
           MOVE "000-SETUP-RTN" TO PARA-NAME.
           INITIALIZE CALC-COSTS-REC, MISC-FIELDS.
           PERFORM 800-OPEN-FILES THRU 800-EXIT.
       000-EXIT.
           EXIT.

       100-GET-PLAN-DATA.
           DISPLAY '100-GET-PLAN-DATA'
      *** CALL DB2 HEALTH_PLAN TABLE.  GET RECORD
           MOVE "100-GET-PLAN-DATA" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE PATIENT-ID IN PATIENT-MASTER-REC TO
                PATIENT-INS-KEY, PRSN-KEY.

           READ PATINS INTO PATIENT-INSURANCE.

           IF NOT PATINS-FOUND
              MOVE "** PATIENT NOT ON PATINS" TO ERR-MSG-PAT
              MOVE PATIENT-MASTER-REC  TO
                   REST-OF-PAT-REC
              GO TO 1000-ERROR-RTN.

           MOVE INS-COMPANY-PRIMARY-ID TO  PLAN-ID IN DCLHEALTH-PLAN.

           READ PRSNMSTR  INTO PATIENT-INSURANCE.

           IF NOT PRSN-FOUND IN PRSN-STATUS
              MOVE "** PATIENT NOT ON PRSNMSTR" TO ERR-MSG-PAT
              MOVE PATIENT-MASTER-REC  TO
                   REST-OF-PAT-REC
              GO TO 1000-ERROR-RTN.


      ****** CHECK FOR VALID DIAGNOSTIC CODE
           EXEC SQL
           SELECT
            PLAN_ID,
             GROUP_ID,
             PROVIDER,
             DEDUCTIBLE,
             COPAYMENT,
             CO_INSURANCE,
             COVERAGE_LIMITS,
             OOP_MAX       ,
             IN_NETWORK_REQ  ,
             PRIOR_AUTHORIZATION    ,
             EXCLUSIONS     ,
             PLAN_COMMENTS
           INTO
           :PLAN-ID               ,
           :GROUP-ID              ,
           :PROVIDER              ,
           :DEDUCTIBLE            ,
           :COPAYMENT             ,
           :CO-INSURANCE          ,
           :COVERAGE-LIMITS       ,
           :OOP-MAX               ,
           :IN-NETWORK-REQ        ,
           :PRIOR-AUTHORIZATION   ,
           :EXCLUSIONS ,
           :PLAN-COMMENTS
              FROM DDS0001.HEALTH_PLAN
              WHERE PLAN_ID = :PLAN-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100 OR SQLCODE < 0
               MOVE "*** DIAGNOSTIC CODE NOT-FOUND IN DIAG_CODES" TO
               ERR-MSG-PAT IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 100-EXIT.
       100-EXIT.
           EXIT.

       200-GET-PRIMARY-PROVIDER.
           DISPLAY '200-GET-PRIMARY-PROVIDER'
      *** CALL DB2 HEALTH_PLAN TABLE.  GET RECORD
           MOVE "200-GET-PROVIDER-DATA" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE PRIMARY-CARE-PHYSICIAN-ID IN PATIENT-MASTER-REC TO
                PROVIDER-ID IN DCLPROVIDER.
      ****** CHECK PROVIDER IN/OUT OF NETWORK
           EXEC SQL
           SELECT
             PROVIDER_ID,
             NETWORK_FLAG,
             COST_OVERRIDE_PCT
           INTO
             :PROVIDER-ID,
             :NETWORK-FLAG,
             :COST-OVERRIDE-PCT
              FROM DDS0001.PROVIDER
              WHERE PROVIDER_ID = :PROVIDER-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100 OR SQLCODE < 0
               MOVE "** PRIMARY PHYSICIAN NOT-FOUND IN PROVIDER" TO
               ERR-MSG-PAT IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 200-EXIT.
       200-EXIT.
           EXIT.

       220-GET-LAB-PROVIDER.
           DISPLAY '220-GET-LAB-PROVIDER'
      *** CALL DB2 HEALTH_PLAN TABLE.  GET RECORD
           MOVE "200-GET-PROVIDER-DATA" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE PHYS-ID-TEMP  TO
                PROVIDER-ID IN DCLPROVIDER.
      ****** CHECK PROVIDER IN/OUT OF NETWORK
           EXEC SQL
           SELECT
             PROVIDER_ID,
             NETWORK_FLAG,
             COST_OVERRIDE_PCT
           INTO
             :PROVIDER-ID,
             :NETWORK-FLAG,
             :COST-OVERRIDE-PCT
              FROM DDS0001.PROVIDER
              WHERE PROVIDER_ID = :PROVIDER-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               MOVE "Y" TO PHYS-FOUND-SW
           ELSE
           IF SQLCODE = +100 OR SQLCODE < 0
               MOVE "*** LAB PHYSICIAN NOT-FOUND IN PROVIDER" TO
               ERR-MSG-PAT IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 220-EXIT.
       220-EXIT.
           EXIT.

       240-GET-EQUIP-PROVIDER.
           DISPLAY '240-GET-EQUIP-PROVIDER'
      *** CALL DB2 HEALTH_PLAN TABLE.  GET RECORD
           MOVE "200-GET-PROVIDER-DATA" TO PARA-NAME.
      ******** EXEC SQL to get info from DB2
           MOVE PRIMARY-CARE-PHYSICIAN-ID IN PATIENT-MASTER-REC TO
                PROVIDER-ID IN DCLPROVIDER.
      ****** CHECK PROVIDER IN/OUT OF NETWORK

           EXEC SQL
           SELECT
             PROVIDER_ID,
             NETWORK_FLAG,
             COST_OVERRIDE_PCT
           INTO
             :PROVIDER-ID,
             :NETWORK-FLAG,
             :COST-OVERRIDE-PCT
              FROM DDS0001.PROVIDER
              WHERE PROVIDER_ID = :PROVIDER-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100 OR SQLCODE < 0
               MOVE "*** EQUIP PHYSICIAN NOT-FOUND IN PROVIDER" TO
               ERR-MSG-PAT IN INPATIENT-DAILY-REC-ERR
               MOVE "Y" TO ERROR-FOUND-SW
               GO TO 240-EXIT .
       240-EXIT.
           EXIT.

       300-CALCULATE-TREATMENT-COSTS.
           DISPLAY '300-CALCULATE-TREATMENT-COSTS'
      *** ROLL UP ALL LAB COSTS IN THE TABLE
           MOVE "300-CALCULATE-TREATMENT-COSTS" TO PARA-NAME.
           PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL
               ROW-SUB > 20 OR LAB-TEST-ID = " "
               MOVE "N" TO PHYS-FOUND-SW
               MOVE PRESCRIBING-S-PHYS-ID(ROW-SUB) TO PHYS-ID-TEMP
               PERFORM 220-GET-LAB-PROVIDER THRU 220-EXIT
               IF PHYSICIAN-FOUND
                   IF NETWORK-FLAG = "Y"
                      MOVE 80 TO REIMBURSE-PCT
                      COMPUTE WS-EQUIP-CHARGES  =
                       WS-EQUIP-CHARGES  +
                          ( EQUIPMENT-CHARGES(ROW-SUB) * REIMBURSE-PCT )
                   ELSE
                      COMPUTE REIMBURSE-PCT = 80 - COST-OVERRIDE-PCT
                      COMPUTE WS-LAB-CHARGES =
                      WS-EQUIP-CHARGES  +
                          ( EQUIPMENT-CHARGES(ROW-SUB) * REIMBURSE-PCT )
                   END-IF
                END-IF
           END-PERFORM.
       300-EXIT.
           EXIT.

       400-CALCULATE-EQUIPMENT-COSTS.
           DISPLAY '400-CALCULATE-EQUIPMENT-COSTS'
      *** ROLL UP ALL EQUIPMENT COSTS
           MOVE "400-CALCULATE-EQUIPMENT-COSTS" TO PARA-NAME.


           PERFORM VARYING ROW-SUB FROM 1 BY 1 UNTIL
               ROW-SUB > 20 OR LAB-TEST-ID = " "
               MOVE "N" TO PHYS-FOUND-SW
               MOVE EQUIPMENT-PRES-PHYS-ID(ROW-SUB) TO PHYS-ID-TEMP
               PERFORM 220-GET-LAB-PROVIDER THRU 220-EXIT
               IF PHYSICIAN-FOUND
                   IF NETWORK-FLAG = "Y"
                      MOVE 80 TO REIMBURSE-PCT
                      COMPUTE WS-LAB-CHARGES =
                       WS-LAB-CHARGES +
                          ( TEST-CHARGES(ROW-SUB) * REIMBURSE-PCT )
                   ELSE
                      COMPUTE REIMBURSE-PCT = 80 - COST-OVERRIDE-PCT
                      COMPUTE WS-LAB-CHARGES =
                      WS-LAB-CHARGES +
                          ( TEST-CHARGES(ROW-SUB) * REIMBURSE-PCT )
                   END-IF
                END-IF
           END-PERFORM.
       400-EXIT.
           EXIT.

       500-COMPUTE-TOTAL-AMOUNT.
           DISPLAY '500-COMPUTE-TOTAL-AMOUNT'
      *** FINAL TOTALS PROCESSING
           MOVE ZERO TO PATIENT-TOT-AMT, STATE-FACTOR.

           IF PRIMARY-PHYS-IN-NETWORK
              PERFORM 600-COMPUTE-IN-NETWORK
           ELSE
              PERFORM 700-COMPUTE-OUT-OF-NETWORK.



       500-EXIT.
           EXIT.

       600-COMPUTE-IN-NETWORK.
           DISPLAY '600-COMPUTE-IN-NETWORK'
      *** STANDARD RATES - REIMBURSE% BY STATE VALUE

           MOVE 80 TO REIMBURSE-PCT IN CALC-COSTS-REC.

           EVALUATE EMP-STATE
               WHEN "NC" MOVE 100 TO STATE-FACTOR
               WHEN "NJ" MOVE 100 TO STATE-FACTOR
               WHEN "NY" MOVE 100 TO STATE-FACTOR
               WHEN "ND" MOVE  60 TO STATE-FACTOR
               WHEN "AZ" MOVE 100 TO STATE-FACTOR
               WHEN "AR" MOVE  75 TO STATE-FACTOR
               WHEN "ID" MOVE 100 TO STATE-FACTOR
               WHEN "DE" MOVE  80 TO STATE-FACTOR
               WHEN "WA" MOVE 100 TO STATE-FACTOR
               WHEN "TX" MOVE 100 TO STATE-FACTOR
               WHEN "PA" MOVE  90 TO STATE-FACTOR
               WHEN "HI" MOVE 100 TO STATE-FACTOR
               WHEN "CA" MOVE  99 TO STATE-FACTOR
               WHEN "OR" MOVE  80 TO STATE-FACTOR
           END-EVALUATE

           COMPUTE PATIENT-TOT-AMT =
              ( WS-LAB-CHARGES + WS-EQUIP-CHARGES )
                               * ( REIMBURSE-PCT * STATE-FACTOR )

           MOVE STATE-FACTOR  TO COPAY IN PATIENT-MASTER-REC.

       600-EXIT.
           EXIT.

       700-COMPUTE-OUT-OF-NETWORK.
           DISPLAY '700-COMPUTE-OUT-OF-NETWORK'
      *** OUT OF NETWORK RATES FOR PATIENTS
           MOVE 72 TO REIMBURSE-PCT IN CALC-COSTS-REC.
           MOVE ZERO TO STATE-FACTOR.

           EVALUATE EMP-STATE
               WHEN "NC" MOVE  82 TO STATE-FACTOR
               WHEN "NJ" MOVE  54 TO STATE-FACTOR
               WHEN "NY" MOVE  19 TO STATE-FACTOR
               WHEN "ND" MOVE  79 TO STATE-FACTOR
               WHEN "AZ" MOVE  40 TO STATE-FACTOR
               WHEN "AR" MOVE  68 TO STATE-FACTOR
               WHEN "ID" MOVE  17 TO STATE-FACTOR
               WHEN "DE" MOVE  90 TO STATE-FACTOR
               WHEN "WA" MOVE  85 TO STATE-FACTOR
               WHEN "TX" MOVE  58 TO STATE-FACTOR
               WHEN "PA" MOVE  58 TO STATE-FACTOR
               WHEN "HI" MOVE  92 TO STATE-FACTOR
               WHEN "OR" MOVE  60 TO STATE-FACTOR
           END-EVALUATE

           COMPUTE PATIENT-TOT-AMT =
              ( WS-LAB-CHARGES + WS-EQUIP-CHARGES )
                               * ( REIMBURSE-PCT * STATE-FACTOR )

           MOVE STATE-FACTOR  TO COPAY IN PATIENT-MASTER-REC.

       700-EXIT.
           EXIT.

       800-OPEN-FILES.
           DISPLAY '800-OPEN-FILES'
           MOVE "800-OPEN-FILES" TO PARA-NAME.
           OPEN INPUT PATINS, PRSNMSTR.
           OPEN OUTPUT PATERR.
           DISPLAY "OPEN FILES".
           DISPLAY PATINS-STATUS.
           DISPLAY PRSN-STATUS.
      *     GOBACK.
       800-EXIT.
           EXIT.

       900-CLOSE-FILES.
           DISPLAY '900-CLOSE-FILES'
           MOVE "900-CLOSE-FILES" TO PARA-NAME.
           CLOSE PATINS, PRSNMSTR, PATERR.
           DISPLAY "FILES CLOSED".
      *     GOBACK.
       900-EXIT.
           EXIT.


       1000-DB2-ERROR-RTN.
           DISPLAY '1000-DB2-ERROR-RTN'
      ************************************************************
      *       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *
      ************************************************************

            DISPLAY '**** WE HAVE A SERIOUS PROBLEM HERE *****'.
            DISPLAY '999-ERROR-TRAP-RTN '.
            MULTIPLY SQLCODE BY -1 GIVING SQLCODE.
            DISPLAY 'SQLCODE ==> ' SQLCODE.
            DISPLAY SQLCA.
            DISPLAY SQLERRM.
            EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
            EXEC SQL ROLLBACK WORK END-EXEC.
            GO TO 1000-ERROR-RTN.

       1000-ERROR-RTN.
           DISPLAY '1000-ERROR-RTN'
           GOBACK.

