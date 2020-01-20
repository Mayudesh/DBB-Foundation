       IDENTIFICATION DIVISION.
       PROGRAM-ID. "EXPREVAL".
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *simple variables
       01  str              PIC X(12) DISPLAY.
       01  int              PIC 99999 DISPLAY.
       01  float            PIC 99V99 DISPLAY.
      *record variables
       01  employee.
          02  employeeName.
             03  firstName  PIC X(10).
             03  lastName   PIC X(10).
          02  dateOfBirth.
             03  dayOfBirth        PIC 99.
             03  monthOfBirth      PIC 99.
             03  yearOfBirth       PIC 99.
      *array variable
       01  tableOfProd.
          02  prods OCCURS 5 TIMES.
             03  prodId     PIC 9(5).
             03  prodName   PIC X(12).
       PROCEDURE DIVISION.

           Move "Hello EXPESSION EVAL!" to str.
           Display str.
           Display ZERO.
           Display 7.
           Display SPACE.
           Display 55.55.

           COMPUTE int = 5 + 7.
           Display int.

           Move 55555 to int.
           Display int.

           Move 55.55 to float.
           Display float.

           Move "John" to firstName.
           Move "Smith" to lastName.
           Display firstName.
           Display lastName.
           Display employeeName.

           Move 12 to dayOfBirth.
           Move 12 to monthOfBirth.
           Move 1212 to yearOfBirth.
           Display dayOfBirth.
           Display monthOfBirth.
           Display yearOfBirth.
           Display dateOfBirth.

           Display employee.

           CALL "inittbl" USING tableOfProd.
           CALL "display" USING tableOfProd.

           DISPLAY "End of EXPREVAL".

           GOBACK.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. "inittbl".
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  counter          PIC 9(5) value zeros.
       LINKAGE SECTION.
       01  tableOfProd.
          02  prods OCCURS 5 TIMES.
             03  prodId     PIC 9(5).
             03  prodName   PIC X(12).
       PROCEDURE DIVISION USING tableOfProd.
           Move 0 to counter.
           Perform
             varying counter from 1 by 1
             until counter > 5
               Move counter to prodID(counter)
               Move "singleName" to prodName(counter)
           End-perform.
           GOBACK.
       END PROGRAM "inittbl".

       IDENTIFICATION DIVISION.
       PROGRAM-ID. "display".
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  counter          PIC 9(5) value zeros.
       LINKAGE SECTION.
       01  tableOfProd.
          02  prods OCCURS 5 TIMES.
             03  prodId     PIC 9(5).
             03  prodName   PIC X(12).
       PROCEDURE DIVISION USING tableOfProd.
           Move 0 to counter.
           Perform
             varying counter from 1 by 1
             until counter > 5
               Display prodID(counter)
               Display prodName(counter)
           End-perform.
           GOBACK.
       END PROGRAM "display".

       END PROGRAM "EXPREVAL".
