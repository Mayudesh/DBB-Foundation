       IDENTIFICATION DIVISION.
       PROGRAM-ID.  cobperf.
       AUTHOR.  IBM.
      * A program that allows you to test out the various
      * COBOL Optimization Techniques - Jon
      * New comment

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
      **** Fields for your test cases
      **** Table and ALL Function

       01  COBOL-DATE.
           05  Cob-DATE.
             10  Cob-YEAR       PIC 9(4) Value 0.
             10  Cob-MONTH      PIC 99 Value 0.
             10  Cob-DAY        PIC 99 Value 0.
           05  Cob-TIME.
             10  Cob-HOUR       PIC 99 Value 0.
             10  Cob-MINUTES    PIC 99 Value 0.
             10  Cob-SECONDS    PIC 99 Value 0.
             10  Cob-SEC-HUND   PIC 99 Value 0.
           05  Cob-TIME-RDF REDEFINES Cob-TIME PIC 9(8).
           05  Cob-TIME-Difference.
             10  GMT-DIR        PIC X  Value space.
             10  GMT-HOUR       PIC 99 Value 0.
             10  GMT-MINUTES    PIC 99 Value 0.

      **** Important note - Lower these values (by a factor of 100
      ****  (at least) when running on RDz Local Workstations

       77  multiplier         pic s9(8) comp value 10000000.
       77  multiplier-2       pic s9(8) comp value  2000000.

       01  Hold-date.
           05            pic x(8).
           05 hold-time  pic 9(8).

       01  Hdr-rec-1.
           05    Pic x(43) value "Use Case Details".
           05    Pic x(6)  value "ML Sec".

       01  Hdr-rec-2.
           05    Pic x(43)
           value "==========================================".
           05    Pic x(6)  value "======".

       01  Out-rec.
           05 msg-out      Pic x(43).
           05 Difference   Pic z(6).

       01  subscripts.
           05 j        pic s9(4) comp.
           05 i        pic s9(4) comp.
           05 idsp     pic s9(3).
           05 zval     pic s9(4) comp.

       01  Table-Data.
           05 dta pic x(32) value "11112222333344445555666677778888".
           05 dta-rdf redefines dta.
               10 item occurs 8 times pic s99v99.
           05 Total        pic 9(11)V99 value zero.
           05 Discount     pic V99 value .03.

       01  Table-Data-srch.
           03 dt.
           05 dt9 pic x(32) value "11112222333344445555666677778888".
           05 dtb pic x(32) value "99990000aaaabbbbccccddddeeeeffff".
           05 dtc pic x(32) value "gggghhhhiiiijjjjkkkkllllmmmmnnnn".
           05 dtd pic x(32) value "ooooppppqqqqrrrrssssttttuuuuvvvv".
           05 dte pic x(32) value "wwwwxxxxyyyyzzzzAAAABBBBCCCCDDDD".
           05 dtf pic x(32) value "EEEEFFFFGGGGHHHHIIIIJJJJKKKKLLLL".
           05 dtg pic x(32) value "MMMMNNNNOOOOPPPPQQQQRRRRSSSSTTTT".
           05 dth pic x(32) value "UUUUVVVVWWWWXXXXYYYYZZZZA111B111".
           05 dti pic x(32) value "C111D111E111F111G111H111I111J111".
           05 dtj pic x(32) value "K111L111M111N111O111P111Q111R111".
           05 dtk pic x(32) value "S111T111U111V111W111X111Y111Z111".
           05 dtl pic x(32) value "A222B222C222D222E222F222G222H222".
           05 dtm pic x(32) value "I222J222K222L222M222N222O222P222".
           05 dtn pic x(32) value "Q222R222S222T222U222V222W222X222".
           05 dto pic x(32) value "a111a222a333a444a555a666a777a888".
           05 dtp pic x(32) value "a999a000baaaabbbacccadddaeeeafff".
           05 dtq pic x(32) value "agggahhhaiiiajjjakkkalllammmannn".
           05 dtr pic x(32) value "aoooapppaqqqarrrasssatttauuuavvv".
           05 dts pic x(32) value "awwwaxxxayyyazzzbAAAbBBBbCCCbDDD".
           05 dtt pic x(32) value "bEEEbFFFbGGGbHHHbIIIJJJJKKKKLLLL".
           05 dtu pic x(32) value "bMMMbNNNbOOObPPPbQQQbRRRbSSSbTTT".
           05 dtv pic x(32) value "bUUUbVVVbWWWbXXXbYYYbZZZbb11b211".
           05 dtw pic x(32) value "ba11bb11bc11bd11be11bf11bg11bh11".
           05 dtx pic x(32) value "bi11bj11bk11bl11bm11bn11bo11bp11".
           05 dty pic x(32) value "bq11Tr11bs11bt11bu11bv11bw11bx11".
           05 dtz pic x(32) value "by22bz22b922b822b722b622b522b422".
           05 dt1 pic x(32) value "be22be22Ke22berebrewbrewqeq2Prew".
           05 dt2 pic x(32) value "basdfghjkl;mnopqurstuvwxyz123456".
           03 dt-rdf redefines dt.
               05 itm  occurs 224 times.
               10 flddisp pic x(4).

       01  Table-Data-srch-idx.
           03 dtx.
           05 dt9 pic x(32) value "11112222333344445555666677778888".
           05 dtb pic x(32) value "99990000aaaabbbbccccddddeeeeffff".
           05 dtc pic x(32) value "gggghhhhiiiijjjjkkkkllllmmmmnnnn".
           05 dtd pic x(32) value "ooooppppqqqqrrrrssssttttuuuuvvvv".
           05 dte pic x(32) value "wwwwxxxxyyyyzzzzAAAABBBBCCCCDDDD".
           05 dtf pic x(32) value "EEEEFFFFGGGGHHHHIIIIJJJJKKKKLLLL".
           05 dtg pic x(32) value "MMMMNNNNOOOOPPPPQQQQRRRRSSSSTTTT".
           05 dth pic x(32) value "UUUUVVVVWWWWXXXXYYYYZZZZA111B111".
           05 dti pic x(32) value "C111D111E111F111G111H111I111J111".
           05 dtj pic x(32) value "K111L111M111N111O111P111Q111R111".
           05 dtk pic x(32) value "S111T111U111V111W111X111Y111Z111".
           05 dtl pic x(32) value "A222B222C222D222E222F222G222H222".
           05 dtm pic x(32) value "I222J222K222L222M222N222O222P222".
           05 dtn pic x(32) value "Q222R222S222T222U222V222W222X222".
           05 dto pic x(32) value "a111a222a333a444a555a666a777a888".
           05 dtp pic x(32) value "a999a000baaaabbbacccadddaeeeafff".
           05 dtq pic x(32) value "agggahhhaiiiajjjakkkalllammmannn".
           05 dtr pic x(32) value "aoooapppaqqqarrrasssatttauuuavvv".
           05 dts pic x(32) value "awwwaxxxayyyazzzbAAAbBBBbCCCbDDD".
           05 dtt pic x(32) value "bEEEbFFFbGGGbHHHbIIIJJJJKKKKLLLL".
           05 dtu pic x(32) value "bMMMbNNNbOOObPPPbQQQbRRRbSSSbTTT".
           05 dtv pic x(32) value "bUUUbVVVbWWWbXXXbYYYbZZZbb11b211".
           05 dtw pic x(32) value "ba11bb11bc11bd11be11bf11bg11bh11".
           05 dtx pic x(32) value "bi11bj11bk11bl11bm11bn11bo11bp11".
           05 dty pic x(32) value "bq11Tr11bs11bt11bu11bv11bw11bx11".
           05 dtz pic x(32) value "by22bz22b922b822b722b622b522b422".
           05 dt1 pic x(32) value "be22be22Ke22berebrewbrewqeq2Prew".
           05 dt2 pic x(32) value "basdfghjkl;mnopqurstuvwxyz123456".
           03 dtx-rdf redefines dtx.
               05 itx pic x(4) occurs 224 times indexed by ix.

       01  Table-Data-srch-all-idx.
           03 dtxa.
           05 dt9 pic x(32) value "00010002000300040005000600070008".
           05 dt9 pic x(32) value "00110012001300140015001600170018".
           05 dt9 pic x(32) value "00210022002300240025002600270028".
           05 dt9 pic x(32) value "00310032003300340035003600370038".
           05 dt9 pic x(32) value "00410042004300440045004600470048".
           05 dt9 pic x(32) value "00510052005300540055005600570058".
           05 dt9 pic x(32) value "00610062006300640065006600670068".
           05 dt9 pic x(32) value "00710072007300740075007600770078".
           05 dt9 pic x(32) value "00810082008300840085008600870088".
           05 dt9 pic x(32) value "00910092009300940095009600970098".
           05 dt9 pic x(32) value "01010102010301040105010601070108".
           05 dt9 pic x(32) value "02010202020302040205020602070208".
           05 dt9 pic x(32) value "03010302030303040305030603070308".
           05 dt9 pic x(32) value "04010402040304040405040604070408".
           05 dt9 pic x(32) value "05010502050305040505050605070508".
           05 dt9 pic x(32) value "06010602060306040605060606070608".
           05 dt9 pic x(32) value "07010702070307040705070607070708".
           05 dt9 pic x(32) value "08010802080308040805080608070808".
           05 dt9 pic x(32) value "09010902090309040905090609070908".
           05 dt9 pic x(32) value "10011002100310041005100610071008".
           05 dt9 pic x(32) value "20012002200320042005200620072008".
           05 dt9 pic x(32) value "30013002300330043005300630073008".
           05 dt9 pic x(32) value "40014002400340044005400640074008".
           05 dt9 pic x(32) value "50015002500350045005500650075008".
           05 dt9 pic x(32) value "60016002600360046005600660076008".
           05 dt9 pic x(32) value "70017002700370047005700670077008".
           05 dt9 pic x(32) value "80018002800380048005800680078008".
           05 dt9 pic x(32) value "90019002900390049005900690079008".
           03 dtxa-rdf redefines dtxa.
               05 itxa pic x(4) occurs 224 times
                    ASCENDING KEY IS itxa indexed by ixa.


       77  end-val         pic x.

       01  math-variables.
      ******* Integers
           05 binaryVar        pic s9(4) usage comp value +432.
           05 displayVar       pic s9(4) usage display value +432.
           05 packedVar        pic s9(5) usage comp-3 value +432.
           05 binaryVarTot     pic s9(8) usage comp.
           05 displayVarTot    pic s9(8) usage display.
           05 packedVarTot     pic s9(9) usage comp-3.
      ******* Decimal numbers
           05 binaryDec        pic s9(4)V99 usage comp value +432.98.
           05 displayDec       pic s9(4)V99 usage display value +432.98.
           05 packedDec        pic s9(5)V99 usage comp-3 value +432.98.
           05 binaryDecTot10   pic s9(8)V99 usage comp.
           05 binaryDecTot8    pic s9(6)V99 usage comp.
           05 displayDecTot    pic s9(8)V99 usage display.
           05 packedDecTot     pic s9(9)V99 usage comp-3.

      ******* WORK AREAS
           05 work-fld-10B      pic s9(8)V99 usage comp value +432.98.
           05 work-fld-10T      pic s9(8)V99 usage comp.

           05 work-fld-8B      pic s9(6)V99 usage comp value +432.98.
           05 work-fld-8T      pic s9(6)V99 usage comp.

           05 work-fld-10P     pic s9(8)V99 usage comp-3 value +432.98.
           05 work-fld-10TP    pic s9(8)V99 usage comp-3.


       PROCEDURE DIVISION.
           INITIALIZE math-variables.
      **** 1. Refactoring a Computation (remove a multiplication)
      ****
           Display "**************************************************".
           Display "****** Refactoring Multiplication Example ********".
           Display "**************************************************".
           display " ".
           display Hdr-rec-1.
           DISPLAY Hdr-rec-2.
           Move Function Current-Date to COBOL-Date.
           Move COBOL-DATE to Hold-date.
           perform Goodperf thru Goodperf-exit multiplier times.
       GoodPerf.
           MOVE ZERO TO TOTAL
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 8
               COMPUTE TOTAL = TOTAL + ITEM(I)
           END-PERFORM
           COMPUTE TOTAL = TOTAL * DISCOUNT.
       Goodperf-exit.
           exit.
       Next-Para.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Use Case - Move factor out of equation" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

           perform Badperf thru Badperf-exit multiplier times.
       BadPerf.
           MOVE ZERO TO TOTAL
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = 8
               COMPUTE TOTAL = TOTAL + ITEM(I) * DISCOUNT
           END-PERFORM.
       Badperf-exit.
           exit.
       Last-perf.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Use Case - Factor repeated in equation" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Internal data representation
      **** Part I - Integer math
           Display " ".
           Display "**************************************************".
           Display "Efficient Computation Examples - Integer - Starts:".
           Display "**************************************************".
           DISPLAY " ".

           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.
      **** Binary integer
           perform Binarycomp thru Binarycomp-exit multiplier times.
       Binarycomp.
               COMPUTE binaryVarTot  = binaryVar * binaryVar.
       Binarycomp-exit.
           exit.
       Next-binary.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Use Case - Binary Integer Multiplication" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Display integer
           perform Displaycomp thru Displaycomp-exit multiplier times.
       Displaycomp.
               COMPUTE displayVarTot  = displayVar * displayVar.
       Displaycomp-exit.
           exit.
       Next-Display.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Use Case - Display Integer Multiplication" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Packed integer
           perform Packedcomp thru Packedcomp-exit multiplier times.
       Packedcomp.
               COMPUTE packedVarTot  = packedVar * packedVar.
       Packedcomp-exit.
           exit.
       Next-packed.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Use Case - Packed Decimal Integer Mult." to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Mixed integer #s - mix packed and binary
           perform Mixedcomp thru Mixedcomp-exit multiplier times.
       Mixedcomp.
               COMPUTE binaryVarTot  = displayVar * packedVar.
       Mixedcomp-exit.
           exit.
       Next-Mixed.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Use Case - Mixed Types - Integer Mult." to msg-out.
           display Out-rec.


      **** Part II - Decimal math *********************************
      ****
           Display " ".
           Display "**************************************************".
           Display "Efficient Computation Examples - Decimal - Starts:".
           Display "**************************************************".
           DISPLAY " ".

           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date

      **** Binary decimal > 8 digits
           perform BinaryDcomp thru BinaryDcomp-exit multiplier times.
       BinaryDcomp.
               COMPUTE binaryDecTot10  = binaryDec * binaryDec.
       BinaryDcomp-exit.
           exit.

       Next-binary.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Binary S9(10) Decimal Multiplication" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Binary decimal = 8 digits
           perform BinaryD8comp thru BinaryD8comp-exit multiplier times.
       BinaryD8comp.
               COMPUTE binaryDecTot8  = binaryDec * binaryDec.
       BinaryD8comp-exit.
           exit.

       Next-binary8.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Binary S9(08) Decimal Multiplication" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Display decimal
           perform DisplayDcomp thru DisplayDcomp-exit multiplier times.
       DisplayDcomp.
               COMPUTE displayDecTot  = displayDec * displayDec.
       DisplayDcomp-exit.
           exit.

       Next-Display.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Display Decimal Multiplication" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Packed decimal
           perform PackedDcomp thru PackedDcomp-exit multiplier times.
       PackedDcomp.
               COMPUTE packedDecTot  = packedDec * packedDec.
       PackedDcomp-exit.
           exit.

       Next-packed.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Packed Decimal Multiplication" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Mixed decimal #s - mix packed result and binary * display
           perform MixedAcomp thru MixedAcomp-exit multiplier times.
       MixedAcomp.
               COMPUTE PackedDecTot  = binaryDecTot8 * DisplayDecTot.
       MixedAcomp-exit.
           exit.

       Next-MixedA.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Mixed-type Decimal Mult. Packed Total" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Mixed decimal #s - mix binary result and packed * display
           perform MixedDcomp thru MixedDcomp-exit multiplier times.
       MixedDcomp.
               COMPUTE BinaryDecTot8  = PackedDecTot * DisplayDecTot.
       MixedDcomp-exit.
           exit.

       Next-Mixed.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Mixed-type Decimal Mult. Binary8 Total" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Consistent types and usage - Binary > 8 digits
           perform Cons10comp thru Cons10comp-exit multiplier times.
       Cons10comp.
               COMPUTE work-fld-10T  = work-fld-10B * work-fld-10B.
       Cons10comp-exit.
           exit.

       Next-Cons10.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Consistent Types Decimal Mult. Binary 10" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Consistent types and usage - Binary 8 digits
           perform Cons8comp thru Cons8comp-exit multiplier times.
       Cons8comp.
               COMPUTE work-fld-8T  = work-fld-8B * work-fld-10B.
       Cons8comp-exit.
           exit.

       Next-Cons8.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Consistent Types Decimal Mult. Binary 8" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Consistent types and usage - packed > 8 digits
           perform ConsPack thru ConsPack-exit multiplier times.
       ConsPack.
               COMPUTE work-fld-10TP  = work-fld-10P * work-fld-10P.
       ConsPack-exit.
           exit.

       Next-ConsPack.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Consistent Types Decimal Mult. Packed 10" to msg-out.
           display Out-rec.


      **** Part III - Table Handling *********************************
      ****
           Display " ".
           Display "**************************************************".
           Display "Efficient Table Handling Examples - Starts:".
           Display "**************************************************".
           DISPLAY " ".

           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Table display subscript sequential process - at table end..
           perform Table-no-idxe thru Table-no-idx-exite
               multiplier-2 times.

       Table-no-idxe.
           move 0 to zVal.
           perform varying idsp from 1 by 1 until zVal > 0
               if itm(idsp) = "3456"
                   move +1 to zVal
               end-if
               end-perform.

       Table-no-idx-exite.
           exit.

       Next-Table-no-idx-exite.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Perform Varying Subscript - 244th Element" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Table display subscript sequential process - at table begin
           perform Table-no-idxb thru Table-no-idx-exitb
               multiplier-2 times.

       Table-no-idxb.
           move 0 to zVal.
           perform varying idsp from 1 by 1 until zVal > 0
               if itm(idsp) = "1111"
                   move +1 to zVal
               end-if
               end-perform.

       Table-no-idx-exitb.
           exit.

       Next-table.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Perform Varying Subscript - 1st Element" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Table display subscript sequential process - end.
           perform Table-idx thru Table-idx-exit multiplier-2 times.
       Table-idx.
               set ix to 1.
               search itx varying ix
               when itx(ix) = "3456"
                   move +0 to zVal.
       Table-idx-exit.
           exit.

       Next-table-dx.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Sequential Search - Find 244th Element" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Table display subscript sequential process - begin.
           perform Table-idxs thru Table-idx-exits multiplier-2 times.
       Table-idxs.
               set ix to 1.
               search itx varying ix
               when itx(ix) = "1111"
                   move +0 to zVal.
       Table-idx-exits.
           exit.

       Next-table-dxs.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Sequential Search - Find 1st Element" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

      **** Table display subscript sequential process - begin.
           perform Table-idxa thru Table-ida-exits multiplier-2 times.
       Table-idxa.
               set ix to 1.
               search all itxa
               when itxa(ixa) = "3456"
                   move +0 to zVal.
       Table-ida-exits.
           exit.

       Next-table-dxa.
           Move Function Current-Date to COBOL-Date.
           subtract hold-time from Cob-TIME-RDF giving Difference.
           move "Search ALL - Find 244th Element" to msg-out.
           display Out-rec.
           Move Function Current-Date to COBOL-Date.
           move COBOL-DATE to Hold-date.

           display "End of COBPERF".
      *     accept end-val.
           STOP RUN.
