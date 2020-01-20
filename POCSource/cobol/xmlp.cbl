       Identification division.
       Program-id. ValidCk.
       Environment division.
       Configuration section.
       Special-names.
           xml-schema schema is 'ddschema'.
       Data division.
       Working-storage section.
       1   xml-decode.
           2 rtn comp pic 9(2).
           2 rsn comp-5 pic 9(4).
       1   hv pic x(16) value '0123456789ABCDEF'.
       1   xml-document-1.
           2 pic x(52) value
           '<!--Valid: the "itemName" element can be omitted-->'.
           2 pic x(31) value '<stockItem itemNumber="123-AB">'.
           2 pic x(36) value ' <quantityOnHand>1</quantityOnHand>'.
           2 pic x(12) value '</stockItem>'.
       1   xml-document-2.
           2 pic x(34)
            value '<!--Invalid: missing attribute itemNumber-->'.
           2 pic x(11) value '<stockItem>'.
           2 pic x(30) value ' <itemName>No name</itemName>'.
           2 pic x(36) value ' <quantityOnHand>1</quantityOnHand>'.
           2 pic x(12) value '</stockItem>'.
       1   xml-document-3.
           2 pic x(37)
             value '<!--Invalid: unexpected attribute warehouse-->'.
           2 pic x(46) value
            '<stockItem itemNumber="074-UN" warehouse="NJ">'.
           2 pic x(37) value ' <quantityOnHand>10</quantityOnHand>'.
           2 pic x(32) value ' <itemName>Not here!</itemName>'.
           2 pic x(12) value '</stockItem>'.
       1   xml-document-4.
           2 pic x(40)
             value '<!--Invalid: illegal attribute value 123-Ab-->'.
           2 pic x(31) value '<stockItem itemNumber="123-Ab">'.
           2 pic x(33) value ' <itemName>Paintbrush</itemName>'.
           2 pic x(37) value ' <quantityOnHand>10</quantityOnHand>'.
           2 pic x(12) value '</stockItem>'.
       1   xml-document-5.
           2 pic x(32)
             value '<!--Invalid: missing element quantityOnHand-->'.
           2 pic x(31) value '<stockItem itemNumber="074-UN">'.
           2 pic x(32) value ' <itemName>Not here!</itemName>'.
           2 pic x(12) value '</stockItem>'.
       1   xml-document-6.
           2 pic x(36)
            value '<!--Invalid: unexpected element comment-->'.
           2 pic x(31) value '<stockItem itemNumber="123-AB">'.
           2 pic x(33) value ' <itemName>Paintbrush</itemName>'.
           2 pic x(36) value ' <quantityOnHand>1</quantityOnHand>'.
           2 pic x(35) value ' <comment>Nylon bristles</comment>'.
           2 pic x(12) value '</stockItem>'.
       1   xml-document-7.                           Identification division.
       Program-id. ValidCk.
       Environment division.
       Configuration section.
       Special-names.
           xml-schema schema is 'ddschema'.
       Data division.
       Working-storage section.
       1 xml-decode.
           2 rtn comp pic 9(2).
           2 rsn comp-5 pic 9(4).
       1 hv pic x(16) value '0123456789ABCDEF'.
       1 xml-document-1.
           2 pic x(52) value
           '<!--Valid: the "itemName" element can be omitted-->'.
           2 pic x(31) value '<stockItem itemNumber="123-AB">'.
           2 pic x(36) value ' <quantityOnHand>1</quantityOnHand>'.
           2 pic x(12) value '</stockItem>'.
       1 xml-document-2.
           2 pic x(44)
            value '<!--Invalid: missing attribute itemNumber-->'.
           2 pic x(11) value '<stockItem>'.
           2 pic x(30) value ' <itemName>No name</itemName>'.
           2 pic x(36) value ' <quantityOnHand>1</quantityOnHand>'.
           2 pic x(12) value '</stockItem>'.
       1 xml-document-3.
           2 pic x(47)
             value '<!--Invalid: unexpected attribute warehouse-->'.
           2 pic x(46) value
            '<stockItem itemNumber="074-UN" warehouse="NJ">'.
           2 pic x(37) value ' <quantityOnHand>10</quantityOnHand>'.
           2 pic x(32) value ' <itemName>Not here!</itemName>'.
           2 pic x(12) value '</stockItem>'.
       1 xml-document-4.
           2 pic x(50)
             value '<!--Invalid: illegal attribute value 123-Ab-->'.
           2 pic x(31) value '<stockItem itemNumber="123-Ab">'.
           2 pic x(33) value ' <itemName>Paintbrush</itemName>'.
           2 pic x(37) value ' <quantityOnHand>10</quantityOnHand>'.
           2 pic x(12) value '</stockItem>'.
       1 xml-document-5.
           2 pic x(52)
             value '<!--Invalid: missing element quantityOnHand-->'.
           2 pic x(31) value '<stockItem itemNumber="074-UN">'.
           2 pic x(32) value ' <itemName>Not here!</itemName>'.
           2 pic x(12) value '</stockItem>'.
       1 xml-document-6.
           2 pic x(46)
            value '<!--Invalid: unexpected element comment-->'.
           2 pic x(31) value '<stockItem itemNumber="123-AB">'.
           2 pic x(33) value ' <itemName>Paintbrush</itemName>'.
           2 pic x(36) value ' <quantityOnHand>1</quantityOnHand>'.
           2 pic x(35) value ' <comment>Nylon bristles</comment>'.
           2 pic x(12) value '</stockItem>'.
       1 xml-document-7.
           2 pic x(53) value
              '<!--Invalid: out-of-range element value 100-->'.
           2 pic x(31) value '<stockItem itemNumber="123-AB">'.
           2 pic x(33) value ' <itemName>Paintbrush</itemName>'.
           2 pic x(38) value ' <quantityOnHand>100</quantityOnHand>'.
           2 pic x(12) value '</stockItem>'.
       Procedure division.
       m.
           xml parse xml-document-1 validating with file schema
                  processing procedure p
           xml parse xml-document-2 validating with file schema
                  processing procedure p
           xml parse xml-document-3 validating with file schema
                   processing procedure p
           xml parse xml-document-4 validating with file schema
                   processing procedure p
           xml parse xml-document-5 validating with file schema
                   processing procedure p
           xml parse xml-document-6 validating with file schema
                   processing procedure p
           xml parse xml-document-7 validating with file schema
                   processing procedure p
           goback
           .
       p.
           evaluate xml-event
           when 'COMMENT'
           display ' '
           display xml-text
           when 'END-OF-DOCUMENT'
           display ' Document successfully parsed.'
           when 'EXCEPTION'
           move xml-code to xml-decode
           display ' RC=' rtn ', reason=x'''
           hv(function mod(rsn / 4096 16) + 1:1)
           hv(function mod(rsn / 256 16) + 1:1)
           hv(function mod(rsn / 16 16) + 1:1)
           hv(function mod(rsn 16) + 1:1) ''''
           end-evaluate
           .
       End program ValidCk.

           2 pic x(43) value
              '<!--Invalid: out-of-range element value 100-->'.
           2 pic x(31) value '<stockItem itemNumber="123-AB">'.
           2 pic x(33) value ' <itemName>Paintbrush</itemName>'.
           2 pic x(38) value ' <quantityOnHand>100</quantityOnHand>'.
           2 pic x(12) value '</stockItem>'.
       Procedure division.
       m.
           xml parse xml-document-1 validating with file schema
                  processing procedure p
           xml parse xml-document-2 validating with file schema
                  processing procedure p
           xml parse xml-document-3 validating with file schema
                   processing procedure p
           xml parse xml-document-4 validating with file schema
                   processing procedure p
           xml parse xml-document-5 validating with file schema
                   processing procedure p
           xml parse xml-document-6 validating with file schema
                   processing procedure p
           xml parse xml-document-7 validating with file schema
                   processing procedure p
           goback
           .
       p.
           evaluate xml-event
           when 'COMMENT'
           display ' '
           display xml-text
           when 'END-OF-DOCUMENT'
           display ' Document successfully parsed.'
           when 'EXCEPTION'
           move xml-code to xml-decode
           display ' RC=' rtn ', reason=x'''
           hv(function mod(rsn / 4096 16) + 1:1)
           hv(function mod(rsn / 256 16) + 1:1)
           hv(function mod(rsn / 16 16) + 1:1)
           hv(function mod(rsn 16) + 1:1) ''''
           end-evaluate
           .
       End program ValidCk.
