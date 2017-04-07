require "pdc-syntax.k"

module PDC-SEMANTICS

  imports PDC-SYNTAX

  syntax KResult ::= PDCRule

  configuration <T color="yellow">
                    <k color="green"> $PGM </k>
                    // <state color="red"> .List </state>
                    <sc>
                        .PDCMSGList
                    </sc>
                </T>

//rule <sc> .PDCMSGList => @MSG ( @UC ( String2Id("A") ) , @UC ( String2Id("B") ) , @UC ( String2Id("REQ") ) ) .PDCMSGList </sc>



  rule <sc> .PDCMSGList =>
                        &include& msglist
                        .PDCMSGList
       </sc>
          


/*
  syntax Bool ::= PDCMSG "==PDCMSG" PDCMSG                 [function, left]
  rule @MSG(A1, B1, M1) ==PDCMSG @MSG(A2, B2, M2) => true  when A1 ==K A2 andBool B1 ==K B2 andBool M1 ==K M2
  rule _ ==PDCMSG _ => false                               [owise]

  syntax Bool ::= PDCMSG "=/=PDCMSG" PDCMSG                [function]
  rule @MSG(A1, B1, M1) =/=PDCMSG @MSG(A2, B2, M2) => true when A1 =/=K A2 orBool B1 =/=K B2 orBool M1 =/=K M2
  rule _ =/=PDCMSG _ => false                               [owise]
*/


endmodule
