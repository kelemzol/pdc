require "pdc-syntax.k"

module PDC-ID-SEMANTICS

  imports PDC-ID-SYNTAX

  syntax Bool ::= PDCId "==PDCId" PDCId                    [function, left]
  syntax Bool ::= PDCId "=/=PDCId" PDCId                   [function, left]

  rule @UC(A) ==PDCId @UC(B) => true                       when A ==K B
  rule @LC(A) ==PDCId @LC(B) => true                       when A ==K B
  rule _ ==PDCId _ => false                                [owise]

  rule A =/=PDCId B => false                               when A ==PDCId B
  rule _ =/=PDCId _ => true                                [owise]

endmodule

module PDC-MSG-SEMANTICS

  imports PDC-ID-SYNTAX
  imports PDC-MSG-SYNTAX
  imports PDC-ID-SEMANTICS

  syntax Bool ::= PDCMSG "==PDCMSG" PDCMSG                 [function, left]
  syntax Bool ::= PDCMSG "=/=PDCMSG" PDCMSG                [function]

  rule @MSG(A1, B1, M1) ==PDCMSG @MSG(A2, B2, M2) => true  when A1 ==PDCId A2 andBool B1 ==PDCId B2 andBool M1 ==PDCId M2
  rule _ ==PDCMSG _ => false                               [owise]
  rule @MSG(A1, B1, M1) =/=PDCMSG @MSG(A2, B2, M2) => true when A1 =/=PDCId A2 orBool B1 =/=PDCId B2 orBool M1 =/=PDCId M2
  rule _ =/=PDCMSG _ => false                              [owise]

  endmodule

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
          






endmodule
