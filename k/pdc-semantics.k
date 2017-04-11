require "pdc-syntax.k"

module PDC-ID-SEMANTICS

  imports PDC-ID-SYNTAX

  rule @UC(A) ==PDCId @UC(B) => true                       when A ==K B
  rule @LC(A) ==PDCId @LC(B) => true                       when A ==K B
  rule _ ==PDCId _           => false                      [owise]

  rule A =/=PDCId B => false                               when A ==PDCId B
  rule _ =/=PDCId _ => true                                [owise]

  rule isPV(@UC(_)) => true
  rule isPV(_)      => false                               [owise]

  rule isPI(@LC(_)) => true
  rule isPI(_)      => false                               [owise]

  rule pvs (.PDCIdList) => .PDCIdList
  rule pvs (@LC(A) L)   => @LC(A) pvs(L)
  rule pvs (_ L)        => pvs(L)                          [owise]

  rule pis (.PDCIdList) => .PDCIdList
  rule pis (@UC(A) L)   => @UC(A) pis(L)
  rule pis (_ L)        => pis(L)                          [owise]

  rule idConcat(.PDCIdList, L) => L
  rule idConcat(A AS, L)       => A idConcat(AS, L)        [owise]

  rule idInsert(A, .PDCIdList) => A
  rule idInsert(A, B L)        => B idInsert(A, L)         when A =/=PDCId B
  rule idInsert(_, B L)        => B L                      [owise]

  rule idRemove(_, .PDCIdList) => .PDCIdList
  rule idRemove(A, B L)        => B idRemove(A, L)         when A =/=PDCId B
  rule idRemove(_, _ L)        => L                        [owise]

  rule idIsMember(_, .PDCIdList) => false
  rule idIsMember(A, B L)      => idIsMember(A, L)         when A =/=PDCId B
  rule idIsMember(_, _)        => true                     [owise]

endmodule

module PDC-MSG-SEMANTICS

  imports PDC-ID-SYNTAX
  imports PDC-MSG-SYNTAX
  imports PDC-ID-SEMANTICS

  rule @MSG(A1, B1, M1) ==PDCMSG @MSG(A2, B2, M2) => true  when A1 ==PDCId A2 andBool B1 ==PDCId B2 andBool M1 ==PDCId M2
  rule _ ==PDCMSG _ => false                               [owise]

  rule @MSG(A1, B1, M1) =/=PDCMSG @MSG(A2, B2, M2) => true when A1 =/=PDCId A2 orBool B1 =/=PDCId B2 orBool M1 =/=PDCId M2
  rule _ =/=PDCMSG _ => false                              [owise]

  rule pvsOfMsg(@MSG(A, B, Msg)) => pvs (A B .PDCIdList)

  rule pisOfMsg(@MSG(A, B, Msg)) => pis (A B .PDCIdList)

  endmodule

module PDC-RULE-SEMANTICS

  imports PDC-SYNTAX
  imports PDC-ID-SEMANTICS
  imports PDC-MSG-SEMANTICS

endmodule

module PDC-SEMANTICS

  imports PDC-SYNTAX
  imports PDC-ID-SEMANTICS
  imports PDC-MSG-SEMANTICS
  imports PDC-RULE-SEMANTICS

  syntax KResult ::= PDCRule

  configuration <T color="yellow">
                    <k color="green">
                        $PGM
                    </k>
                    <sc>
                        .PDCMSGList
                    </sc>
                </T>

  rule <sc> .PDCMSGList =>
           &include& msglist
           .PDCMSGList
       </sc>

endmodule
