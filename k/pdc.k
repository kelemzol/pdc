// PDC - interprocess protocol definition and checking

module PDC-ID
  syntax PDCId     ::= Token{[A-Za-z\_][A-Za-z0-9\_\-]*}   [notInRules, regex("(?<![A-Za-z0-9\\_])[A-Za-z\\_][A-Za-z0-9\\_\\-]*"), token, autoReject]
  syntax String ::= "PDCId2String" "(" PDCId ")"         [function, hook(STRING.token2string), klabel(PDCId2String)]
  syntax PDCId ::= "String2PDCId" "(" String ")"         [function, klabel(String2PDCId)]
  rule PDCId2String(X:PDCId) => #tokenToString(X)
  rule String2PDCId(S:String) => #parseToken("PDCId", S)
endmodule

module PDC-MSG
  imports PDC-ID

  syntax PDCMSG    ::= PDCId "->" PDCId ":" PDCId          
  syntax PDCMSGList ::= List {PDCMSG, " "}

  syntax Bool ::= PDCMSG "==PDCMSG" PDCMSG                 [function, left]
  rule A1 -> B1 : M1 ==PDCMSG A2 -> B2 : M2 => true        when A1 ==K A2 andBool B1 ==K B2 andBool M1 ==K M2
  rule _ ==PDCMSG _ => false                               [owise]
  syntax Bool ::= PDCMSG "=/=PDCMSG" PDCMSG                [function]
  rule A1 -> B1 : M1 ==PDCMSG A2 -> B2 : M2 => true        when A1 =/=K A2 orBool B1 =/=K B2 orBool M1 =/=K M2
endmodule

module PDC

  imports PDC-ID
  imports PDC-MSG


  syntax PDCParamList ::= List {PDCId, ","}

  syntax PDCRule   ::= "rule" PDCId "(" PDCParamList ")" RuleBody
                                                           
  syntax RuleBody  ::= "{" EntryList "}"                   
  syntax EntryList ::= Entry
                     | Entry EntryList                     
  syntax Entry     ::= "start" RuleBody                    
                     | "start" Entry                       
                     | "seq" RuleBody                      
                     | "one-of" RuleBody                   
                     | "many-of" RuleBody                  
                     | "optional" RuleBody                 
                     | "optional" Entry                    
                     | PDCMSG

  syntax KResult ::= PDCRule

  configuration <T color="yellow">
                    <k color="green"> $PGM </k>
                    // <state color="red"> .List </state>
                </T>





endmodule
