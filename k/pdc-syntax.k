
module PDC-ID-SYNTAX

  syntax PDCId ::= "@UC" "(" Id ")"
                 | "@LC" "(" Id ")"
                 | ".PDCId"
  
endmodule

module PDC-MSG-SYNTAX

  imports PDC-ID-SYNTAX

  syntax PDCMSG    ::= "@MSG" "(" PDCId "," PDCId "," PDCId ")" [seqstrict]
                     | ".PDCMSG"
  syntax PDCMSGList ::= ".PDCMSGList"
                      | PDCMSG PDCMSGList                      [right, strict(2)]

endmodule

module PDC-RULE-SYNTAX

  imports PDC-MSG-SYNTAX
  imports PDC-ID-SYNTAX

  syntax PDCParamList ::= List {PDCId, ","}

  syntax PDCRule   ::= "@rule" PDCId "(" PDCParamList ")" RuleBody
                                                           
  syntax RuleBody  ::= "{" EntryList "}"                   
  syntax EntryList ::= Entry
                     | Entry EntryList                     
  syntax Entry     ::= "@start" RuleBody                    
                     | "@start" Entry                       
                     | "@seq" RuleBody                      
                     | "@one-of" RuleBody                   
                     | "@many-of" RuleBody                  
                     | "@optional" RuleBody                 
                     | "@optional" Entry                    
                     | PDCMSG

endmodule

module PDC-SYNTAX

  imports PDC-MSG-SYNTAX
  imports PDC-ID-SYNTAX
  imports PDC-RULE-SYNTAX

  //List {PDCMSG, " "}



endmodule
