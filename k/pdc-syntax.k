module PDC-SYNTAX

  syntax PDCId ::= "@UC" "(" Id ")"
                 | "@LC" "(" Id ")"
                 | ".PDCId"

  syntax PDCMSG    ::= "@MSG" "(" PDCId "," PDCId "," PDCId ")" [seqstrict]
                     | ".PDCMSG"
  syntax PDCMSGList ::= ".PDCMSGList"
                      | PDCMSG PDCMSGList                      [right, strict(2)]



  //List {PDCMSG, " "}


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
