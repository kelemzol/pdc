
module PDC-ID-SYNTAX

  syntax PDCId ::= "@UC" "(" Id ")"
                 | "@LC" "(" Id ")"
                 | ".PDCId"

  syntax Bool ::= PDCId "==PDCId" PDCId                    [function, left]
  syntax Bool ::= PDCId "=/=PDCId" PDCId                   [function, left]
  // is process variable
  syntax Bool ::= "isPV" "(" PDCId ")"                     [function]
  // is process instance
  syntax Bool ::= "isPI" "(" PDCId ")"                     [function]

  syntax PDCIdList ::= ".PDCIdList"
                      | PDCId PDCIdList                    [right, strict(2)]

  syntax Bool ::= "isEmptyPCDIdList" "(" PDCIdList ")"     [function]

  // filter PI-s
  syntax PDCIdList ::= "pis" "(" PDCIdList ")"             [function]
  // filter PV-s
  syntax PDCIdList ::= "pvs" "(" PDCIdList ")"             [function]
  syntax PDCIdList ::= "idConcat" "(" PDCIdList "," PDCIdList ")"
                                                           [function]
  syntax PDCIdList ::= "idInsert" "(" PDCId "," PDCIdList ")"
                                                           [function]
  syntax PDCIdList ::= "idRemove" "(" PDCId "," PDCIdList ")"
                                                           [function]
  syntax Bool      ::= "idIsMember" "(" PDCId "," PDCIdList ")"
                                                           [function]

endmodule

module PDC-MSG-SYNTAX

  imports PDC-ID-SYNTAX

  syntax PDCMSG ::= "@MSG" "(" PDCId "," PDCId "," PDCId ")"
                                                           [seqstrict]
  syntax PDCMSGList ::= ".PDCMSGList"
                      | PDCMSG PDCMSGList                  [right, strict(2)]

  syntax Bool ::= "isEmptyPDCMSGList" "(" PDCMSGList ")"   [function]

  syntax Bool ::= PDCMSG "==PDCMSG" PDCMSG                 [function, left]
  syntax Bool ::= PDCMSG "=/=PDCMSG" PDCMSG                [function]
  syntax Bool ::= PDCMSG "==PDCMSGPartial" PDCMSG          [function]
  syntax Bool ::= "containProcessVariable" "(" PDCMSG ")"  [function]
  syntax Bool ::= "notContainProcessVariable" "(" PDCMSG ")"
                                                           [function]
  syntax PDCIdList  ::= "pvsOfMsg" "(" PDCMSG ")"          [function]
  syntax PDCIdList  ::= "pisOfMsg" "(" PDCMSG ")"          [function]

  syntax PDCIdList ::= "insertIdsOfMsg" "(" PDCMSG "," PDCIdList ")"
                                                           [function]

  syntax Bool ::= "dependentMSG" "(" PDCMSG "," PDCIdList ")"
                                                           [function]

endmodule

module PDC-RULE-SYNTAX

  imports PDC-MSG-SYNTAX
  imports PDC-ID-SYNTAX

  syntax PDCModuleEntryList
                   ::= PDCModuleEntry PDCModuleEntryList   [right, strict(2)]
                    | ".PDCModuleEntryList"

  syntax PDCModuleEntry
                   ::= PDCRule
                     | "@main-rule" PDCId


  syntax PDCRule   ::= "@rule" PDCId "(" PDCParamList ")" RuleBody
                     | ".PDCRule"
                                                           
  syntax RuleBody  ::= "{" EntryList "}"
                     | "{" "}"                   
  syntax EntryList ::= Entry
                     | Entry EntryList                     [right, strict(2)]
                     | ".EntryList"
  syntax Entry     ::= "@start" RuleBody
                     | "@start" Entry
                     | "@start" "@instantly"
                     | "@seq" RuleBody
                     | "@one-of" RuleBody
                     | "@more-of" RuleBody
                     | "@many-of" RuleBody
                     | "@optional" RuleBody
                     | "@optional" Entry
                     | PDCMSG

  syntax PDCParamList ::= List {PDCId, ","}


  // interal syntax element
  syntax Bool      ::= "isEntryList" "(" EntryList ")"     [function]

  // interal syntax element
  syntax PDCModule ::= "@PDCModule" "(" PDCRule "," PDCModuleEntryList ")"
                     | ".PDCModule"

  // interal syntax element
  syntax PDCModule ::= "processModule" "(" PDCModuleEntryList ")"
                                                           [function]
  // interal syntax element
  syntax PDCModule ::= "verifyModule" "(" PDCModule ")"
                                                           [function]
  // interal syntax element
  syntax PDCId     ::= "findMainRuleId" "(" PDCModuleEntryList ")"
                                                           [function]
  // interal syntax element
  syntax PDCRule   ::= "lookupRule" "(" PDCId "," PDCModuleEntryList ")"
                                                           [function]
  // interal syntax element
  syntax PDCId     ::= "ruleId" "(" PDCRule ")"            [function]
  // interal syntax element
  syntax RuleBody  ::= "ruleBody" "(" PDCRule ")"          [function]
  // interal syntax element
  syntax PDCParamList ::= "ruleParams" "(" PDCRule ")"     [function]
  // interal syntax element
  syntax Bool ::= "buildedModule" "(" PDCModule ")"        [function]
  // interal syntax element
  syntax PDCRule ::= "getMainRule" "(" PDCModule ")"       [function]
  // interal syntax element
  syntax PDCRuleList ::= "getRules" "(" PDCModule ")"      [function]

endmodule

module PDC-RULE-STATUS

  syntax RuleStatus ::= "@InitInclude" // definitly init, include the msglist
                      | "@InitModule"  // preprocess rules, build the module
                      | "@InitPrepare" // prapare phase, ast correction, fille configuration cells
                      | "@InitStart"
                      | "@Passive"
                      | "@Running"
                      | "@Failed"
                      | "@Success"

  syntax Bool ::= RuleStatus "==RuleStatus" RuleStatus     [function]
  syntax Bool ::= RuleStatus "=/=RuleStatus" RuleStatus    [function]

endmodule

module PDC-SYNTAX

  imports PDC-MSG-SYNTAX
  imports PDC-ID-SYNTAX
  imports PDC-RULE-SYNTAX

  //List {PDCMSG, " "}



endmodule
