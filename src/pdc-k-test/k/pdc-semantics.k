require "pdc-syntax.k"

module PDC-ID-SEMANTICS

  imports PDC-ID-SYNTAX

  rule @UC(A) ==PDCId @UC(B) => true                       when A ==K B
  rule @LC(A) ==PDCId @LC(B) => true                       when A ==K B
  rule .PDCId ==PDCId .PDCId => true
  rule _ ==PDCId _           => false                      [owise]

  rule A =/=PDCId B => false                               when A ==PDCId B
  rule _ =/=PDCId _ => true                                [owise]

  rule isEmptyPCDIdList ( .PDCIdList ) => true
  rule isEmptyPCDIdList ( _ ) => false                     [owise]

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

  rule idInsert(A, .PDCIdList) => A .PDCIdList
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

  rule @MSG(A1, B1, M1) ==PDCMSGPartial @MSG(A2, B2, M2) => true
                                                           when (M1 ==PDCId M2)
                                                           andBool ((isPI(A1) andBool isPI(A2)) impliesBool A1 ==PDCId A2)
                                                           andBool ((isPI(B1) andBool isPI(B2)) impliesBool B1 ==PDCId B2)
  rule _ ==PDCMSGPartial _ => false                        [owise]

  rule isEmptyPDCMSGList ( .PDCMSGList ) => true
  rule isEmptyPDCMSGList ( _ ) => false                    [owise]

  rule containProcessVariable ( MSG ) => notBool ( isEmptyPCDIdList ( pvsOfMsg ( MSG ) ) )
                                                           [owise]

  rule notContainProcessVariable ( MSG ) => isEmptyPCDIdList ( pvsOfMsg ( MSG ) )
                                                           [owise]

  rule pvsOfMsg(@MSG(A, B, MSG)) => pvs (A B .PDCIdList)

  rule pisOfMsg(@MSG(A, B, MSG)) => pis (A B .PDCIdList)

  rule insertIdsOfMsg(@MSG(A,B,_), L) => idInsert(A, idInsert(B, L))
                                                           [owise]

  rule dependentMSG(@MSG(A, B, _), L) => idIsMember(A, L) orBool idIsMember(B, L)
                                                           [owise]

  endmodule


module PDC-RULE-SEMANTICS

  imports PDC-SYNTAX
  imports PDC-ID-SEMANTICS
  imports PDC-MSG-SEMANTICS
  imports PDC-SETUP

    

    // init rules
    //-------------------------------------------------------------------------


    rule <k> MEL:PDCModuleEntryList </k>
         <pdc-module> .PDCModule       => processModule ( MEL ) </pdc-module>
         <rulestatus> @InitModule      => @InitStructuralCheck </rulestatus>


    rule <pdc-module> .PDCModule </pdc-module>
         <rulestatus> @InitStructuralCheck
                                       => @Failed </rulestatus>
         <k> MEL:PDCModuleEntryList </k>
         <error-code> @NoError         => errorCase ( findMainRuleId ( MEL ) ==PDCId .PDCId , @NoDeclaredMainRule , @NotFindMainRule ) </error-code>

    rule <pdc-module> MOD </pdc-module>
         <rulestatus> @InitStructuralCheck
                                       => rulestatusCase ( structuralCheck ( MOD ) , @InitPrepare , @Failed ) </rulestatus>
                                                           when checkModule ( MOD )

    rule <pdc-module> MOD </pdc-module>
         <orig> .EntryList             => prepareEL ( prepareB ( ruleBody ( getMainRule ( MOD ) ) ) ) </orig>
         <current-pattern> .EntryList  => prepareEL ( prepareB ( ruleBody ( getMainRule ( MOD ) ) ) ) </current-pattern>
         <params> _                    => ruleParams ( getMainRule ( MOD ) ) </params>
         <current-process-instances> .PDCIdList
                                       => pis ( paramListToIdList ( ruleParams ( getMainRule ( MOD ) ) ) ) </current-process-instances>
         <metadata> <name> .K          => ruleId ( getMainRule ( MOD ) ) </name> </metadata>
         <rulestatus> @InitPrepare     => @InitStart </rulestatus>
                                                           when checkModule ( MOD )
/*
    rule <k> @rule I ( PL ) B </k>
         <orig> .EntryList             => prepareEL ( prepareB ( B ) ) </orig>
         <current-pattern> .EntryList  => prepareEL ( prepareB ( B ) ) </current-pattern>         
         <params> .PDCParamList        => PL </params>
         <metadata> <name> .K          => I </name> </metadata>
         <rulestatus> @InitPrepare     => @InitStart </rulestatus>
*/
    // internal module building
    // ------------------------------------------------------------------------

    // syntax PDCModule ::= "@PDCModule" "(" PDCRule "," PDCRuleList ")"
    // syntax PDCModule ::= "processModule" "(" PDCModuleEntryList ")"
    // syntax PDCId     ::= "findMainRuleId" "(" PDCModuleEntryList ")"
    // syntax PDCRule   ::= "lookupRule" "(" PDCId "," PDCModuleEntryList ")"
    // syntax PDCRuleList ::= "filterRules" "(" PDCModuleEntryList ")"
    // syntax PDCId       ::= "ruleId" "(" PDCRule ")"
    // syntax RuleBody    ::= "ruleBody" "(" PDCRule ")"
    // syntax PDCParamList ::= "ruleParams" "(" PDCRule ")"
    // syntax PDCRule ::= "getMainRule" "(" PDCModule ")"       [function]
    // syntax Bool ::= "checkModule" "(" PDCModule ")"        [function]
    // syntax PDCModule ::= "verifyModule" "(" PDCModule ")"
    // syntax Bool ::= "checkRule" "(" PDCRule ")"              [function]
    // syntax ErrorCode ::= "errorCase" "(" Bool "," ErrorCode "," ErrorCode ")"

    rule processModule ( MEL ) => verifyModule ( @PDCModule ( lookupRule ( findMainRuleId ( MEL ) , MEL ) , MEL ) )
                                                           [owise]
    rule verifyModule ( @PDCModule ( .PDCRule , _ ) ) => .PDCModule
    rule verifyModule ( M ) => M                           [owise]

    rule findMainRuleId ( ( @main-rule ID ) MEL ) => ID
    rule findMainRuleId ( .PDCModuleEntryList ) => .PDCId
    rule findMainRuleId ( _ MEL ) => findMainRuleId ( MEL )
                                                           [owise]

    rule lookupRule ( ID , ( @rule ID P B ) MEL ) => @rule ID P B
    rule lookupRule ( .PDCId , _ ) => .PDCRule
    rule lookupRule ( _ , .PDCModuleEntryList ) => .PDCRule
    rule lookupRule ( ID , _ MEL ) => lookupRule ( ID , MEL )
                                                           [owise]

    rule ruleId ( @rule I P B ) => I                       [owise]

    rule ruleBody ( @rule I P B ) => B                     [owise]

    rule ruleParams ( @rule I P B ) => P                   [owise]

    rule getMainRule ( @PDCModule ( R , _ ) ) => R         [owise]

    rule getRules ( @PDCModule ( _ , RL ) ) => RL          [owise]

    rule checkModule ( .PDCModule ) => false
    rule checkModule ( _ ) => true                         [owise]

    rule checkRule ( .PDCRule ) => false
    rule checkRule ( _ ) => true                           [owise]

    rule errorCase ( true , E , _ ) => E
    rule errorCase ( _ , _ , E ) => E                      [owise]

    rule rulestatusCase ( true , RS , _ ) => RS
    rule rulestatusCase ( _ , _ , RS ) => RS               [owise]


    rule paramListToIdList ( ( ) ) => .PDCIdList
    rule paramListToIdList ( ( .RuleParamList ) ) => .PDCIdList
    rule paramListToIdList ( ( I:PDCId)  ) => I .PDCIdList
    rule paramListToIdList ( ( I , L:RuleParamList ) ) => I paramListToIdList ( ( L ) )


    rule structuralCheck ( MOD ) => true

    rule isEmptyEntryList ( .EntryList ) => true
    rule isEmptyEntryList ( _ )          => false               [owise]


    // preparating/restructuring helper rule-functions
    // ------------------------------------------------------------------------
    

    syntax EntryList ::= "prepareB" "(" RuleBody ")"       [function]
    rule ( prepareB ( { } ) ) => .EntryList
    rule ( prepareB ( { EL } ) ) => ( prepareEL ( EL ) )

    syntax EntryList ::= "prepareEL" "(" EntryList ")"     [function]
    rule ( prepareEL ( E:Entry EL:EntryList ) ) => ( prepareE ( E ) prepareEL ( EL ) )
    rule ( prepareEL ( E:Entry ) ) => ( prepareE ( E ) .EntryList )
    rule ( prepareEL ( .EntryList ) ) => .EntryList        [owise]


    syntax Entry ::= "prepareE" "(" Entry ")"              [function]
    rule (prepareE ( @start E:Entry ) )          => ( @start    { prepareE ( E ) .EntryList    } )
    rule (prepareE ( @start B:RuleBody ) )       => ( @start    { prepareEL ( prepareB ( B ) ) } )
    rule (prepareE ( @optional E:Entry ) )       => ( @optional { prepareE ( E ) .EntryList    } )
    rule (prepareE ( @optional B:RuleBody ) )    => ( @optional { prepareEL ( prepareB ( B ) ) } )
    rule (prepareE ( @seq B:RuleBody ) )         => ( @seq      { prepareEL ( prepareB ( B ) ) } )
    rule (prepareE ( @one-of B:RuleBody ) )      => ( @one-of   { prepareEL ( prepareB ( B ) ) } )
    rule (prepareE ( @more-of B:RuleBody ) )     => ( @more-of  { prepareEL ( prepareB ( B ) ) } )
    rule (prepareE ( @many-of B:RuleBody ) )     => ( @many-of  { prepareEL ( prepareB ( B ) ) } )
    rule (prepareE ( M:PDCMSG ) )                => M
    rule (prepareE ( O ) ) => O                            [owise]


    syntax EntryList ::= "appendEL" "(" EntryList "," EntryList ")"
                                                           [function]
    rule appendEL ( E EL , O ) => E appendEL ( EL , O )
    rule appendEL ( .EntryList , O ) => O                  [owise]


    syntax EntryList ::= "boundMSGVariables" "(" PDCMSG "," PDCMSG "," EntryList ")"
                                                           [function]
    rule boundMSGVariables ( @MSG ( @UC ( FROM ) , @UC ( TO ) , MSGT ) , @MSG ( @LC ( FROMV ) , @LC ( TOV ) , MSGT ) , EL ) =>
                   boundVariable ( @UC ( TO ) , @LC ( TOV ) , boundVariable ( @UC ( FROM ) , @LC ( FROMV ) , EL ) )
    rule boundMSGVariables ( @MSG ( @UC ( FROM ) , @UC ( TO ) , MSGT ) , @MSG ( @UC ( FROM ) , @LC ( TOV ) , MSGT ) , EL ) =>
                   boundVariable ( @UC ( TO ) , @LC ( TOV ) , EL )
    rule boundMSGVariables ( @MSG ( @UC ( FROM ) , @UC ( TO ) , MSGT ) , @MSG ( @LC ( FROMV ) , @UC ( TO ) , MSGT ) , EL ) =>
                   boundVariable ( @UC ( FROM ) , @LC ( FROMV ) , EL )
    rule boundMSGVariables ( _ , _ , EL ) => EL            [owise]



    syntax EntryList ::= "boundVariable" "(" PDCId "," PDCId "," EntryList ")"
                                                           [function]
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @MSG ( @LC ( IDV ) , @LC ( IDV ) , MSGT ) ) EL) =>
                   @MSG ( @UC ( ID ) , @UC ( ID ) , MSGT ) ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @MSG ( @LC ( IDV ) , ID2 , MSGT ) ) EL) =>
                   @MSG ( @UC ( ID ) , ID2 , MSGT ) ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @MSG ( ID2 , @LC ( IDV ), MSGT ) ) EL) =>
                   @MSG ( ID2 , @UC ( ID ) , MSGT ) ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )
//    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @MSG ( ID2 , ID3 , MSGT ) ) EL) =>
//                   @MSG ( ID2 , ID3 , MSGT ) ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @start { EL1 } ) EL2) =>
                   @start { boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL1 ) } ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL2 ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @optional { EL1 } ) EL2) =>
                   @optional { boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL1 ) } ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL2 ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @one-of { EL1 } ) EL2) =>
                   @one-of { boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL1 ) } ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL2 ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @many-of { EL1 } ) EL2) =>
                   @many-of { boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL1 ) } ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL2 ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @seq { EL1 } ) EL2) =>
                   @seq { boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL1 ) } ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL2 ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , .EntryList ) => .EntryList // ( @start @instantly .EntryList )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , OMSG EL ) =>
                   OMSG ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )
                                                           [owise]




    rule @Failed ==RuleStatus @Failed => true
    rule @Success ==RuleStatus @Success => true
    rule @InitInclude ==RuleStatus @InitInclude => true
    rule @InitPrepare ==RuleStatus @InitPrepare => true
    rule @InitStart ==RuleStatus @InitStart => true
    rule @Passive ==RuleStatus @Passive => true
    rule @Running ==RuleStatus @Running => true
    rule _ ==RuleStatus _ => false                         [owise]

    rule A =/=RuleStatus B => notBool (A ==RuleStatus B)   [owise]


    // processing rules
    // ========================================================================

    // START ------------------------------------------------------------------
    // ------------------------------------------------------------------------

    rule <current-pattern> @start @instantly NEXT => NEXT </current-pattern>
         <rulestatus> @InitStart       => @Running </rulestatus>

    rule <current-pattern> @start { SEL } EL
                                       => SEL </current-pattern>
         <after-start> X               => EL </after-start>
         <current-stream> MSG OMSG </current-stream>
         <rulestatus> @InitStart       => @Passive </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-pattern> @start { SEL } EL </branch-pattern>
                          <branch-stream> OMSG </branch-stream>
                          <branch-process-instances> .PDCIdList </branch-process-instances>
                          <branch-rulestatus> @InitStart </branch-rulestatus>
                      </branch>)
             ...
         </branches>

    rule <current-pattern> .EntryList  => OB </current-pattern>
         <after-start> OB              => .EntryList </after-start>
         <branches> BRS                => .Bag </branches>
         <rulestatus> @Passive         => @Running </rulestatus>



    // SEQ --------------------------------------------------------------------
    // ------------------------------------------------------------------------
    rule <current-pattern> @seq { EL } NEXT
                                       => appendEL(EL, NEXT) </current-pattern>



    // ONE-OF -----------------------------------------------------------------
    // ------------------------------------------------------------------------
    rule <current-pattern> @one-of { E EL } NEXT
                                       => @one-of { EL } NEXT /* E NEXT */ </current-pattern>
         <current-stream> SEQUENCE </current-stream>
         <current-process-instances> PIS </current-process-instances>
         <rulestatus> RULESTATUS </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-pattern> E NEXT /* @one-of { EL } NEXT */ </branch-pattern>
                          <branch-stream> SEQUENCE </branch-stream>
                          <branch-process-instances> PIS </branch-process-instances>
                          <branch-rulestatus> RULESTATUS </branch-rulestatus>
                      </branch>)
             ...
         </branches>



    // MORE-OF ----------------------------------------------------------------
    // ------------------------------------------------------------------------
    rule <current-pattern> @more-of { EL } NEXT
                                       => @one-of { EL } @more-of { EL } NEXT </current-pattern>
         <current-stream> SEQUENCE </current-stream>
         <current-process-instances> PIS </current-process-instances>
         <rulestatus> RULESTATUS </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-pattern> @one-of { EL } NEXT </branch-pattern>
                          <branch-stream> SEQUENCE </branch-stream>
                          <branch-process-instances> PIS </branch-process-instances>
                          <branch-rulestatus> RULESTATUS </branch-rulestatus>
                      </branch>)
             ...
         </branches>
         <ee> @DirectExecution </ee>

    // algebraic variant
    rule <current-pattern> @more-of { EL } NEXT
                                       => @one-of { EL }
//                                          @one-of {
//                                              @seq { .EntryList }
//                                              @more-of { EL }
//                                              .EntryList
//                                          }
                                          @optional { @more-of { EL } .EntryList }
                                          NEXT
         </current-pattern>
         <ee> @AlgebraicExecution </ee>


/*    
    rule <current-pattern> @more-of { EL } NEXT
                                       => @seq {
                                              @one-of { EL }
                                              @optional {
                                                  @more-of { EL }
                                                  .EntryList
                                              }
                                              NEXT
                                          }
                                          .EntryList
         </current-pattern>
         <ee> @AlgebraicExecution </ee>
*/


    // MANY-OF ----------------------------------------------------------------
    // ------------------------------------------------------------------------
    rule <current-pattern> @many-of { EL } NEXT
                                       => @one-of { EL } @many-of { EL } NEXT </current-pattern>
         <current-stream> SEQUENCE </current-stream>
         <current-process-instances> PIS </current-process-instances>
         <rulestatus> RULESTATUS </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-pattern> NEXT </branch-pattern>
                          <branch-stream> SEQUENCE </branch-stream>
                          <branch-process-instances> PIS </branch-process-instances>
                          <branch-rulestatus> RULESTATUS </branch-rulestatus>
                      </branch>)
             ...
         </branches>
         <ee> @DirectExecution </ee>


    // algebraic variant

    rule <current-pattern> @many-of { EL } NEXT
                                       => @optional { @more-of { EL } .EntryList } NEXT
         </current-pattern>
         <ee> @AlgebraicExecution </ee>

/*
    rule <current-pattern> @many-of { EL } NEXT
                                       => @one-of {
                                              @seq {
                                                  @one-of { EL }
                                                  @one-of {
                                                      @seq { .EntryList }
                                                      @many-of { EL }
                                                      .EntryList
                                                  }
                                                  .EntryList
                                              }
                                              @seq { .EntryList }
                                              .EntryList
                                          }
                                          NEXT
         </current-pattern>
         <ee> @AlgebraicExecution </ee>
*/


    // OPTIONAL ---------------------------------------------------------------
    // ------------------------------------------------------------------------

    rule <current-pattern> @optional { EL } NEXT
                                       => NEXT </current-pattern>
         <current-stream> SEQUENCE </current-stream>
         <current-process-instances> PIS </current-process-instances>
         <rulestatus> RULESTATUS </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-pattern> appendEL(EL, NEXT) </branch-pattern>
                          <branch-stream> SEQUENCE </branch-stream>
                          <branch-process-instances> PIS </branch-process-instances>
                          <branch-rulestatus> RULESTATUS </branch-rulestatus>
                      </branch>)
             ...
         </branches>
         <ee> @DirectExecution </ee>


    // algebraic variant
    rule <current-pattern> @optional { EL } NEXT
                                       => @one-of {
                                              @seq { EL }
                                              @seq { .EntryList }
                                              .EntryList
                                          }
                                          NEXT
         </current-pattern>
         <ee> @AlgebraicExecution </ee>



    // CALL -------------------------------------------------------------------
    // ------------------------------------------------------------------------

    rule <current-pattern> ( ID:PDCId NEXT )
              => appendEL ( prepareEL ( prepareB ( ruleBody ( lookupRule ( ID , getRules ( MOD ) ) ) ) ) , NEXT ) </current-pattern>
         <pdc-module> ( MOD:PDCModule ) </pdc-module>
                                                           when checkRule ( lookupRule ( ID , getRules ( MOD ) ) )


    // MESSAGE ----------------------------------------------------------------
    // ------------------------------------------------------------------------


    rule <current-pattern> MSG1 ES     => boundMSGVariables ( MSG2 , MSG1 , MSG1 ES) </current-pattern>
         <current-stream> MSG2 MS      => MSG2 MS </current-stream>
                                                           when containProcessVariable(MSG1) andBool (MSG1 ==PDCMSGPartial MSG2)

    rule <current-pattern> MSG1 ES     => ES </current-pattern>
         <current-stream> MSG2 MS      => MS </current-stream>
         <current-process-instances> PIS => insertIdsOfMsg(MSG1, PIS) </current-process-instances>
                                                           when MSG1 ==PDCMSG MSG2 andBool ( notContainProcessVariable ( MSG1 ) )


    // drop a msg from stream if it is independent from the system
    rule <current-pattern> MSG1 ES </current-pattern>
         <current-stream> MSG2 MS      => MS </current-stream>
         <current-process-instances> PIS </current-process-instances>
                                                           when MSG1 =/=PDCMSG MSG2
                                                                andBool (containProcessVariable(MSG1) impliesBool (notBool(MSG1 ==PDCMSGPartial MSG2)))
                                                                andBool (notBool (dependentMSG(MSG2, PIS)))


    // current branch failed, activate anoter one
    rule <current-pattern> MSG1 ES     => EL </current-pattern>
         <current-stream> MSG2 MS      => SL </current-stream>
         <current-process-instances> _ => PIS </current-process-instances>
         <rulestatus> X                => RULESTATUS </rulestatus>
         <branches>
             // ...
             (<branch>
                 <branch-pattern> EL </branch-pattern>
                 <branch-stream> SL </branch-stream>
                 <branch-process-instances> PIS </branch-process-instances>
                 <branch-rulestatus> RULESTATUS </branch-rulestatus>
             </branch>
             => .Bag)
             ...
         </branches>
                                                           when MSG1 =/=PDCMSG MSG2
                                                                andBool (containProcessVariable(MSG1) impliesBool (notBool(MSG1 ==PDCMSGPartial MSG2)))
                                                                andBool (dependentMSG (MSG2, PIS))

    // current branch failed and haven't more -> @Failed
    rule <current-pattern> MSG1 ES </current-pattern>
         <current-stream> MSG2 MS </current-stream>
         <current-process-instances> PIS </current-process-instances>
         <rulestatus> X                => @Failed </rulestatus>
         <branches> .Bag </branches>
         <error-code> _                => @SequenceMissmatch </error-code>
                                                           when MSG1 =/=PDCMSG MSG2
                                                                andBool (X =/=RuleStatus @Failed)
                                                                andBool (containProcessVariable(MSG1) impliesBool (notBool(MSG1 ==PDCMSGPartial MSG2)))
                                                                andBool (dependentMSG (MSG2, PIS))

    // Message list empty
    rule <current-pattern> MSG:PDCMSG ES
                                       => EL </current-pattern>
         <current-stream> .PDCMSGList  => SEQUENCE </current-stream>
         <current-process-instances> _ => PIS </current-process-instances>
         <rulestatus> X                => RULESTATUS </rulestatus>
         <branches>
             // ...
             (<branch>
                 <branch-pattern> EL </branch-pattern>
                 <branch-stream> SEQUENCE </branch-stream>
                 <branch-process-instances> PIS </branch-process-instances>
                 <branch-rulestatus> RULESTATUS </branch-rulestatus>
             </branch>
             => .Bag)
             ...
         </branches>

    // one-of empty, but have branch
    rule <current-pattern> @one-of { .EntryList } ES
                                       => EL </current-pattern>
         <current-stream> _            => SEQUENCE </current-stream>
         <current-process-instances> _ => PIS </current-process-instances>
         <rulestatus> _                => RULESTATUS </rulestatus>
         <branches>
             // ...
             (<branch>
                 <branch-pattern> EL </branch-pattern>
                 <branch-stream> SEQUENCE </branch-stream>
                 <branch-process-instances> PIS </branch-process-instances>
                 <branch-rulestatus> RULESTATUS </branch-rulestatus>
             </branch>
             => .Bag)
             ...
         </branches>

    // one-of empty, but not have branch
    rule <current-pattern> @one-of { .EntryList } NEXT </current-pattern>
         <rulestatus> RULESTATUS       => @Failed </rulestatus>
         <branches> .Bag </branches>
         // <error-code> _                => @EmptyOneOf </error-code>
                                                           when RULESTATUS =/=RuleStatus @Failed


    // helpers
    // ========================================================================
    
    syntax Bool ::= "NotEmptyEL" "(" EntryList ")"         [function]
    rule NotEmptyEL ( .EntryList ) => false
    rule NotEmptyEL ( O ) => true                          [owise]

// I was glad to meet you


    // termination rules
    // ========================================================================

    rule <current-pattern> .EntryList </current-pattern>
         // <current-stream> .PDCMSGList </current-stream>
         <rulestatus> @Running         => @Success </rulestatus>
         <branches> _                  => .Bag </branches>

    rule <current-pattern> MSG:PDCMSG EL </current-pattern>
         <current-stream> .PDCMSGList </current-stream>
         <branches> .Bag </branches>
         <rulestatus> @Running         => @Failed </rulestatus>


endmodule

module PDC-SEMANTICS

  imports PDC-SYNTAX
  imports PDC-ID-SEMANTICS
  imports PDC-MSG-SEMANTICS
  imports PDC-RULE-SEMANTICS

endmodule




module PDC-SETUP

  imports PDC-SYNTAX
  imports PDC-RULE-STATUS

  // syntax KResult ::= PDCId
  syntax KResult ::= PDCRule
  syntax KResult ::= PDCModuleEntryList
  // syntax KResult ::= "@Success"
  // syntax KResult ::= "@Failed"


  syntax ExecutionEngine ::= "@DirectExecution"
                           | "@AlgebraicExecution"


  configuration <T>
                    // Original Program
                    <k> $PGM:PDCModuleEntryList </k>
                    // original preparated entry list
                    <orig> .EntryList </orig>
                    // Entries
                    <current-pattern> .EntryList </current-pattern>
                    // Msg list
                    <current-stream> .PDCMSGList </current-stream>
                    // List of accepted process instances
                    <current-process-instances> .PDCIdList </current-process-instances>
                    // temp field for non-activated entities without start
                    <after-start> .EntryList </after-start>
                    <pdc-module> .PDCModule </pdc-module>
                    <branches>
                        <branch multiplicity="*">
                            <branch-pattern> .EntryList </branch-pattern>
                            <branch-stream> .PDCMSGList </branch-stream>
                            <branch-rulestatus> @InitInclude </branch-rulestatus>
                            <branch-process-instances> .PDCIdList </branch-process-instances>
                        </branch>
                    </branches>
                    // Parameters
                    <params> ( ) </params>
                    // Metadata
                    <metadata>
                        <name> .K </name>
                    </metadata>
                    <ee> @AlgebraicExecution </ee>
                    // <ee> @DirectExecution </ee>
                    <error-code> @NoError </error-code>
                    <rulestatus> @InitInclude </rulestatus>
                </T>
// reachability logic k-ban
// cikk
  rule <current-stream> .PDCMSGList =>
           &include& msglist
           .PDCMSGList
       </current-stream>
       <rulestatus> @InitInclude => @InitModule </rulestatus>

endmodule

