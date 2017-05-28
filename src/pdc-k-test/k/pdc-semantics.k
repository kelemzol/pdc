require "pdc-syntax.k"

module PDC-ID-SEMANTICS

  imports PDC-ID-SYNTAX

  rule @UC(A) ==PDCId @UC(B) => true                       when A ==K B
  rule @LC(A) ==PDCId @LC(B) => true                       when A ==K B
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

  endmodule


module PDC-RULE-SEMANTICS

  imports PDC-SYNTAX
  imports PDC-ID-SEMANTICS
  imports PDC-MSG-SEMANTICS
  imports PDC-SETUP

    

// running:
//   \pdc>pdc-krunner k\pdc-semantics.k examples\ex1.pdc msglist=examples\ex1_msglist.txt

    // init rule
    //-------------------------------------------------------------------------


    rule <k> @rule I ( PL ) B </k>
         <orig> .EntryList             => prepareEL ( prepareB ( B ) ) </orig>
         <current-el> .EntryList       => prepareEL ( prepareB ( B ) ) </current-el>
         <params> .PDCParamList        => PL </params>
         <metadata> <name> .K          => I </name> </metadata>
         <rulestatus> @InitPrepare     => @InitStart </rulestatus>

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
    //rule boundMSGVariables ( @MSG ( _ , _ , MSG1 ) , @MSG ( _ , _ , MSG2 ))



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
//    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , O ) => O // ( @start @instantly .EntryList )
//                                                           [owise]
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , .EntryList ) => .EntryList // ( @start @instantly .EntryList )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , OMSG EL ) =>
                   OMSG ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )
                                                           [owise]

/*
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @MSG ( @LC ( IDV ) , @UC ( ID2 ), MSGT ) ) EL) =>
                   @MSG ( @UC ( ID ) , @UC ( ID2 ) , MSGT ) ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @MSG ( @UC ( ID2 ) , @LC ( IDV ), MSGT ) ) EL) =>
                   @MSG ( @UC ( ID2 ) , @UC ( ID ) , MSGT ) ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )
    rule boundVariable ( @UC ( ID ) , @LC ( IDV ) , ( @MSG ( @UC ( ID2 ) , @UC ( ID3 ), MSGT ) ) EL) =>
                   @MSG ( @UC ( ID2 ) , @UC ( ID3 ) , MSGT ) ( boundVariable ( @UC ( ID ) , @LC ( IDV ) , EL ) )

*/
    // processing rules
    // ========================================================================

    // START ------------------------------------------------------------------
    // rule <current-el> @start { EL } NEXT     => appendEL(EL, NEXT) </current-el>

    rule <current-el> @start @instantly NEXT => NEXT </current-el>
//       boundMSGVariables ( @MSG(@UC(String2Id("A")),@UC(String2Id("B")),@UC(String2Id("ABS"))) , @MSG(@UC(String2Id("A")),@LC(String2Id("b")),@UC(String2Id("ABS"))) , @MSG(@UC(String2Id("A")),@LC(String2Id("b")),@UC(String2Id("ABS"))) .EntryList) </current-el> // 
         <rulestatus> @InitStart             => @Running </rulestatus>

    rule <current-el> @start { SEL } EL      => SEL </current-el>
         <after-start> X                     => EL </after-start>
         <current-sequence> MSG OMSG </current-sequence>
         <rulestatus> @InitStart             => @Passive </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-el> @start { SEL } EL </branch-el>
                          <branch-sequence> OMSG </branch-sequence>
                          <branch-rulestatus> @InitStart </branch-rulestatus>
                      </branch>)
             ...
         </branches>

    rule <current-el> .EntryList       => OB </current-el>
         <after-start> OB              => .EntryList </after-start>
         <branches> BRS                => .Bag </branches>
         <rulestatus> @Passive         => @Running </rulestatus>



    // SEQ --------------------------------------------------------------------
    rule <current-el> @seq { EL } NEXT      => appendEL(EL, NEXT) </current-el>


    // ONE-OF -----------------------------------------------------------------
    rule <current-el> @one-of { E EL } NEXT => @one-of { EL } NEXT </current-el>
         <current-sequence> SEQUENCE </current-sequence>
         <rulestatus> RULESTATUS </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-el> E NEXT </branch-el>
                          <branch-sequence> SEQUENCE </branch-sequence>
                          <branch-rulestatus> RULESTATUS </branch-rulestatus>
                      </branch>)
             ...
         </branches>
                                                           when NotEmptyEL ( EL )
    
    rule <current-el> @one-of { E .EntryList } NEXT => E NEXT </current-el>


    // MANY-OF ----------------------------------------------------------------
    rule <current-el> @many-of { EL } NEXT => NEXT </current-el>
         <current-sequence> SEQUENCE </current-sequence>
         <rulestatus> RULESTATUS </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-el> @one-of { EL } @many-of { EL } NEXT </branch-el>
                          <branch-sequence> SEQUENCE </branch-sequence>
                          <branch-rulestatus> RULESTATUS </branch-rulestatus>
                      </branch>)
             ...
         </branches>
//                                                           when NotEmptyEL ( EL )


    // OPTIONAL ---------------------------------------------------------------
    rule <current-el> @optional { EL } NEXT => NEXT </current-el>
         <current-sequence> SEQUENCE </current-sequence>
         <rulestatus> RULESTATUS </rulestatus>
         <branches>
             (.Bag => <branch>
                          <branch-el> appendEL(EL, NEXT) </branch-el>
                          <branch-sequence> SEQUENCE </branch-sequence>
                          <branch-rulestatus> RULESTATUS </branch-rulestatus>
                      </branch>)
             ...
         </branches>



    // MESSAGE ----------------------------------------------------------------


    rule <current-el> MSG1 ES               => boundMSGVariables ( MSG2 , MSG1 , MSG1 ES) </current-el>
         <current-sequence> MSG2 MS         => MSG2 MS </current-sequence>
                                                           when containProcessVariable(MSG1) andBool (MSG1 ==PDCMSGPartial MSG2)

    rule <current-el> MSG1 ES               => ES </current-el>
         <current-sequence> MSG2 MS         => MS </current-sequence>
                                                           when MSG1 ==PDCMSG MSG2 andBool ( notContainProcessVariable ( MSG1 ) )


    // current branch failed, activate anoter one
    rule <current-el> MSG1 ES               => EL </current-el>
         <current-sequence> MSG2 MS         => SL </current-sequence>
         <rulestatus> X                     => RULESTATUS </rulestatus>
         <branches>
             (<branch>
                 <branch-el> EL </branch-el>
                 <branch-sequence> SL </branch-sequence>
                 <branch-rulestatus> RULESTATUS </branch-rulestatus>
             </branch>
             => .Bag)
             ...
         </branches>
                                                           when MSG1 =/=PDCMSG MSG2
                                                                andBool (containProcessVariable(MSG1) impliesBool (notBool(MSG1 ==PDCMSGPartial MSG2)))
    // Message list empty
    rule <current-el> MSG:PDCMSG ES         => EL </current-el>
         <current-sequence> .PDCMSGList     => SL </current-sequence>
         <rulestatus> X                     => RULESTATUS </rulestatus>
         <branches>
             (<branch>
                 <branch-el> EL </branch-el>
                 <branch-sequence> SL </branch-sequence>
                 <branch-rulestatus> RULESTATUS </branch-rulestatus>
             </branch>
             => .Bag)
             ...
         </branches>

    // InitStart msg fail



    // helpers
    // ========================================================================
    
    syntax Bool ::= "NotEmptyEL" "(" EntryList ")"         [function]
    rule NotEmptyEL ( .EntryList ) => false
    rule NotEmptyEL ( O ) => true                          [owise]

// I was glad to meet you


    // termination rules
    // ========================================================================

    rule <current-el> .EntryList </current-el>
         // <current-sequence> .PDCMSGList </current-sequence>
         <rulestatus> @Running => @Success </rulestatus>

    rule <current-el> MSG:PDCMSG EL </current-el>
         <current-sequence> .PDCMSGList </current-sequence>
         <branches> .Bag </branches>
         <rulestatus> @Running => @Failed </rulestatus>

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

  syntax KResult ::= PDCRule
  // syntax KResult ::= "@Success" // RuleStatus
  // syntax KResult ::= Entry
  // syntax KResult ::= EntityList

//  syntax PDCMSG ::= ".MSG"


  configuration <T>
                    // Original Program
                    <k>
                        $PGM
                    </k>
                    // original preparated entry list
                    <orig>
                        .EntryList
                    </orig>
                    // Entries
                    <current-el>
                        .EntryList
                    </current-el>
                    // Msg list
                    <current-sequence>
                        .PDCMSGList
                    </current-sequence>
                    // temp field for non-activated entities without start
                    <after-start>
                        .EntryList
                    </after-start>
                    <branches>
                        <branch multiplicity="*">
                            <branch-el>
                                .EntryList
                            </branch-el>
                            <branch-sequence>
                                .PDCMSGList
                            </branch-sequence>
                            <branch-rulestatus>
                                @InitInclude
                            </branch-rulestatus>
                        </branch>
                    </branches>
                    // Parameters
                    <params>
                        .PDCParamList
                    </params>
                    // Metadata
                    <metadata>
                        <name> .K </name>
                    </metadata>
                    <rulestatus>
                        @InitInclude
                    </rulestatus>
                </T>

  rule <current-sequence> .PDCMSGList =>
           &include& msglist
           .PDCMSGList
       </current-sequence>
       <rulestatus> @InitInclude => @InitPrepare </rulestatus>

endmodule

