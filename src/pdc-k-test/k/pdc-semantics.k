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
         <rulestatus> @Passive         => @Running </rulestatus>


    // preparating rules/functions
    // ------------------------------------------------------------------------
    
    //rule (E:Entry EL:EntryList):EntryList => E ~> EL [structural]
    //rule (E:Entry):EntryList => E .EntryList [macro, structural]

/*
    rule { E } => E [structural]
    rule @rule I PL EL => EL ~> PL [structural]
    rule @rule I PL EL => EL ~> I [structural]
    rule (E:Entry EL:EntryList):EntryList => E ~> EL [structural]
    rule (E:Entry):EntryList => E .EntryList
*/

//    syntax PDCRule ::= "prepare" "(" PDCRule ")"      [function]
//    rule (prepare ( @rule I PL { EL } )) => @rule I PL { prepareEL ( EL ) }
//                                                      [owise]

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

/*
    syntax Entry ::= "prepareE" "(" Entry ")"              [function]
    rule (prepareE ( @start E:Entry ) )               => ( @start    { prepareE ( E ) .EntryList } )
    rule (prepareE ( @start { EL:EntryList } ) )      => ( @start    { prepareEL ( EL )          } )
    rule (prepareE ( @optional E:Entry ) )            => ( @optional { prepareE ( E ) .EntryList } )
    rule (prepareE ( @optional { EL:EntryList } ) )   => ( @optional { prepareEL ( EL )          } )
    rule (prepareE ( @seq { EL:EntryList } ) )        => ( @seq      { prepareEL ( EL )          } )
    rule (prepareE ( @one-of { EL:EntryList } ) )     => ( @one-of   { prepareEL ( EL )          } )
    rule (prepareE ( @many-of { EL:EntryList } ) )    => ( @many-of  { prepareEL ( EL )          } )
    rule (prepareE ( M:PDCMSG ) )                     => M
    rule (prepareE ( O ) ) => O                            [owise]
*/
    syntax EntryList ::= "appendEL" "(" EntryList "," EntryList ")"
                                                           [function]
    rule appendEL ( E EL , O ) => E appendEL ( EL , O )
    rule appendEL ( .EntryList , O ) => O                  [owise]


    // processing rules
    // ========================================================================

    // START ------------------------------------------------------------------
    rule <current-el> @start { EL } NEXT    => appendEL(EL, NEXT) </current-el>


    // SEQ --------------------------------------------------------------------
    rule <current-el> @seq { EL } NEXT      => appendEL(EL, NEXT) </current-el>


    // ONE-OF -----------------------------------------------------------------
    rule <current-el> @one-of { E EL } NEXT => @one-of { EL } NEXT </current-el>
         <current-sequence> SEQUENCE </current-sequence>
         <branches>
             (.Bag => <branch>
                          <branch-el> E NEXT </branch-el>
                          <branch-sequence> SEQUENCE </branch-sequence>
                      </branch>)
             ...
         </branches>
                                                           when NotEmptyEL ( EL )
    
    rule <current-el> @one-of { E .EntryList } NEXT => E NEXT </current-el>


    // MANY-OF ----------------------------------------------------------------
    rule <current-el> @many-of { EL } NEXT => NEXT </current-el>
         <current-sequence> SEQUENCE </current-sequence>
         <branches>
             (.Bag => <branch>
                          <branch-el> @one-of { EL } @many-of { EL } NEXT </branch-el>
                          <branch-sequence> SEQUENCE </branch-sequence>
                      </branch>)
             ...
         </branches>
                                                           when NotEmptyEL ( EL )


    // OPTIONAL ---------------------------------------------------------------
    rule <current-el> @optional { EL } NEXT => NEXT </current-el>
         <current-sequence> SEQUENCE </current-sequence>
         <branches>
             (.Bag => <branch>
                          <branch-el> appendEL(EL, NEXT) </branch-el>
                          <branch-sequence> SEQUENCE </branch-sequence>
                      </branch>)
             ...
         </branches>


    rule <current-el> MSG1 ES               => ES </current-el>
         <current-sequence> MSG2 MS         => MS </current-sequence>
                                                           when MSG1 ==PDCMSG MSG2

    // MESSAGE ----------------------------------------------------------------
    // current branch failed, activate anoter one
    rule <current-el> MSG1 ES               => EL </current-el>
         <current-sequence> MSG2 MS         => SL </current-sequence>
         <branches>
             (<branch>
                 <branch-el> EL </branch-el>
                 <branch-sequence> SL </branch-sequence>
             </branch>
             => .Bag)
             ...
         </branches>
                                                           when MSG1 =/=PDCMSG MSG2
    // Message list empty
    rule <current-el> MSG:PDCMSG ES         => EL </current-el>
         <current-sequence> .PDCMSGList     => SL </current-sequence>
         <branches>
             (<branch>
                 <branch-el> EL </branch-el>
                 <branch-sequence> SL </branch-sequence>
             </branch>
             => .Bag)
             ...
         </branches>


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
                    <branches>
                        <branch multiplicity="*">
                            <branch-el>
                                .EntryList
                            </branch-el>
                            <branch-sequence>
                                .PDCMSGList
                            </branch-sequence>
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
                        @Init
                    </rulestatus>
                </T>

  rule <current-sequence> .PDCMSGList =>
           &include& msglist
           .PDCMSGList
       </current-sequence>
       <rulestatus> @Init => @Passive </rulestatus>

endmodule

