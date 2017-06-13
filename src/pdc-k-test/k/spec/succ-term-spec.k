
require "../pdc-semantics.k"

module PDC-SPEC-SUCC-TERM

    imports PDC-SEMANTICS

// C:\Users\ezoltke\haskell\pdc\pdc\src\pdc-k-test\k>krun -d . --prove spec\succ-term-spec.k ..\testfiles\prim-pos-1.pdc.proc.pdc
    rule <current-pattern> START => END </current-pattern>
         <rulestatus> @Running   => S </rulestatus>
    requires S ==RuleStatus @Success
    ensures isEntryList ( END )

endmodule
