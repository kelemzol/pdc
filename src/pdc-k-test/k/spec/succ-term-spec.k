
require "../pdc-semantics.k"

module PDC-SPEC-SUCC-TERM

    imports PDC-SEMANTICS

// C:\Users\ezoltke\haskell\pdc\pdc>krun -d src\pdc-k-test\k\ --prove src\pdc-k-test\k\spec\succ-term-spec.k src\pdc-k-test\testfiles\prim-pos-1.pdc.proc.pdc
    rule <current-pattern> START => END </current-pattern>
         <rulestatus> @Running   => S </rulestatus>
    requires S ==RuleStatus @Success
    ensures isEntryList ( END )

endmodule
