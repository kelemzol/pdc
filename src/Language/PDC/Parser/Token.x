{
module Language.PDC.Parser.Token where
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
    $white+                  ;

{

}

