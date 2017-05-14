
module Main where

import Control.Monad
import Data.List
import Data.Ord

import Test.Tasty
import Test.Tasty.HUnit

import PDC.KRunner

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

data RuleResult
  = Passive
  | Running
  | Success
  | Failed
  deriving (Eq, Show)

(&.) c1 c2 = \ a -> (c1 a) && (c2 a)

parseRuleResult :: String -> Maybe RuleResult
parseRuleResult str = case filter ((/= ' ')) str of
    "@Passive" -> Just Passive
    "@Running" -> Just Failed
    "@Success" -> Just Success
    "@Failed"  -> Just Failed
    _ -> Nothing

unitTests = testGroup "Unit tests"
    [ {- positiveTest "prim-pos-1"
    , -} positiveTest "prim-pos-2"
    , negativeTest "prim-neg-1"
    , negativeTest "prim-neg-2"
    , negativeTest "prim-neg-3"
    , positiveTest "seq-pos-1"
    , positiveTest "seq-pos-2"
    , positiveTest "seq-pos-3"
    , positiveTest "seq-pos-4"
    , negativeTest "seq-neg-1"
    , negativeTest "seq-neg-2"
    , negativeTest "seq-neg-3"
    ]
{-
    [ testCase "rule prim-pos-1" $ do
        conf <- parseconf <$> readFile "./.pdc_krunner"
        res <- dowork "pdc-semantics.k" "seq-pos-4.pdc" (Just True) ["msglist=seq-pos-4-msglist.txt"]
        assertEqual "ruleresult" (parseRuleResult (getRunResult (krun_stdout res))) (Just Success)
    ]
-}

gentest :: RuleResult -> String -> TestTree
gentest expRes id = testCase id $ do
    conf <- parseconf <$> readFile "./.pdc_krunner"
    res <- dowork "pdc-semantics.k" (id ++ ".pdc") (Just True) ["msglist="++(id++"-msglist.txt")]
    assertEqual "ruleresult" (parseRuleResult (getRunResult (krun_stdout res))) (Just expRes)

positiveTest = gentest Success
negativeTest = gentest Failed


