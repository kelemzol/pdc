
module Main where

import Control.Monad
import Data.List
import Data.Ord
import Data.Typeable (Typeable)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options


import PDC.KRunner

main = defaultMain tests
{-
data TestTarget
  = AllTest
  | PrimUnitTests
  | SeqUnitTests
  | OptionalUnitTests
  | OneOfUnitTests
  | ManyOfUnitTests


data Target = Target String
  deriving (Eq, Ord, Typeable)

instance IsOption Target where
    defaultValue = "all"
-}


tests :: TestTree
tests = testGroup "All unit test" [primUnitTests, seqUnitTests, optionalUnitTests, oneOfUnitTests, manyOfTests]
-- tests ["prim"] = testGroup "Prim unit test" [primUnitTests]
-- tests ["many-of"] = testGroup "Many-of unit test" [manyOfTests]

data RuleResult
  = InitInclude
  | InitPrepare
  | Passive
  | Running
  | Success
  | Failed
  deriving (Eq, Show)

(&.) c1 c2 = \ a -> (c1 a) && (c2 a)

parseRuleResult :: String -> Maybe RuleResult
parseRuleResult str = case filter ((/= ' ')) str of
    "@InitInclude" -> Just InitInclude
    "@InitPrepare" -> Just InitPrepare
    "@Passive" -> Just Passive
    "@Running" -> Just Running
    "@Success" -> Just Success
    "@Failed"  -> Just Failed
    _ -> Nothing

isSuccess :: RuleResult -> Bool
isSuccess Success = True
isSuccess _ = False

unitTests = testGroup "All unit tests"
    $ concat
    [ primUnitTestsList
    , seqUnitTestsList
    , optionalUnitTestsList
    , oneOfUnitTestsList
    , manyOfTestsList
    ]

primUnitTests = testGroup "Prim tests" primUnitTestsList
seqUnitTests = testGroup "Seq tests" seqUnitTestsList
optionalUnitTests = testGroup "Optional tests" optionalUnitTestsList
oneOfUnitTests = testGroup "One-of tests" oneOfUnitTestsList
manyOfTests = testGroup "Many-of tests" manyOfTestsList

primUnitTestsList =
    [ positiveTest "prim-pos-1"
    , positiveTest "prim-pos-2"
    , negativeTest "prim-neg-1"
    , negativeTest "prim-neg-2"
    , negativeTest "prim-neg-3"
    ]
seqUnitTestsList =
    [ positiveTest "seq-pos-1"
    , positiveTest "seq-pos-2"
    , positiveTest "seq-pos-3"
    , positiveTest "seq-pos-4"
    , negativeTest "seq-neg-1"
    , negativeTest "seq-neg-2"
    , negativeTest "seq-neg-3"
    ]
optionalUnitTestsList =
    [ positiveTest "optional-pos-1"
    , positiveTest "optional-pos-2"
    , positiveTest "optional-pos-3"
    , positiveTest "optional-pos-4"
    , positiveTest "optional-pos-5"
    , positiveTest "optional-pos-5"
    , positiveTest "optional-pos-6"
    , positiveTest "optional-pos-7"
    , positiveTest "optional-pos-8"
    ]
oneOfUnitTestsList =
    [ positiveTest "one-of-pos-1"
    , positiveTest "one-of-pos-2"
    , positiveTest "one-of-pos-3"
    , positiveTest "one-of-pos-4"
    , positiveTest "one-of-pos-5"
    , positiveTest "one-of-pos-6"
    , negativeTest "one-of-neg-1"
    , negativeTest "one-of-neg-2"
    ]
manyOfTestsList =
    [ positiveTest "many-of-pos-1"
    , positiveTest "many-of-pos-2"
    , positiveTest "many-of-pos-3"
    , positiveTest "many-of-pos-4"
    , positiveTest "many-of-pos-5"
    , positiveTest "many-of-pos-6"
    , positiveTest "many-of-pos-7"
    , positiveTest "many-of-pos-8"
    , positiveTest "many-of-pos-9"
    ]

gentest :: (RuleResult -> Bool) -> String -> TestTree
gentest expect id = testCase id $ do
    conf <- parseconf <$> readFile "./.pdc_krunner"
    res <- dowork "pdc-semantics.k" (id ++ ".pdc") (Just True) ["msglist="++(id++"-msglist.txt")]
    assertEqual "ruleresult" (fmap expect (parseRuleResult (getRunResult (krun_stdout res)))) (Just True)

positiveTest = gentest isSuccess
negativeTest = gentest (not . isSuccess)


