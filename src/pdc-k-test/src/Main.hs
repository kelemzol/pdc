
{-# LANGUAGE ViewPatterns
           #-}

module Main where

import Control.Monad
import Data.List
import Data.Ord
import Data.Maybe
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
tests = testGroup "All unit test" [ primUnitTests
                                  , seqUnitTests
                                  , optionalUnitTests
                                  , oneOfUnitTests
                                  , manyOfUnitTests
                                  , moreOfUnitTests
                                  , startUnitTests
                                  , boundingUnitTests
                                  , msgRelatedUnitTests
                                  , moduleUnitTests
                                  , callUnitTests
                                  , complexUnitTests
                                  ]

data RuleResult
  = InitInclude
  | InitPrepare
  | InitStart
  | Passive
  | Running
  | Success
  | Failed
  deriving (Eq, Show)

data ErrorCode
  = NoError
  | NoDeclaredMainRule
  | NotFindMainRule
  | SequenceMissmatch
  | EmptyOneOf
  deriving (Eq, Show)

(&.) c1 c2 = \ a -> (c1 a) && (c2 a)

parseRuleResult :: String -> Maybe RuleResult
parseRuleResult str = case filter ((/=' ') &. (/='\n') &. (/='\t')) str of
    "@InitInclude" -> Just InitInclude
    "@InitPrepare" -> Just InitPrepare
    "@InitStart" -> Just InitStart
    "@Passive" -> Just Passive
    "@Running" -> Just Running
    "@Success" -> Just Success
    "@Failed"  -> Just Failed
    _ -> Nothing

parseErrorCode :: String -> Maybe ErrorCode
parseErrorCode str = case filter ((/=' ') &. (/='\n') &. (/='\t')) str of
    "@NoError" -> Just NoError
    "@NoDeclaredMainRule" -> Just NoDeclaredMainRule
    "@NotFindMainRule" -> Just NotFindMainRule
    "@SequenceMissmatch" -> Just SequenceMissmatch
    "@EmptyOneOf" -> Just EmptyOneOf
    _ -> Nothing

isSuccess :: RuleResult -> Bool
isSuccess Success = True
isSuccess _ = False

primUnitTests = testGroup "Prim tests" primUnitTestsList
seqUnitTests = testGroup "Seq tests" seqUnitTestsList
optionalUnitTests = testGroup "Optional tests" optionalUnitTestsList
oneOfUnitTests = testGroup "One-of tests" oneOfUnitTestsList
moreOfUnitTests = testGroup "More-of tests" moreOfUnitTestsList
manyOfUnitTests = testGroup "Many-of tests" manyOfUnitTestsList
startUnitTests = testGroup "Start tests" startUnitTestsList
boundingUnitTests = testGroup "Bounding tests" boundingUnitTestsList
msgRelatedUnitTests = testGroup "Msg Related tests" msgRelatedUnitTestsList
moduleUnitTests = testGroup "Module" moduleUnitTestsList
callUnitTests = testGroup "Call" callUnitTestsList
complexUnitTests = testGroup "Complex tests" complexUnitTestsList

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
    , smarttest "seq-neg-4"
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
manyOfUnitTestsList =
    [ positiveTest "many-of-pos-1"
    , positiveTest "many-of-pos-2"
    , positiveTest "many-of-pos-3"
    , positiveTest "many-of-pos-4"
    , positiveTest "many-of-pos-5"
    , positiveTest "many-of-pos-6"
    , positiveTest "many-of-pos-7"
    , positiveTest "many-of-pos-8"
    , smarttest "many-of-pos-10"
    , smarttest "many-of-pos-11"
    , positiveTest "many-of-pos-9"
    ]
startUnitTestsList =
    [ positiveTest "start-pos-1"
    , positiveTest "start-pos-2"
    , positiveTest "start-pos-3"
    , positiveTest "start-pos-4"
    , positiveTest "start-pos-5"
    , positiveTest "start-pos-6"
    , smarttest "start-pos-7"
    , smarttest "start-pos-8"
    ]
boundingUnitTestsList =
    [ smarttest "bounding-pos-1"
    , smarttest "bounding-pos-2"
    , smarttest "bounding-pos-3"
    , smarttest "bounding-pos-4"
    , smarttest "bounding-pos-5"
    , smarttest "full-bounding-pos-1"
    , smarttest "full-bounding-pos-2"
    ]
moreOfUnitTestsList =
    [ smarttest "more-of-pos-1"
    , smarttest "more-of-pos-2"
    , smarttest "more-of-pos-3"
    , smarttest "more-of-pos-4"
    , smarttest "more-of-pos-5"
    , smarttest "more-of-pos-6"
    , smarttest "more-of-pos-7"
    , smarttest "more-of-pos-8"
    , smarttest "more-of-pos-9"
    , smarttest "more-of-pos-10"
    , smarttest "more-of-pos-11"
    , smarttest "more-of-pos-12"
    , smarttest "more-of-pos-13"
    , smarttest "more-of-pos-15"
    , smarttest "more-of-pos-14"
    ]
msgRelatedUnitTestsList =
    [ smarttest "msg-related-pos-1"
    , smarttest "msg-related-pos-2"
    , smarttest "msg-related-pos-3"
    , smarttest "msg-related-pos-4"
    , smarttest "msg-related-neg-1"
    , smarttest "msg-related-neg-2"
    , smarttest "msg-related-neg-3"
    ]
moduleUnitTestsList =
    [ smarttest "module-pos-1"
    , smarttest "module-pos-2"
    , smarttest "module-pos-3"
    , smarttest "module-pos-4"
    , smarttest "module-pos-5"
    , smarttest "module-neg-1"
    , smarttest "module-neg-2"
    ]
callUnitTestsList =
    [ smarttest "call-pos-1"
    , smarttest "call-pos-2"
    , smarttest "call-pos-3"
    , smarttest "call-neg-1"
    ]
complexUnitTestsList =
    [ smarttest "complex-pos-1"
    , smarttest "complex-pos-2"
    , smarttest "complex-pos-3"
    , smarttest "complex-pos-4"
    , smarttest "complex-pos-5"
    , smarttest "complex-pos-6"
    , smarttest "complex-neg-1"
    , smarttest "complex-neg-2"
    ]

gentest :: (RuleResult -> Bool) -> String -> TestTree
gentest expect id = testCase id $ do
    conf <- parseconf <$> readFile "./.pdc_krunner"
    res <- dowork "pdc-semantics.k" (id ++ ".pdc") (Just True) ["msglist="++(id++"-msglist.txt")]
    assertEqual "ruleresult" (fmap expect (parseRuleResult (fromJust $ getRunResult (krun_stdout res)))) (Just True)

positiveTest = gentest isSuccess
negativeTest = gentest (not . isSuccess)


smarttest :: String -> TestTree
smarttest id = testCase id $ do
    conf <- parseconf <$> readFile "./.pdc_krunner"
    let path = pdc_prog_dir conf
        xmlpath = path ++ id ++ "-res.txt"
    expose conf id
    xml <- readXML xmlpath
    workRres <- dowork "pdc-semantics.k" (id ++ ".pdc") (Just True) ["msglist="++(id++"-msglist.txt")]
    
    let exceptedRuleresult = join $ fmap parseRuleResult $ xml `getBy` "rulestatus"
        exceptedErrorcode = join $ fmap parseErrorCode $ xml `getBy` "error-code"
        ruleresult = join $ fmap parseRuleResult $ getRunResult (krun_stdout workRres)
        errorcode = exceptedErrorcode >> (join $ (fmap parseErrorCode (getErrorCode (krun_stdout workRres))))

    eq <- assertEqual "ruleresult" (exceptedRuleresult, exceptedErrorcode) (ruleresult, errorcode)
    deleteExposed conf id
    return eq

