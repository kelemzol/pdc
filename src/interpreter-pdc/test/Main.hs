
{-# LANGUAGE RecordWildCards
           , StandaloneDeriving #-}

module Main where

import Data.List
import Data.List.Split

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import Control.Monad

import Language.PDC.Parser
import Language.PDC.Repr
import Language.PDC.Interpreter
import Language.PDC.Interpreter.Utils
import Language.PDC.Interpreter.EvalRepr
import Language.PDC.Interpreter.Env



main :: IO ()
main = do
    dirContext <- listDirectory "./test/units/"
    let units = sort dirContext
    putStrLn "Found unit tests:"
    forM_ (zip [1..] units) (\ (i,n) -> putStrLn ("  " ++ show i ++ ":\t" ++ n))
    putStrLn ""
    files <- forM (map ("./test/units/"++) units) readFile
    let unitTests = map processFileContent (zip files units)
        tests = map unitTestTree unitTests
    defaultMain (testGroup "Unit Tests from files (./test/units/)" tests)


sayYoTest :: TestTree
sayYoTest = testCase "Testing sayYo"
  (assertEqual "Should say Yo to Friend!" (3*2) 6)

unitTestTree :: PDCUnitTest -> TestTree
unitTestTree (PDCUnitTest {..}) =
    testCase (unitTId ++ " \\ (" ++ unitFn ++ ")")
             (assertEqual "Result" res (Right (read tResult :: SimpleRes)))
  where
    parsedModule =  moduleParser unitFn pdcCode
    parsedMsgList = msgListParser unitFn msgList
    res = case (parsedModule, parsedMsgList) of
        (Left r, Left m) -> Left ("Rule parsing fail\n" ++ show r ++ "\nMsg list parsing fail" ++ show m)
        (Left r, _) -> Left ("Rule parsing fail\n" ++ show r)
        (_, Left m) -> Left ("Msg list parsing fail" ++ show m)
        (Right m@(PDCModule {..}), Right msglist) -> do
            case findRuleEntry "test" m of
                Nothing -> Left ("not found main rule: test")
                (Just re) -> let node = ast2node m (pdcRulePattern re)
                             in Right (res2res (evalNode node msglist emptyBoundEnv))

res2res :: EvalNodeRes -> SimpleRes
res2res EvalNodeFail {..} = Failed (fmap (prettyPDCRulePattern . PDCMsgPattern) failedMsg) (show boundEnv)
res2res EvalNodeSuccess {..} = Success (show boundEnv)

data SimpleRes
  = Failed
    { failedMessage :: Maybe String
    , env :: String
    }
  | Success
    { env :: String
    }
  deriving (Eq, Show, Read)


data PDCUnitTest
  = PDCUnitTest
    { unitFn  :: String
    , unitTId :: String
    , pdcCode :: String
    , msgList :: String
    , tResult :: String
    }
  deriving (Eq, Show)

processFileContent :: (String,String) -> PDCUnitTest
processFileContent (str, unitFn) = let [unitTId, pdcCode, msgList, tResult] = splitOn ";" str in PDCUnitTest {..}
