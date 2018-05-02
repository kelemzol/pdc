
{-# LANGUAGE RecordWildCards
           , StandaloneDeriving
           , ViewPatterns
           , ScopedTypeVariables
           #-}

module Main where

import Data.List
import Data.List.Split

import Test.Tasty
import Test.Tasty.HUnit

import System.Directory
import Control.Monad
import Control.Concurrent

import Language.PDC.Parser
import Language.PDC.Repr
import Language.PDC.Interpreter
import Language.PDC.Interpreter.Utils
import Language.PDC.Interpreter.EvalRepr
import Language.PDC.Interpreter.Env
import Language.PDC.Interpreter.Scope



main :: IO ()
main = do
    dirContext <- listDirectory "./test/units/"
    let units = sort dirContext
    putStrLn ""
    files <- forM (map ("./test/units/"++) units) readFile
    mvar <- newMVar 0
    let unitTests = map processFileContent (zip files units)
        tests = map (unitTestTree mvar) $ filter uactive unitTests
    putStrLn "Found unit tests:"
    forM_ (zip [1..] unitTests) $ \ (i,u) -> do
        let activityIcon = if uactive u then "[X]" else "[_]"
        putStrLn ("  " ++ show i ++ ":\t" ++ activityIcon ++ " " ++ (unitFn u))
        if uactive u
            then return ()
            else putStrLn $ "\n    " ++ (filter ('\n'/=) (comment u)) ++ "\n"
    defaultMain (testGroup "Unit Tests from files (./test/units/)" tests)


sayYoTest :: TestTree
sayYoTest = testCase "Testing sayYo"
  (assertEqual "Should say Yo to Friend!" (3*2) 6)


unitTestTree :: MVar Integer -> PDCUnitTest -> TestTree
unitTestTree mvar (PDCUnitTest {..}) =
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
                (Just re) -> let node = ast2node m { pdcCallUnivSeqNum = Just mvar } (pdcRulePattern re)
                             in Right (res2res (evalNode node msglist emptyBoundEnv emptyScopeH))

res2res :: EvalNodeRes -> SimpleRes
res2res EvalNodeFail {..} = Failed (prettyPDCRulePattern failedPattern) (fmap (\m -> (prettyPDCRulePattern $ PDCMsgPattern m) {- ++ (show $ sourceInfoMsg m) -} ) failedMsg) boundEnv
res2res EvalNodeSuccess {..} = Success boundEnv --(updateIdEnv boundEnv phElim))
  where
    phElim a = if isPrefixOf "phantom" a then "phantom" else a

data SimpleRes
  = Failed
    { failedPattern_ :: String
    , failedMessage :: Maybe String
    , env :: BoundEnv -- String
    }
  | Success
    { env :: BoundEnv -- String
    }
  deriving (Eq, Show, Read)


data PDCUnitTest
  = PDCUnitTest
    { unitFn  :: String
    , unitTId :: String
    , uactive :: Bool
    , comment :: String
    , pdcCode :: String
    , msgList :: String
    , tResult :: String
    }
  deriving (Eq, Show)

processFileContent :: (String,String) -> PDCUnitTest
processFileContent (str, unitFn) = case splitOn ";" str of
    [unitTId, pdcCode, msgList, tResult] ->  let uactive = True in let comment = "" in PDCUnitTest {..}
    [unitTId, reads -> [(uactive,_::String)], comment, pdcCode, msgList, tResult] -> PDCUnitTest {..}

