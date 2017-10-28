{-# LANGUAGE ViewPatterns
           , DeriveGeneric
           #-}

module Language.PDC.PDCSemanticChecker where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Either
import Data.Typeable
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL

import GHC.Generics (Generic)

import Language.PDC.Repr
import Language.PDC.SemanticChecker

import Debug.Trace

data PDCIssue
  = ModuleNameUpperCase PDCId
  | MultipleExport
  | MultipleRuleDefinition
  | NotExistRuleDefinition
  | RuleTypeError
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCIssue
instance JSON.FromJSON PDCIssue

instance Message PDCIssue where
    message2readable = showPDCIssue
    message2workable = JSON.toJSON -- BL.unpack . JSON.encode
    workable2message = (\ (JSON.Success a) -> a) . JSON.fromJSON -- JSON.decode . BL.pack

type PDCSemanticChecker level a = SemanticChecker PDCIssue level a

showPDCIssue :: PDCIssue -> String
showPDCIssue (ModuleNameUpperCase i) = "Module name upper case!\n  The module name must be upper case: " ++ (pdcid i)
showPDCIssue MultipleExport = "Multiple export!"
showPDCIssue MultipleRuleDefinition = "Multiple rule definition!"
showPDCIssue NotExistRuleDefinition = "Not exist rule definition!"
showPDCIssue RuleTypeError = "Ruletype error!"



runPDCSemanticChecker :: PDCModule -> IO [Issue PDCIssue]
runPDCSemanticChecker = (snd <$>) . (runSemanticChecker checkPDCModule)

checkPDCModule :: PDCSemanticChecker PDCModule ()
checkPDCModule = do
    down pdcModuleName $ errorAssert1
        (may ((/= LC) . ulcase))
        (\ (Just i) -> ModuleNameUpperCase i) -- "  The module name must be upper case: " ++ pdcid i)



{-
    downA (filterExportEntries . pdcModuleEntries) (filterRuleEntries . pdcModuleEntries) $ \ rules -> do
        errorAssert1
            findMultiples
            ruleNameAtSource
        errorAssert1
            (findNotExists rules)
            ruleNameAtSource
-}
{-
            down (filterRuleEntries . pdcModuleEntries) $ do
        errorAssert1 MultipleRuleDefinition
            findMultiples
            ruleNameAtSource

        downTA (filterRuleEntries . pdcModuleEntries) (map pdcRuleType . filterRuleEntries . pdcModuleEntries) $ \ ruleTypes -> do
        -- errorAssert1 RuleTypeError
        --     may ruleTypecheck
        --     undefined
        downRA pdcRulePattern (const ruleTypes) $ \ ruleTypes -> do
            errorAssert1 RuleTypeError
                undefined
                undefined


ruleTypecheck :: PDCRuleE -> [PDCRuleType] -> [PDCId]
ruleTypecheck = undefined

ruleNameAtSource :: (GetSourceInfo a, GetRuleName a) => [a] -> String
ruleNameAtSource l = unlines $ concat $ map (\ e -> [ "  " ++ (pdcid (getRuleName e)) ,"    at: " ++ stringSourceInfo e]) l

trace' s l = let l' = map getRuleName l in trace (s ++ ": len: " ++ (show $ length l) ++ ": " ++ show l') l

enub :: (GetRuleName a, Eq a) => [a] -> [a]
enub = nubBy eeq

eeq :: (GetRuleName a, GetRuleName b) => a -> b -> Bool
eeq a b = (pdcid (getRuleName a)) == (pdcid (getRuleName b))

findMultiples :: (GetRuleName a, Eq a) => [a] -> [a]
findMultiples l = filter (\ a -> not (null (filter (eeq a) multiples))) l
  where
    multiples = l \\ (enub l)

findNotExists :: (GetRuleName a, GetRuleName b) => [a] -> [b] -> [a]
findNotExists base exists = filter (\ a -> null (filter (eeq a) exists)) base
-}
