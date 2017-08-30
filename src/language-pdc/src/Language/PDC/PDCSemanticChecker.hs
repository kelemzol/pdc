{-# LANGUAGE ViewPatterns
           #-}

module Language.PDC.PDCSemanticChecker where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Either
import Data.Typeable

import Language.PDC.Repr
import Language.PDC.SemanticChecker

import Debug.Trace

data PDCSlogen
  = ModuleNameUpperCase
  | MultipleExport
  | MultipleRuleDefinition
  | NotExistRuleDefinition
  | RuleTypeError
  deriving (Eq, Ord, Show)

type PDCSemanticChecker level a = SemanticChecker PDCSlogen level a

showPDCSlogen :: PDCSlogen -> String
showPDCSlogen ModuleNameUpperCase = "Module name upper case!"
showPDCSlogen MultipleExport = "Multiple export!"
showPDCSlogen MultipleRuleDefinition = "Multiple rule definition!"
showPDCSlogen NotExistRuleDefinition = "Not exist rule definition!"
showPDCSlogen RuleTypeError = "Ruletype error!"

runPDCSemanticChecker :: PDCModule -> IO [Issue PDCSlogen]
runPDCSemanticChecker = (snd <$>) . (runSemanticChecker checkPDCModule)

checkPDCModule :: PDCSemanticChecker PDCModule ()
checkPDCModule = do
    down pdcModuleName $ errorAssert1 ModuleNameUpperCase
        (may ((/= LC) . ulcase))
        (\ (Just i) -> "  The module name must be upper case: " ++ pdcid i)
    downA (filterExportEntries . pdcModuleEntries) (filterRuleEntries . pdcModuleEntries) $ \ rules -> do
        errorAssert1 MultipleExport
            findMultiples
            ruleNameAtSource
        errorAssert1 NotExistRuleDefinition
            (findNotExists rules)
            ruleNameAtSource
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
