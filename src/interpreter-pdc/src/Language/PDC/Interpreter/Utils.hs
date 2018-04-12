
{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Interpreter.Utils where

import Data.List(find)
import Data.Data

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations

import Language.PDC.Repr

import Debug.Trace

-- module utils
---------------



findRuleEntry :: (GetId a) => a -> PDCModule -> Maybe PDCRuleE
findRuleEntry name mod = find (\ re -> pdcid (pdcRuleName $ pdcRuleEntryHeader re) == (getId name)) $ filterRuleEntries (pdcModuleEntries mod)

findRecordDataTypeEntry :: (GetId a) => a -> PDCModule -> Maybe PDCRecordTypeE
findRecordDataTypeEntry name mod = find (\ re -> ucid (pdcRecordTypeName re) == (getId name)) $ filterRecordDataTypeEntries $ filterDataTypeEntries (pdcModuleEntries mod)

findMsgAttrTypeEntry :: (GetId a) => a -> PDCModule -> Maybe PDCMsgTypeE
findMsgAttrTypeEntry name mod = find (\ re -> ucid (pdcMsgTypeMsg re) == (getId name)) $ filterMsgAttrTypeEntries $ filterDataTypeEntries $ pdcModuleEntries mod


instanceRuleEntry :: PDCCallP -> PDCRuleE -> PDCRuleE
instanceRuleEntry (PDCCallP {..}) r@(PDCRuleE {..}) = r { pdcRulePattern = transformBi transform pdcRulePattern }
  where
    transform :: PDCId -> PDCId
    transform p = case find ((==) (pdcid p) . snd) templatePairs of
        Nothing -> p
        Just n -> p { pdcid = fst n }
    templatePairs :: [(String, String)]
    templatePairs = zip (map pdcid pdcTmplPrmsCall) (map getTmplName (pdcRuleTempParams (pdcRuleType pdcRuleEntryHeader)))
    getTmplName :: PDCRuleTemplParam -> String
    getTmplName (PDCRuleTemplProcParam (PDCTemplProcP {..})) = pdcid pdcTemplProcParamId
    getTmplName (PDCRuleTemplRuleParam (PDCRuleHeader {..})) = pdcid pdcRuleName


{-
  = PDCRuleE
    { sourceInfoRuleEntry :: SourceInfo
    , pdcRuleName       :: PDCId
    , pdcRuleType       :: PDCRuleType
    , pdcRulePattern    :: PDCRulePattern
    }
  = PDCRuleType
    { sourceInfoRuleType :: SourceInfo
    , pdcRuleTempParams :: [PDCTemplParam]
    , pdcRuleProcParams :: [PDCProcParam]
    }
  = PDCTemplProcParam PDCTemplProcP
  = PDCTemplProcP
    { pdcIdTempParam    :: PDCId
    }
  = PDCCallP
    { sourceInfoCall    :: SourceInfo
    , pdcRuleId         :: PDCId
    , pdcTmplPrmsCall   :: [PDCId]
    }
-}

-- list processing utils
------------------------


getOthers :: [a] -> [(a, [a])]
getOthers l = el l []
  where
    el :: [a] -> [a] -> [(a, [a])]
    el [] _ = []
    el (x:xs) l = (x,l++xs) : (el xs (x:l))

anypath :: [[a]] -> [[a]]
anypath [] = []
anypath [l] = map (:[]) l
anypath (a:al) = concat $ map (\ x -> map (:x) a) (anypath al)
    
