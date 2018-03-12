
{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Interpreter.Utils where

import Data.List(find)
import Data.Data

import Data.Generics.Uniplate.Operations

import Language.PDC.Repr


-- module utils
---------------


findRuleEntry :: (GetId a) => a -> PDCModule -> Maybe PDCRuleE
findRuleEntry name mod = find (\ re -> pdcid (pdcRuleName re) == (getId name)) $ filterRuleEntries (pdcModuleEntries mod)

instanceRuleEntry :: PDCCallP -> PDCRuleE -> PDCRuleE
instanceRuleEntry (PDCCallP {..}) r@(PDCRuleE {..}) = r { pdcRulePattern = rewriteBi rewrite pdcRulePattern }
  where
    rewrite :: PDCId -> Maybe PDCId
    rewrite = undefined
    templatePairs :: [(String, String)]
    templatePairs = zip (map pdcid pdcTmplPrmsCall) (map getTmplName (pdcRuleTempParams pdcRuleType))
    getTmplName :: PDCTemplParam -> String
    getTmplName (PDCTemplProcParam (PDCTemplProcP {..})) = pdcid pdcIdTempParam


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
    
