
{-# LANGUAGE RecordWildCards
           , ViewPatterns
           #-}

module Language.PDC.Interpreter.Utils where

import Data.List(find)
import Data.Data

import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations

import Language.PDC.Repr

import System.IO.Unsafe
import Control.Concurrent.MVar

import Debug.Trace

-- module utils
---------------



findRuleEntry :: (GetId a) => a -> PDCModule -> Maybe PDCRuleE
findRuleEntry name mod = find (\ re -> pdcid (pdcRuleName $ pdcRuleEntryHeader re) == (getId name)) $ filterRuleEntries (pdcModuleEntries mod)

findRecordDataTypeEntry :: (GetId a) => a -> PDCModule -> Maybe PDCRecordTypeE
findRecordDataTypeEntry name mod = find (\ re -> ucid (pdcRecordTypeName re) == (getId name)) $ filterRecordDataTypeEntries $ filterDataTypeEntries (pdcModuleEntries mod)

findMsgAttrTypeEntry :: (GetId a) => a -> PDCModule -> Maybe PDCMsgTypeE
findMsgAttrTypeEntry name mod = find (\ re -> ucid (pdcMsgTypeMsg re) == (getId name)) $ filterMsgAttrTypeEntries $ filterDataTypeEntries $ pdcModuleEntries mod

findActionEntry :: (GetId a) => a -> PDCModule -> Maybe PDCActionE
findActionEntry name mod = find (\ re -> lcid (pdcActionName $ pdcActionHeader re) == (getId name)) $ filterActionEntries (pdcModuleEntries mod)


instanceRuleEntry :: Maybe (MVar Integer) -> PDCCallP -> PDCRuleE -> (PDCRuleE, Integer)
instanceRuleEntry pdcCallUnivSeqNum (PDCCallP {..}) r@(PDCRuleE {..}) 
    = (r { pdcRulePattern = transformBi transform pdcRulePattern }, topUniv)
  where
    transform :: PDCId -> PDCId
    transform p = case find ((==) (pdcid p) . snd) (templatePairs ++ univPairs) of
        Nothing -> p
        Just n -> p { pdcid = fst n }
    templatePairs :: [(String, String)]
    templatePairs = zip (map pdcid pdcTmplPrmsCall) (map getTmplName (pdcRuleTempParams (pdcRuleType pdcRuleEntryHeader)))
    procNames :: [String]
    procNames = map pdcid $ filter (\ p -> ulcase p == LC) $ map pdcIdProcParam $ pdcRuleProcParams (pdcRuleType pdcRuleEntryHeader)
    univPairs :: [(String, String)]
    topUniv :: Integer
    -- (univPairs, topUniv) = get $ map (\(pn,i) -> if elem pn (map fst templatePairs) then Nothing else Just (i, pn)) $ zip procNames [(pdcCallUnivSeqNum+1)..]
    (univPairs, topUniv) = get $ map (\(pn,i) -> if elem pn (map fst templatePairs) then Nothing else Just (i, pn)) $ zip procNames [ (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum)
                                                                                                                                    , (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum)
                                                                                                                                    , (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum)
                                                                                                                                    , (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum)
                                                                                                                                    , (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum), (getI pdcCallUnivSeqNum)
                                                                                                                                    ]
      where
        get [] = ([], 0)-- pdcCallUnivSeqNum)
        get ((Just (i,p)):ipss) = let (a, b) = get ipss in (("phantom" ++ show i, p):a, max i b)
        get (Nothing:ipps) = get ipps
    getTmplName :: PDCRuleTemplParam -> String
    getTmplName (PDCRuleTemplProcParam (PDCTemplProcP {..})) = pdcid pdcTemplProcParamId
    getTmplName (PDCRuleTemplRuleParam (PDCRuleHeader {..})) = pdcid pdcRuleName


getI :: Maybe (MVar Integer) -> Integer
getI (Just a) = unsafePerformIO $ do { i <- takeMVar a; putMVar a (i+1); return (i+1) }
    

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
    

flatMerged :: [([a], [[a]])] -> [(a, [a])]
flatMerged preMerged = [ (scheduled, remeaning) | (alt, altl) <- preMerged
                                      , scheduled   <- alt
                                      , remeaning   <- anypath altl
                                      ]
