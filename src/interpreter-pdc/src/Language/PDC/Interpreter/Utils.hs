

module Language.PDC.Interpreter.Utils where

import Data.List(find)

import Language.PDC.Repr


-- module utils
---------------


findRuleEntry :: (GetId a) => a -> PDCModule -> Maybe PDCRuleE
findRuleEntry name mod = find (\ re -> pdcid (pdcRuleName re) == (getId name)) $ filterRuleEntries (pdcModuleEntries mod)


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
    
