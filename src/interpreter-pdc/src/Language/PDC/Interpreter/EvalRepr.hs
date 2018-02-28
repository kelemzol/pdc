{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Interpreter.EvalRepr where

import Data.Maybe
import Data.List (find)
import Control.Monad

import Language.PDC.Repr
import Language.PDC.Interpreter.Utils

import Debug.Trace

data Node
  = Node
    { pattern  :: PDCRulePattern
    , branches :: [(PDCMsgP, Node)]
    }
  | Leaf
  deriving (Eq, Show)

prettyNode :: Node -> String
prettyNode (Node _ l) = " { " ++ (concat (map (\(m, n) -> (prettyPDCRulePattern (PDCMsgPattern m)) ++ " =>" ++ (prettyNode n)) l)) ++ " } "
prettyNode Leaf = "LF"

cliNode :: Node -> IO ()
cliNode = cliNode' 0

cliNode' :: Int -> Node -> IO ()
cliNode' _ Leaf = putStrLn "Leaf"
cliNode' s o@(Node p brs)= do
  forM_ (zip brs [0..]) $ \ (b, s') -> do
    putStr $ if s' == s then " -> " else "    "
    putStrLn $ prettyPDCRulePattern $ PDCMsgPattern $ fst $ b
  str <- getLine
  case (words str) of
    ["next"]   -> next
    ["n"]      -> next
    ["prev"]   -> prev
    ["p"]      -> prev
    ["select"] -> select
    ["s"]      -> select
    []         -> select
    _          -> do
        putStrLn $ "usage: next, prev, select"
        cliNode' s o
    where
      next   = cliNode' (s+1) o
      prev   = cliNode' (s-1) o
      select = cliNode' 0 $ snd $ fst $ fromJust $ find (\ (b, s') -> s == s' ) $ zip brs [0..]


-- AST to EvalRepr

ast2node :: PDCModule -> PDCRulePattern -> Node
ast2node mod p = ast2node' mod [p]

ast2node' :: PDCModule -> [PDCRulePattern] -> Node
ast2node' _ [] = Leaf
ast2node' mod (o@(PDCMsgPattern p):tl) = msg2node mod tl o p
ast2node' mod (o@(PDCSeqPattern p):tl) = seq2node mod tl o p
ast2node' mod (o@(PDCUnSeqPattern p):tl) = unseq2node mod tl o p
ast2node' mod (o@(PDCOneOfPattern p):tl) = oneof2node mod tl o p
ast2node' mod (o@(PDCManyofPattern p):[]) = Leaf
ast2node' mod (o@(PDCManyofPattern p):tl) = manyof2node mod tl o p
ast2node' mod (o@(PDCMergePattern p):tl) = merge2node mod tl o p
ast2node' mod (o@(PDCOptionalPattern p):[]) = Leaf
ast2node' mod (o@(PDCOptionalPattern p):tl) = optional2node mod tl o p
ast2node' mod (o@(PDCMoreOfPattern p):tl) = moreof2node mod tl o p
ast2node' mod (o@(PDCCallPattern p):tl) = call2node mod tl o p



msg2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCMsgP -> Node
msg2node _ [] o p = Node o [(p, Leaf)]
msg2node mod tl o p = Node o [(p, ast2node' mod tl)]


seq2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCSeqP -> Node
seq2node mod tl o (PDCSeqP {..})
  | [] <- pdcRulePatternsSeq
    = ast2node' mod tl
  | (p:ps) <- pdcRulePatternsSeq
    = patternWithCondition mod (ps++tl) p (Node o)

patternWithCondition :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> ([(PDCMsgP, Node)] -> Node) -> Node
patternWithCondition mod tl p f = case pattern2Branch mod tl p of
    Nothing -> Leaf
    Just brs -> f brs

unseq2node ::  PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCUnSeqP -> Node
unseq2node mod tl o (PDCUnSeqP {..})
  | [] <- pdcRulePatternsUnSeq
    = ast2node' mod tl
  | otherwise = Node o (concat $ catMaybes $ map (pattern2Branch mod tl) perms)
    where
      perms = map seq (getOthers pdcRulePatternsUnSeq)
      seq (l, others) = PDCSeqPattern PDCSeqP { sourceInfoSeq = sourceInfoUnSeq, pdcRulePatternsSeq = 
        [l, PDCUnSeqPattern PDCUnSeqP { sourceInfoUnSeq = sourceInfoUnSeq, pdcRulePatternsUnSeq = others }] }


oneof2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCOneOfP -> Node
oneof2node mod tl o (PDCOneOfP {..})
  = Node o (concat $ catMaybes $ map (pattern2Branch mod tl) pdcRulePatternsOneOf)

manyof2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCManyOfP -> Node
manyof2node mod tl@(htl:ttl) o m@(PDCManyOfP {..})
    | Nothing <- nullMatch = Leaf
    | otherwise            = Node o (fromJust nullMatch ++ brs)
  where
      nullMatch = pattern2Branch mod ttl htl
      brs = concat $ catMaybes $ map (pattern2Branch mod ((PDCManyofPattern m):tl)) pdcRulePatternsManyOf

moreof2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCMoreOfP -> Node
moreof2node mod tl@(htl:ttl) o m@(PDCMoreOfP {..})
    | Nothing <- nullMatch = Leaf
    | otherwise            = Node o (brs)
  where
      nullMatch = pattern2Branch mod ttl htl
      brs = concat $ catMaybes $ map (pattern2Branch mod ((PDCManyofPattern (PDCManyOfP {pdcRulePatternsManyOf = pdcRulePatternsMoreOf, sourceInfoManyOf = sourceInfoMoreOf})):tl)) pdcRulePatternsMoreOf
      --                        This is very ugly I know.. ^
      -- brs = concat $ catMaybes $ map (pattern2Branch ((PDCMoreOfPattern m):tl)) pdcRulePatternsMoreOf

optional2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCOptionalP -> Node
optional2node mod tl@(htl:ttl) o m@(PDCOptionalP {..})
    | Nothing <- nullMatch = Leaf
    | otherwise            = Node o (fromJust nullMatch ++ brs)
  where
      nullMatch = pattern2Branch mod ttl htl
      brs = concat $ catMaybes [pattern2Branch mod tl pdcRulePatternOptional]


merge2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCMergeP -> Node
merge2node mod tl o (PDCMergeP {..})
  = Node o $ concat $ map mergePath (anypath brss)
  where
    brss :: [[(PDCMsgP, Node)]]
    brss = catMaybes $ map (pattern2Branch mod tl) pdcRulePatternsMerge


mergePath :: [(PDCMsgP, Node)] -> [(PDCMsgP, Node)]
mergePath [] = []
mergePath [p] = [p]
mergePath (p:op) = concat $ map (mergeBr p) (mergePath op)



mergeBr :: (PDCMsgP, Node) -> (PDCMsgP, Node) -> [(PDCMsgP, Node)]
mergeBr (m1, Leaf) (m2, Leaf) = [(m1, Node undefined [(m2, Leaf)]), (m2, Node undefined [(m1, Leaf)])]
mergeBr (m1, Leaf) (m2, n2) = ((m1, Node (pattern n2) [(m2, n2)]))
    :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )
mergeBr (m2, n2) (m1, Leaf) = ((m1, Node (pattern n2) [(m2, n2)]))
    :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )
mergeBr (m1, n1) (m2, n2) = concat $ [ [ (m1, Node (pattern n1) (mergeBr b1 ((\(a,b) -> (m2, Node undefined [(a,b)])) b2) ))
                                       , (m2, Node (pattern n2) (mergeBr b2 ((\(a,b) -> (m1, Node undefined [(a,b)])) b1) ))
                                       ]  | b1 <- branches n1, b2 <- branches n2 ]


call2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCCallP -> Node
call2node mod tl o (PDCCallP {..}) = ast2node' mod ((pdcRulePattern ruleEntry):tl)
  where
    Just ruleEntry = findRuleEntry pdcRuleId mod -- find (\ re -> pdcid (pdcRuleName re) == (pdcid pdcRuleId)) $ filterRuleEntries (pdcModuleEntries mod)
    







pattern2Branch :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> Maybe [(PDCMsgP, Node)]
pattern2Branch mod tl p = case (ast2node' mod (p:tl)) of
  (Node {..}) -> Just branches
  Leaf -> Nothing

