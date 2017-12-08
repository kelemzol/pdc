{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Interpreter.EvalRepr where

import Data.Maybe
import Data.List (find)
import Control.Monad
import Language.PDC.Repr
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

ast2node :: PDCRulePattern -> Node
ast2node p = ast2node' [p]

ast2node' :: [PDCRulePattern] -> Node
ast2node' [] = Leaf
ast2node' (o@(PDCMsgPattern p):tl) = msg2node tl o p
ast2node' (o@(PDCSeqPattern p):tl) = seq2node tl o p
ast2node' (o@(PDCOneOfPattern p):tl) = oneof2node tl o p
ast2node' (o@(PDCManyofPattern p):[]) = Leaf
ast2node' (o@(PDCManyofPattern p):tl) = manyof2node tl o p
ast2node' (o@(PDCMergePattern p):tl) = merge2node tl o p
ast2node' (o@(PDCOptionalPattern p):[]) = Leaf
ast2node' (o@(PDCOptionalPattern p):tl) = optional2node tl o p


msg2node :: [PDCRulePattern] -> PDCRulePattern -> PDCMsgP -> Node
msg2node [] o p = Node o [(p, Leaf)]
msg2node tl o p = Node o [(p, ast2node' tl)]


seq2node :: [PDCRulePattern] -> PDCRulePattern -> PDCSeqP -> Node
seq2node tl o (PDCSeqP {..})
  | [] <- pdcRulePatternsSeq
    = ast2node' tl
  | (p:ps) <- pdcRulePatternsSeq
    = patternWithCondition (ps++tl) p (Node o)
      {- case pattern2Branch (ps++tl) p of
      Just brs -> Node o brs
      Nothing -> Leaf-}

patternWithCondition :: [PDCRulePattern] -> PDCRulePattern -> ([(PDCMsgP, Node)] -> Node) -> Node
patternWithCondition tl p f = case pattern2Branch tl p of
    Nothing -> Leaf
    Just brs -> f brs

oneof2node :: [PDCRulePattern] -> PDCRulePattern -> PDCOneOfP -> Node
oneof2node tl o (PDCOneOfP {..})
  = Node o (concat $ catMaybes $ map (pattern2Branch tl) pdcRulePatternsOneOf)

manyof2node :: [PDCRulePattern] -> PDCRulePattern -> PDCManyOfP -> Node
manyof2node tl@(htl:ttl) o m@(PDCManyOfP {..})
    | Nothing <- nullMatch = Leaf
    | otherwise            = Node o (fromJust nullMatch ++ brs)
  where
      nullMatch = pattern2Branch ttl htl
      brs = concat $ catMaybes $ map (pattern2Branch ((PDCManyofPattern m):tl)) pdcRulePatternsManyOf


optional2node :: [PDCRulePattern] -> PDCRulePattern -> PDCOptionalP -> Node
optional2node tl@(htl:ttl) o m@(PDCOptionalP {..})
    | Nothing <- nullMatch = Leaf
    | otherwise            = Node o (fromJust nullMatch ++ brs)
  where
      nullMatch = pattern2Branch ttl htl
      brs = concat $ catMaybes [pattern2Branch tl pdcRulePatternOptional]


merge2node :: [PDCRulePattern] -> PDCRulePattern -> PDCMergeP -> Node
merge2node tl o (PDCMergeP {..})
  = Node o $ concat $ map mergePath (anypath brss)
  where
    brss :: [[(PDCMsgP, Node)]]
    brss = catMaybes $ map (pattern2Branch tl) pdcRulePatternsMerge


mergePath :: [(PDCMsgP, Node)] -> [(PDCMsgP, Node)]
mergePath [] = []
mergePath [p] = [p]
mergePath (p:op) = concat $ map (mergeBr p) (mergePath op)


anypath :: [[a]] -> [[a]]
anypath [] = []
anypath [l] = map (:[]) l
anypath (a:al) = concat $ map (\ x -> map (:x) a) (anypath al)

mergeBr :: (PDCMsgP, Node) -> (PDCMsgP, Node) -> [(PDCMsgP, Node)]
mergeBr (m1, Leaf) (m2, Leaf) = [(m1, Node undefined [(m2, Leaf)]), (m2, Node undefined [(m1, Leaf)])]
mergeBr (m1, Leaf) (m2, n2) = ((m1, Node (pattern n2) [(m2, n2)]))
    :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )
mergeBr (m2, n2) (m1, Leaf) = ((m1, Node (pattern n2) [(m2, n2)]))
    :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )
mergeBr (m1, n1) (m2, n2) = concat $ [ [ (m1, Node (pattern n1) (mergeBr b1 ((\(a,b) -> (m2, Node undefined [(a,b)])) b2) ))
                                       , (m2, Node (pattern n2) (mergeBr b2 ((\(a,b) -> (m1, Node undefined [(a,b)])) b1) ))
                                       ]  | b1 <- branches n1, b2 <- branches n2 ]


getOthers :: [a] -> [(a, [a])]
getOthers l = el l []
  where
    el :: [a] -> [a] -> [(a, [a])]
    el [] _ = []
    el (x:xs) l = (x,l++xs) : (el xs (x:l))

pattern2Branch :: [PDCRulePattern] -> PDCRulePattern -> Maybe [(PDCMsgP, Node)]
pattern2Branch tl p = case (ast2node' (p:tl)) of
  (Node {..}) -> Just branches
  Leaf -> Nothing -- trace (show $ map prettyPDCRulePattern (p:tl)) $ error "Internal Error: Language.PDC.Interpreter.EvalRepr.pattern2Branch: Evaluated branch is a Leaf"

