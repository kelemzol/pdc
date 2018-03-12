{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Interpreter.EvalRepr where

import Data.Maybe
import Data.List (find)
-- import Prelude hiding (concat)
import Control.Monad

import Language.PDC.Repr
import Language.PDC.Interpreter.Utils

import qualified Data.Tree        as Pretty
import qualified Data.Tree.Pretty as Pretty
import qualified Debug.Trace      as Debug

trace a b = b -- Debug.trace a b
trace2 a b = b -- Debug.trace a b
trace3 a = a -- Debug.trace (show a) a

--mergeTrace = Debug.trace
mergeTrace a b = b
--callTrace = Debug.trace
callTrace a b = b



-- Repr
-- ----------------------------------------------------------------------------

data Node
  = Node
    { pattern  :: PDCRulePattern
    , branches :: [(PDCMsgP, Node)]
    }
  | Leaf
  deriving (Eq, Show)


-- Pretty
-- ----------------------------------------------------------------------------

prettyNode :: Node -> String
prettyNode (Node _ l) = " { " ++ (concat (map (\(m, n) -> (prettyPDCRulePattern (PDCMsgPattern m)) ++ " =>" ++ (prettyNode n)) l)) ++ " } "
prettyNode Leaf = "LF"

getPrettyNodes :: Int -> Node -> [Pretty.Tree PDCMsgP]
getPrettyNodes 0 _ = []
getPrettyNodes _ Leaf = []
getPrettyNodes d (Node {..}) = map to branches
  where
    to (m, n) = Pretty.Node m (getPrettyNodes (d-1) n)

prettyNodeMsgMap :: Pretty.Tree PDCMsgP -> Pretty.Tree String
prettyNodeMsgMap = fmap (pdcid . pdcMsgType)

prettyNodeMap :: Pretty.Tree PDCMsgP -> Pretty.Tree String
prettyNodeMap = fmap (prettyPDCRulePattern . PDCMsgPattern)

rootPrettyNode :: [Pretty.Tree String] -> Pretty.Tree String
rootPrettyNode = Pretty.Node "ROOT"

prettyNodeTreeToString :: Int -> (Pretty.Tree PDCMsgP -> Pretty.Tree String) -> Node -> String
prettyNodeTreeToString d mod = Pretty.drawVerticalTree . rootPrettyNode . map mod . getPrettyNodes d

showNodeTreeMsg :: Int -> Node -> String
showNodeTreeMsg d = prettyNodeTreeToString d prettyNodeMap

printNodeTreeMsg :: Int -> Node -> IO ()
printNodeTreeMsg d = putStrLn . prettyNodeTreeToString d prettyNodeMsgMap

printNodeTree :: Int -> Node -> IO ()
printNodeTree d = putStrLn . prettyNodeTreeToString d prettyNodeMap

writeFileNodeTree :: String -> Int -> Node -> IO ()
writeFileNodeTree fn d = writeFile fn . prettyNodeTreeToString d prettyNodeMap

-- Node CLI
-- ----------------------------------------------------------------------------

cliNode :: Node -> IO Node
cliNode = cliNode' 0

cliNode' :: Int -> Node -> IO Node
cliNode' _ Leaf = putStrLn "Leaf" >> return Leaf
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
-- ----------------------------------------------------------------------------

ast2node :: PDCModule -> PDCRulePattern -> Node
ast2node mod p = ast2node' mod [p]

ast2node' :: PDCModule -> [PDCRulePattern] -> Node
ast2node' _ [] = Leaf
ast2node' mod (o@(PDCMsgPattern p):tl) =    trace ("ast2node'.msg:    " ++ (prettyPDCRulePattern o)) $ msg2node mod tl o p
ast2node' mod (o@(PDCSeqPattern p):tl) =    trace ("ast2node'.seq:    " ++ (prettyPDCRulePattern o)) $ seq2node mod tl o p
ast2node' mod (o@(PDCUnSeqPattern p):tl) =  trace "ast2node'.unique" $ unseq2node mod tl o p
ast2node' mod (o@(PDCOneOfPattern p):tl) =  trace "ast2node'.oneof" $ oneof2node mod tl o p
ast2node' mod (o@(PDCManyofPattern p):[]) = trace "ast2node'.manyofLeaf" $ Leaf
ast2node' mod (o@(PDCManyofPattern p):tl) = trace ("ast2node'.manyof: " ++ (prettyPDCRulePattern o)) $ manyof2node mod tl o p
ast2node' mod (o@(PDCMergePattern p):tl) =  trace "ast2node'.merge" $ merge2node mod tl o p
ast2node' mod (o@(PDCOptionalPattern p):[]) = trace "ast2node'.optionalLeaf" $ Leaf
ast2node' mod (o@(PDCOptionalPattern p):tl) = trace "ast2node'.optional" $ optional2node mod tl o p
ast2node' mod (o@(PDCMoreOfPattern p):tl) = trace "ast2node'.moreof" $ moreof2node mod tl o p
ast2node' mod (o@(PDCCallPattern p):tl) =   trace "ast2node'.call" $ call2node mod tl o p


pattern2Branch :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> Maybe [(PDCMsgP, Node)]
pattern2Branch mod tl p = case (ast2node' mod (p:tl)) of
  (Node {..}) -> Just branches
  Leaf -> Nothing


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

prettyBranch :: (PDCMsgP, Node) -> String
prettyBranch = prettyPDCRulePattern . PDCMsgPattern . fst

traceb brs = brs
--traceb brs = trace2 ("brs  " ++ (show $ map prettyBranch brs)) brs
tracemb = id
--tracemb Nothing = trace2 "Nothing" Nothing
--tracemb (Just brs) = trace2 ("null " ++ (show $ map prettyBranch brs)) (Just brs)

manyof2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCManyOfP -> Node
manyof2node mod tl@(htl:ttl) o m@(PDCManyOfP {..})
    | Nothing <- nullMatch = Leaf
    | otherwise = Node o (trace2 (show $ map prettyBranch (fromJust nullMatch ++ brs)) $ fromJust nullMatch ++ brs)
  where
      get1 [] = []
      get1 [x] = [x]
      get1 (x:_) = [x]
      nullMatch = tracemb $ {- Just [] --} pattern2Branch mod (trace2 ((++) "ttl: " $ show $ map prettyPDCRulePattern ttl) ttl){-ttl-}
                                                                          (trace2 ((++) "htl: " $ prettyPDCRulePattern htl) htl)
      --brs = traceb $ concat $ catMaybes $ map (pattern2Branch mod ((PDCManyofPattern m):tl)) pdcRulePatternsManyOf
      brs = traceb $ concat $ catMaybes $ map (pattern2Branch mod ((PDCManyofPattern m):tl)) pdcRulePatternsManyOf

      -- brs =    concat $ catMaybes $ map (pattern2Branch mod tl) pdcRulePatternsManyOf
      -- PDCManyofPattern m
    -- | otherwise            = Node o (maybe [] id nullMatch ++ brs)
    -- | otherwise            = trace2 ("== tl:    " ++ show (map prettyPDCRulePattern tl)) $ trace2 ("== m:     " ++ prettyPDCRulePattern (PDCManyofPattern m)) $
    --    Node o (fromJust nullMatch ++ brs)


{-manyof2node mod tl o m@(PDCManyOfP {..}) = ast2node' mod (seq:tl)
  where
    seq = PDCSeqPattern (PDCSeqP sourceInfoManyOf [optional,manyof])
    optional = PDCOptionalPattern (PDCOptionalP sourceInfoManyOf oneof)
    oneof = PDCOneOfPattern (PDCOneOfP sourceInfoManyOf pdcRulePatternsManyOf)
    manyof = PDCManyofPattern m
-}


moreof2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCMoreOfP -> Node
moreof2node mod tl o m@(PDCMoreOfP {..})
    -- | Nothing <- nullMatch = Leaf
    -- | otherwise            = Node o (brs)
    = Node o (brs)
  where
    --brs = concat $ catMaybes $ map (pattern2Branch mod ((PDCMoreOfPattern m):tl)) pdcRulePatternsMoreOf
      -- nullMatch = pattern2Branch mod ttl htl
      brs = concat $ catMaybes $ map 
          (pattern2Branch mod ((PDCManyofPattern (PDCManyOfP {pdcRulePatternsManyOf = pdcRulePatternsMoreOf, sourceInfoManyOf = sourceInfoMoreOf})):tl)) 
          pdcRulePatternsMoreOf
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
  = Node o $ concat $ map (mergeTrace "call: mergePath" mergePath) (anypath brss)
  where
    brss :: [[(PDCMsgP, Node)]]
    brss = catMaybes $ map (pattern2Branch mod tl) pdcRulePatternsMerge


mergePath :: [(PDCMsgP, Node)] -> [(PDCMsgP, Node)]
mergePath [] = []
mergePath [p] = [p]
mergePath (p:op) = concat $ map (mergeTrace "call: mergeBr" (mergeBr p)) (mergePath op)





tm = prettyPDCRulePattern . PDCMsgPattern
mergeTreeDepth = 4


mergeBr :: (PDCMsgP, Node) -> (PDCMsgP, Node) -> [(PDCMsgP, Node)]
mergeBr (m1, Leaf) (m2, Leaf) = mergeTrace ("== LeafLeaf\n" ++ tm m1 ++ "\n" ++ tm m2) $ [(m1, Node (PDCMsgPattern m2) [(m2, Leaf)]), (m2, Node (PDCMsgPattern m1) [(m1, Leaf)])]

mergeBr (m1, Leaf) (m2, n2) = mergeTrace ("== LeafNode\n" ++ tm m1 ++ "\n" ++ tm m2 ++ " ->\n" ++ showNodeTreeMsg mergeTreeDepth n2) $
    ((m1, Node (pattern n2) [(m2, n2)]))
   :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )

mergeBr (m2, n2) (m1, Leaf) = mergeTrace ("== NodeLeaf " ++ tm m2 ++ " ->\n" ++ showNodeTreeMsg mergeTreeDepth n2 ++ "\n" ++ tm m1) $
    ((m1, Node (pattern n2) [(m2, n2)]))
   :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )

mergeBr (m1, n1) (m2, n2)
    =  mergeTrace ("== NodeNode\n" ++ tm m1 ++ " ->\n" ++ showNodeTreeMsg mergeTreeDepth n1 ++ "\n" ++ tm m2 ++ " ->\n" ++ showNodeTreeMsg mergeTreeDepth n2) $ concat $
        [ [ (m1, Node (pattern n1) (mergeBr b1 ((\(a,b) -> (m2, Node (PDCMsgPattern a) [(a,b)])) b2) ))
          , (m2, Node (pattern n2) (mergeBr b2 ((\(a,b) -> (m1, Node (PDCMsgPattern a) [(a,b)])) b1) ))
          ]
        | b1 <- branches n1, b2 <- branches n2
        ]



call2node :: PDCModule -> [PDCRulePattern] -> PDCRulePattern -> PDCCallP -> Node
call2node mod tl o cp@(PDCCallP {..}) = ast2node' mod ((pdcRulePattern (callTrace "instanceRuleEntry" $ instanceRuleEntry cp ruleEntry)):tl)
  where
    Just ruleEntry = findRuleEntry pdcRuleId mod
    

