{-# LANGUAGE RecordWildCards
           , TypeSynonymInstances
           , FlexibleInstances
           , ViewPatterns
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
import qualified GHC.Stack        as GHC

trace a b = b -- Debug.trace a b
trace2 a b = b -- Debug.trace a b
trace3 a = a -- Debug.trace (show a) a

--mergeTrace = Debug.trace
mergeTrace a b = b
--callTrace = Debug.trace
callTrace a b = b



-- Pretty
-- ----------------------------------------------------------------------------

prettyNode :: Node -> String
prettyNode (Node _ l) = " { " ++ (concat (map (\(m, n) -> {-(prettyPDCRulePattern (toRulePattern m))-} (prettyEdge m) ++ " =>" ++ (prettyNode n)) l)) ++ " } "
prettyNode Leaf = "LF"

getPrettyNodes :: Int -> Node -> [Pretty.Tree Edge]
getPrettyNodes 0 _ = []
getPrettyNodes _ Leaf = []
getPrettyNodes d (Node {..}) = map to branches
  where
    to (m, n) = Pretty.Node m (getPrettyNodes (d-1) n)

prettyNodeMsgMap :: Pretty.Tree Edge -> Pretty.Tree String
prettyNodeMsgMap = fmap (pdcid . subj)
  where
    subj ((MsgEdgeE msg):_) = pdcMsgType msg
    subj _ = PDCId (error "EvalRepr.prettyNodeMsgMap") "action" LC

prettyNodeMap :: Pretty.Tree Edge -> Pretty.Tree String
prettyNodeMap = fmap prettyEdge --(prettyPDCRulePattern . toRulePattern)

prettyEdge :: Edge -> String
prettyEdge = show . map prettyEdgeEntry
prettyEdgeEntry :: EdgeEntry -> String
prettyEdgeEntry (MsgEdgeE e) = prettyPDCRulePattern (toRulePattern e)
prettyEdgeEntry (ScopeEdgeE ScopeClose) = "scope-close"
prettyEdgeEntry (ScopeEdgeE (ScopeOpen e)) = "scope-open{" ++ (concat $ map ((++ ",") . lcid . pdcVarTypeBindingVarName) e) ++ "}"
prettyEdgeEntry (ScopeEdgeE ScopeThisBack) = "scope-this-back"
prettyEdgeEntry (ScopeEdgeE ScopeTop) = "scope-top"
prettyEdgeEntry (ActEdgeE _) = "action"


rootPrettyNode :: [Pretty.Tree String] -> Pretty.Tree String
rootPrettyNode = Pretty.Node "ROOT"

prettyNodeTreeToString :: Int -> (Pretty.Tree Edge -> Pretty.Tree String) -> Node -> String
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
    putStrLn $ {-prettyPDCRulePattern $ toRulePattern-} prettyEdge $ fst $ b
  str <- getLine
  case (words str) of
    ["next"]   -> next
    ["n"]      -> next
    ["prev"]   -> prev
    ["p"]      -> prev
    ["select"] -> select
    ["s"]      -> select
    ["tree"]   -> do
      printNodeTree 6 o
      cliNode' s o
    ["tree", fn]   -> do
      writeFileNodeTree fn 6 o
      cliNode' s o
    []         -> select
    _          -> do
        putStrLn $ "usage: next, prev, select"
        cliNode' s o
    where
      next   = cliNode' (s+1) o
      prev   = cliNode' (s-1) o
      select = cliNode' 0 $ snd $ fst $ fromJust $ find (\ (b, s') -> s == s' ) $ zip brs [0..]









-- Repr
-- ----------------------------------------------------------------------------

data Node
  = Node
    { pattern  :: PDCRulePattern
    , branches :: [(Edge, Node)]
    }
  | Leaf
  deriving (Eq, Show)

type Edge = [EdgeEntry]

data EdgeEntry
  = MsgEdgeE PDCMsgP
  | ActEdgeE PDCAttrContent
  | ScopeEdgeE ScopeAction
  deriving (Eq, Show)

data ScopeAction
  = ScopeClose
  | ScopeOpen [PDCVarTypeBinding]
  | ScopeThisBack
  | ScopeTop
  deriving (Eq, Show)

instance ToRulePattern Edge where
    toRulePattern e = Debug.trace ("    toRulePattern: " ++ (show $ map prettyEdgeEntry e)) (toRulePatternEdge e)

toRulePatternEdge ((MsgEdgeE a):_) = toRulePattern a
toRulePatternEdge (_:es) = toRulePatternEdge es
--    toRulePattern ((ActEdgeE a):_) = toRulePattern a
toRulePatternEdge _ = error $ "Language.PDC.Interpreter.EvalRepr.[ToRulePattern Edge]toRulePattern: non msg pattern in edge"

isMsgEdge :: EdgeEntry -> Bool
isMsgEdge (MsgEdgeE _) = True
isMsgEdge _ = False


-- AST to EvalRepr
-- ----------------------------------------------------------------------------

data Trans
  = PatternT PDCRulePattern
  | ScopeT ScopeAction
  | AttrContextT PDCAttrContent
  | EndCallT
  deriving (Eq)

trans2EE :: String -> Trans -> EdgeEntry
trans2EE _ (ScopeT sa) = ScopeEdgeE sa
trans2EE _ (AttrContextT ac) = ActEdgeE ac
trans2EE c (PatternT _) = error $ "Language.PDC.Interpreter.EvalRepr.trans2EE: PatternT (" ++ c ++")"
trans2EE c (EndCallT) = error $ "Language.PDC.Interpreter.EvalRepr.trans2EE: EndCallT (" ++ c ++")"


ast2node :: PDCModule -> PDCRulePattern -> Node
ast2node mod p = ast2node' mod [PatternT p]

--workaraoundCallEndCorrigator :: Node -> Node

-- intrace str pre tl p f = Debug.trace (str++":\n  pre: | " ++ (show $ map prettyTrans pre) ++ "\n   tl: | " ++ (show $ map prettyTrans tl) ++ "\n  pat: | " ++ prettyPDCRulePattern (toRulePattern p) ++ "\n------END") f
intrace _ _ _ _ f = f

data TransConf
  = TransConf
    { tl :: [Trans]
    , pre :: [Trans]
    , last :: [Trans]
    }

emptyTransConf = TransConf [] [] []

ast2node' :: PDCModule -> [Trans] -> Node
ast2node' _ [] = Leaf
--ast2node' mod ((PatternT o@(PDCMsgPattern p)):tl) =    trace ("ast2node'.msg:    " ++ (prettyPDCRulePattern o)) $ msg2node mod tl o p
ast2node' mod (tracedSplitOnPattern "split.msg"           -> (pre, Just o@(PDCMsgPattern p), tl)) =      trace ("ast2node'.msg:    " ++ (prettyPDCRulePattern o)) $ intrace "MSG" pre tl p $ msg2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.seq"           -> (pre, Just o@(PDCSeqPattern p), tl)) =      trace ("ast2node'.seq:    " ++ (prettyPDCRulePattern o)) $ intrace "SEQ" pre tl p $ seq2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.unique"        -> (pre, Just o@(PDCUnSeqPattern p), tl)) =    trace ("ast2node'.unique")                               $ intrace "UNS" pre tl p $ unseq2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.one-of"        -> (pre, Just o@(PDCOneOfPattern p), tl)) =    trace ("ast2node'.oneof")                                $ intrace "ONE" pre tl p $ oneof2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.many-of-lead"  -> (pre, Just o@(PDCManyofPattern p), [])) =   trace ("ast2node'.manyofLeaf")                           $ intrace "MANY-LEAF" pre [] p $ Leaf
ast2node' mod (tracedSplitOnPattern "split.many-of"       -> (pre, Just o@(PDCManyofPattern p), tl)) =   trace ("ast2node'.manyof: " ++ (prettyPDCRulePattern o)) $ intrace "MANY" pre tl p $ manyof2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.merge"         -> (pre, Just o@(PDCMergePattern p), tl)) =    trace ("ast2node'.merge")                                $ intrace "MERGE" pre tl p $ merge2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.optional-leaf" -> (pre, Just o@(PDCOptionalPattern p), [])) = trace ("ast2node'.optionalLeaf")                         $ intrace "OPT-LEAF" pre [] p $ Leaf
ast2node' mod (tracedSplitOnPattern "split.optional"      -> (pre, Just o@(PDCOptionalPattern p), tl)) = trace ("ast2node'.optional")                             $ intrace "OPT" pre tl p $ optional2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.more-of"       -> (pre, Just o@(PDCMoreOfPattern p), tl)) =   trace ("ast2node'.moreof")                               $ intrace "MORE" pre tl p $ moreof2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.call"          -> (pre, Just o@(PDCCallPattern p), tl)) =     trace ("ast2node'.call")                                 $ intrace "CALL" pre tl p $ call2node mod tl o pre p
ast2node' mod (tracedSplitOnPattern "split.leaf"          -> (pre, Nothing, [])) = Node undefined [(map (trans2EE "ast2node") $ Debug.trace "---- 1" pre, Leaf)]
ast2node' mod (tracedSplitOnPattern "split.error"         -> (pre, Nothing, tl)) = error $ "Language.PDC.Interpreter.EvalRepr.ast2node': Nothing and non empty tl list"
--ast2node' mod (PDCScopeAction PDCScopeClose:tl) =             trace "ast2node'.scopeOut" $ ast2node' mod tl -- top scope end
--ast2node' mod (PDCScopeAction PDCScopeOpen:tl) =             trace "ast2node'.scopeOut" $ ast2node' mod tl -- top scope end
--ast2node' mod (PDCScopeAction PDCScopeThisBack:tl) =             trace "ast2node'.scopeOut" $ ast2node' mod tl -- top scope end

prettyTrans :: Trans -> String
prettyTrans (PatternT p) = "pattern{" ++ (prettyPDCRulePattern p) ++ "}"
prettyTrans (ScopeT ScopeClose) = "scope-close"
prettyTrans (ScopeT (ScopeOpen e)) = "scope-open{" ++ "TODO" {-(pdcid $ getRuleName e)-} ++ "}"
prettyTrans (ScopeT ScopeThisBack) = "scope-this-back"
prettyTrans (ScopeT ScopeTop) = "scope-top"
prettyTrans (AttrContextT _) = "attr-ctx"
prettyTrans (EndCallT) = "end-call"

--splitTrace = Debug.trace
splitTrace _ = id
tracedSplitOnPattern str l = let (a,b,c) = splitOnPattern l in splitTrace (
                                                                     str
                                                                  ++ "\n        "
                                                                  ++ (show $ map prettyTrans l)
                                                                  ++ "\n        pre     |" 
                                                                  ++ (show $ map prettyTrans a)
                                                                  ++ "\n        pattern |"
                                                                  ++ (maybe "empty" prettyPDCRulePattern b)
                                                                  ++ "\n        post    |"
                                                                  ++ (show $ map prettyTrans c)
                                                                  ++ "\n-------------------------------------------------------"
                                                                  ) (a,b,c)

splitOnPattern :: [Trans] -> ([Trans], Maybe PDCRulePattern, [Trans])
splitOnPattern ls = splitOnPattern' ([], ls)
  where
    splitOnPattern' :: ([Trans], [Trans]) -> ([Trans], Maybe PDCRulePattern, [Trans])
    splitOnPattern' (pre, []) = (pre, Nothing, []) -- error $ "Language.PDC.Interpreter.EvalRepr.splitOnPattern: no pattern"
    splitOnPattern' (pre, (PatternT p):trss) = (pre, Just p, trss)
    splitOnPattern' (pre, trs:trss) = splitOnPattern' (pre ++ [trs], trss)

pattern2Branch :: PDCModule -> [Trans] -> [Trans] -> PDCRulePattern -> Maybe [(Edge, Node)]
pattern2Branch mod tl pre p = case (ast2node' mod (pre ++ ((PatternT p):tl))) of
  (Node {..}) -> Just branches
  Leaf -> Nothing


-- msg2nodeTrace = Debug.trace
msg2nodeTrace _ = id

msg2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCMsgP -> Node
msg2node mod tl o pre p = Node o [((map (trans2EE "msg2node.pre") $ msg2nodeTrace (" ##     pre | " ++ (show $ map prettyTrans pre))  $ filter (/=EndCallT)  {- workaraound fitler -}$ pre) 
    ++ list
    ++ (map (trans2EE "msg2node.endcall") $ msg2nodeTrace (" ## endcall | " ++ (show $ map prettyTrans endcall)) endcall), remeaning tl')]
  where
    list
      | Just ac <- pdcMsgContent p = [ ScopeEdgeE (ScopeOpen selectors), MsgEdgeE p, ScopeEdgeE ScopeThisBack, ActEdgeE (inlineAC ac), ScopeEdgeE ScopeClose ]
      | otherwise                  = [ ScopeEdgeE (ScopeOpen selectors), MsgEdgeE p,                                                   ScopeEdgeE ScopeClose ]
    remeaning tl = ast2node' mod tl
    (endcall, tl') = checkEndCall tl
    checkEndCall [] = ([], [])
    checkEndCall (EndCallT:tl) = let (ptl, tl') = checkEndCall tl in (ptl, tl') -- ([],tl)
    checkEndCall (o@((PatternT phantom):tl)) = ([],o)
    --case ast2node' mod [PatternT phantom, PatternT (PDCMsgPattern phantomMsgP)] of
    --  (Node _ edge) -> if elem  let (ptl, tl') = checkEndCall tl in (ptl, (PatternT phantom):tl')
    -- ([],o)
    checkEndCall (t:tl) = let (ptl, tl') = checkEndCall tl in (t:ptl, tl')
    phantomId str = PDCId undefined str UC
    phantomMsgP = PDCMsgP undefined (phantomId "@A") (phantomId "@B") (phantomId "@MSG") Nothing
    selectors = maybe [] id $ do
        msgAttrTypeEntry <- findMsgAttrTypeEntry (pdcMsgType p) mod
        msgDataTypeEntry <- findRecordDataTypeEntry (pdcMsgTypeType msgAttrTypeEntry) mod
        return (pdcRecordEntries msgDataTypeEntry)
    inlineAC :: PDCAttrContent -> PDCAttrContent
    inlineAC (PDCAttrContentActionCall acc) = maybe err PDCAttrContentInlineAction $ do
        a <- findActionEntry name mod
        return (pdcActionBody a)
      where
        name = lcid (pdcActionCallName acc)
        err = error $ "Language.PDC.Interpreter.EvalRepr.msg2node.inlineAC: not found action " ++ name
    inlineAC acb = acb
{-
  = Node
    { pattern  :: PDCRulePattern
    , branches :: [(Edge, Node)]
    }
  | Leaf
    remeaning tl = let (pre, p, ftl) = splitOnPattern tl 
                       (endcall', tl'') = checkEndCall pre
                       p' = case p of
                           Nothing -> []
                           (Just p'') -> [PatternT p'']
                    in ast2node' mod (tl'' ++ p' ++ ftl)

-}


seq2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCSeqP -> Node
seq2node mod tl o pre (PDCSeqP {..}) = ast2node' mod (pre ++ (map PatternT pdcRulePatternsSeq) ++ tl)
{-
  | [] <- pdcRulePatternsSeq
    = ast2node' mod (pre ++ tl)
  | op@(p:ps) <- pdcRulePatternsSeq
    = ast2node' mod (pre ++ (map PatternT op) ++ tl)
      -- patternWithCondition mod (pre ++ (map PatternT ps)++tl) p (Node o)
-}

patternWithCondition :: PDCModule -> [Trans] -> PDCRulePattern -> ([(Edge, Node)] -> Node) -> Node
patternWithCondition mod tl p f = case pattern2Branch mod tl [] p of
    Nothing -> Leaf
    Just brs -> f brs

unseq2node ::  PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCUnSeqP -> Node
unseq2node mod tl o pre (PDCUnSeqP {..})
  | [] <- pdcRulePatternsUnSeq
    = ast2node' mod (pre ++ tl)
  | otherwise = Node o (concat $ catMaybes $ map (pattern2Branch mod tl pre) perms)
    where
      perms = map seq (getOthers pdcRulePatternsUnSeq)
      seq (l, others) = PDCSeqPattern PDCSeqP { sourceInfoSeq = sourceInfoUnSeq, pdcRulePatternsSeq = 
        [l, PDCUnSeqPattern PDCUnSeqP { sourceInfoUnSeq = sourceInfoUnSeq, pdcRulePatternsUnSeq = others }] }


oneof2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCOneOfP -> Node
oneof2node mod tl o pre (PDCOneOfP {..})
  = Node o (concat $ catMaybes $ map (pattern2Branch mod tl pre) pdcRulePatternsOneOf)

prettyBranch :: (Edge, Node) -> String
prettyBranch = prettyEdge {-prettyPDCRulePattern . toRulePattern-} . fst

traceb brs = brs
--traceb brs = trace2 ("brs  " ++ (show $ map prettyBranch brs)) brs
tracemb = id
--tracemb Nothing = trace2 "Nothing" Nothing
--tracemb (Just brs) = trace2 ("null " ++ (show $ map prettyBranch brs)) (Just brs)

manyof2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCManyOfP -> Node
manyof2node mod tl@((PatternT htl):ttl) o pre m@(PDCManyOfP {..})
    | Nothing <- nullMatch = Leaf
    | otherwise = Node o (trace2 (show $ map prettyBranch (fromJust nullMatch ++ brs)) $ fromJust nullMatch ++ brs)
  where
      get1 [] = []
      get1 [x] = [x]
      get1 (x:_) = [x]
      nullMatch = tracemb $ {- Just [] --} pattern2Branch mod ({-(trace2 ((++) "ttl: " $ show $ map prettyPDCRulePattern ttl)-} ttl){-ttl-}
                                                              (pre)
                                                              ({-(trace2 ((++) "htl: " $ prettyPDCRulePattern htl)-} htl)
      --brs = traceb $ concat $ catMaybes $ map (pattern2Branch mod ((PDCManyofPattern m):tl)) pdcRulePatternsManyOf
      brs = traceb $ concat $ catMaybes $ map (pattern2Branch mod ((PatternT (PDCManyofPattern m)):tl) pre) pdcRulePatternsManyOf
manyof2node mod tl@(_:ttl) o _ m@(PDCManyOfP {..}) = error $ "Language.PDC.Interpreter.EvalRepr.manyof2node: not PatternT in head"
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


moreof2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCMoreOfP -> Node
moreof2node mod tl o pre m@(PDCMoreOfP {..})
    -- | Nothing <- nullMatch = Leaf
    -- | otherwise            = Node o (brs)
    = Node o (brs)
  where
    --brs = concat $ catMaybes $ map (pattern2Branch mod ((PDCMoreOfPattern m):tl)) pdcRulePatternsMoreOf
      -- nullMatch = pattern2Branch mod ttl htl
      brs = concat $ catMaybes $ map 
          (pattern2Branch mod (PatternT ((PDCManyofPattern (PDCManyOfP
            { pdcRulePatternsManyOf = pdcRulePatternsMoreOf
            , sourceInfoManyOf = sourceInfoMoreOf
            }))):tl) pre) 
          pdcRulePatternsMoreOf
      --                        This is very ugly I know.. ^
      -- brs = concat $ catMaybes $ map (pattern2Branch ((PDCMoreOfPattern m):tl)) pdcRulePatternsMoreOf

optional2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCOptionalP -> Node
optional2node mod tl@(splitOnPattern -> (pre', Just htl, ttl)) o pre m@(PDCOptionalP {..})
  = Node o (concat $ catMaybes $ [pattern2Branch mod tl pre pdcRulePatternOptional, pattern2Branch mod ttl pre' htl])
  -- map (pattern2Branch mod tl pre) pdcRulePatternsOneOf)
optional2node mod tl@(htl:ttl) o pre m@(PDCOptionalP {..}) = error $ "Language.PDC.Interpreter.EvalRepr.optional2node: not PatternT in head"

{-
(PDCOneOfP {..})
  = Node o (concat $ catMaybes $ map (pattern2Branch mod tl pre) pdcRulePatternsOneOf)
-}

      {-      
optional2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCOptionalP -> Node
optional2node mod tl@(splitOnPattern -> (pre', Just htl, ttl)) o pre m@(PDCOptionalP {..})
    | Nothing <- nullMatch = Leaf
    | otherwise            = Node o (fromJust nullMatch ++ brs)
  where
      nullMatch = pattern2Branch mod ttl (pre ++ pre') htl
      brs = concat $ catMaybes [pattern2Branch mod tl pre pdcRulePatternOptional]
optional2node mod tl@(htl:ttl) o pre m@(PDCOptionalP {..}) = error $ "Language.PDC.Interpreter.EvalRepr.optional2node: not PatternT in head"
-}

{-
data Node
  = Node
    { pattern  :: PDCRulePattern
    , branches :: [(Edge, Node)]
    }
  | Leaf
  deriving (Eq, Show)
data PDCMergeP
  = PDCMergeP
    { sourceInfoMerge    :: SourceInfo
    , pdcRulePatternsMerge :: [PDCRulePattern]
    }
pattern2Branch :: PDCModule -> [Trans] -> [Trans] -> PDCRulePattern -> Maybe [(Edge, Node)]
ast2node' :: PDCModule -> [Trans] -> Node
ast2node :: PDCModule -> PDCRulePattern -> Node
-}


type MNode = (Edge, Node)
type MAlt = [MNode]


-- [     thread1      ,    thread2      , ...,     thread3       ]
--   [alt1, ..., altn] [alt1, ..., altn]  ... [alt1, ..., altn]

merge2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCMergeP -> Node
merge2node mod tl o pre (PDCMergeP {..})
--    = Node o (concat (map getBrs $ combineAlts brss))
--  where
--    brss :: (MAlt, MAlt)
--    brss = let [a,b] = map branches' (map (\ p -> ast2node' mod (pre ++ [PatternT p])) pdcRulePatternsMerge) in (a,b)
--    branches' Leaf = []
--    branches' n = branches n

--    combineAlts :: (MAlt, MAlt) -> [(MNode, MNode)]
--    combineAlts (a, b) = [ (alta, altb) | alta <- a, altb <- b ]
--    getBrs :: (MNode, MNode) -> [MNode]
--    getBrs (a, b) = mergeBr a b

    

    = Node o $ concat $ map mergePath (anypath brss)
  where
    brss :: [[(Edge, Node)]]
    brss = catMaybes $ map (pattern2Branch mod [] pre) pdcRulePatternsMerge

    end = ast2node' mod tl
  
  
    mergePath :: [(Edge, Node)] -> [(Edge, Node)]
    mergePath [] = []
    mergePath [p] = [p]
    mergePath (p:op) = concat $ map (mergeBr p) (mergePath op)
  

    mergeBr :: (Edge, Node) -> (Edge, Node) -> [(Edge, Node)]
    mergeBr (m1, Leaf) (m2, Leaf) = [(m1, Node (toRulePattern m2) [(m2, end)]), (m2, Node (toRulePattern m1) [(m1, end)])]
    
    mergeBr (m1, Leaf) (m2, n2) =
        ((m1, Node (pattern n2) [(m2, n2)]))
       :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )
    
    mergeBr (m2, n2) (m1, Leaf) =
        ((m1, Node (pattern n2) [(m2, n2)]))
       :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )
    
    mergeBr (m1, n1) (m2, n2)
        = concat $
            [ [ (m1, Node (pattern n1) (mergeBr b1 ((\(a,b) -> (m2, Node (toRulePattern a) [(a,b)])) b2) ))
              , (m2, Node (pattern n2) (mergeBr b2 ((\(a,b) -> (m1, Node (toRulePattern a) [(a,b)])) b1) ))
              ]
            | b1 <- branches n1, b2 <- branches n2
            ]

{-}
    = Node o (map mergeStep (flatMerged (preMerged brss))) -- (foldr folder (head brss) (tail brss))
  where
    folder a b = map mergeStep (flatMerged (preMerged [a,b]))
    brss :: [MAlt]
    brss = map branches (map (ast2node mod) pdcRulePatternsMerge)
    preMerged :: [MAlt] -> [(MAlt, [MAlt])]
    preMerged brss = getOthers brss
--    flatMerged :: [(MNode, [MNode])]
--    flatMerged = [ (scheduled, remeaning) | (alt, altl) <- preMerged
--                                          , scheduled   <- alt
--                                          , remeaning   <- anypath altl
--                                          ]
    
    mergeStep :: ((Edge, Node), [(Edge, Node)]) -> (Edge, Node)
    mergeStep ((e, Leaf), []) = (e, Leaf)
    mergeStep ((e, Leaf), nl) = (e, Node (pattern (snd (head nl))) nl)
    mergeStep ((e, n),    nl) = (e, Node (pattern n) (branches n ++ nl))
    -- mergeStep ((e, n),    nl) = (e, Node (pattern n) ) -- (map mergeStep (flatMerged (branches n ++ nl))))
-}

    
    {-
    = Node o (concat (map merge preMerged))
  where
    nodes :: [Node]
    nodes = map (ast2node mod) pdcRulePatternsMerge
    preMerged :: [(Node, [Node])]
    preMerged = getOthers nodes
    merge :: (Node, [Node]) -> [(Edge, Node)]
    merge (n, nl) = map (mergeStep nl) (branches n)
    mergeStep :: [Node] -> (Edge, Node) -> (Edge, Node)
    mergeStep [] (e, Leaf) = (e, Leaf)
    mergeStep nl (e, Leaf) = (e, Node (pattern (head nl)) (concat (map branches' nl))  )
    mergeStep nl (e, n) = (e, Node (pattern n) (branches n ++ (concat (map branches' nl)) ) )
    branches' Leaf = []
    branches' n = branches n
-}
{-
merge2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCMergeP -> Node
merge2node mod tl o pre (PDCMergeP {..})
  = Node o $ concat $ map (mergeTrace "call: mergePath" mergePath) (anypath brss)
  where
    brss :: [[(Edge, Node)]]
    brss = catMaybes $ map (pattern2Branch mod tl pre) pdcRulePatternsMerge


mergePath :: [(Edge, Node)] -> [(Edge, Node)]
mergePath [] = []
mergePath [p] = [p]
mergePath (p:op) = concat $ map (mergeTrace "call: mergeBr" (mergeBr p)) (mergePath op)



tm = prettyPDCRulePattern . toRulePattern
mergeTreeDepth = 4


mergeBr :: (Edge, Node) -> (Edge, Node) -> [(Edge, Node)]
mergeBr (m1, Leaf) (m2, Leaf) = mergeTrace ("== LeafLeaf\n" ++ tm m1 ++ "\n" ++ tm m2) $ [(m1, Node (toRulePattern m2) [(m2, Leaf)]), (m2, Node (toRulePattern m1) [(m1, Leaf)])]

mergeBr (m1, Leaf) (m2, n2) = mergeTrace ("== LeafNode\n" ++ tm m1 ++ "\n" ++ tm m2 ++ " ->\n" ++ showNodeTreeMsg mergeTreeDepth n2) $
    ((m1, Node (pattern n2) [(m2, n2)]))
   :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )

mergeBr (m2, n2) (m1, Leaf) = mergeTrace ("== NodeLeaf " ++ tm m2 ++ " ->\n" ++ showNodeTreeMsg mergeTreeDepth n2 ++ "\n" ++ tm m1) $
    ((m1, Node (pattern n2) [(m2, n2)]))
   :( map (\t-> (m2, Node (pattern n2) t)) $ map (mergeBr (m1,Leaf)) (branches n2) )

mergeBr (m1, n1) (m2, n2)
    =  mergeTrace ("== NodeNode\n" ++ tm m1 ++ " ->\n" ++ showNodeTreeMsg mergeTreeDepth n1 ++ "\n" ++ tm m2 ++ " ->\n" ++ showNodeTreeMsg mergeTreeDepth n2) $ concat $
        [ [ (m1, Node (pattern n1) (mergeBr b1 ((\(a,b) -> (m2, Node (toRulePattern a) [(a,b)])) b2) ))
          , (m2, Node (pattern n2) (mergeBr b2 ((\(a,b) -> (m1, Node (toRulePattern a) [(a,b)])) b1) ))
          ]
        | b1 <- branches n1, b2 <- branches n2
        ]
-}

call2node :: PDCModule -> [Trans] -> PDCRulePattern -> [Trans] -> PDCCallP -> Node
call2node mod tl o pre cp@(PDCCallP {..})
    | Just ac <- pdcCallContent
    = ast2node' newMod
      (pre ++ [ScopeT (ScopeOpen selectors)] ++ (maybe [] (\ a -> [ScopeT ScopeThisBack, AttrContextT (inlineAC a), ScopeT ScopeTop]) pdcPreCallContent) ++
      ( (PatternT (pdcRulePattern (callTrace "instanceRuleEntry" $ entry)))
      : (ScopeT ScopeThisBack)
      : (AttrContextT (inlineAC ac))
      : (ScopeT ScopeClose)
      : EndCallT
      : tl
      ))
    | otherwise = ast2node' newMod
      (pre ++
      ( (ScopeT (ScopeOpen selectors))
      : (PatternT (pdcRulePattern (callTrace "instanceRuleEntry" $ entry)))
      : (ScopeT ScopeClose)
      : EndCallT
      : tl
      ))
  where
    newMod = mod -- { pdcCallUnivSeqNum = newUniv }
    (entry, newUniv) = instanceRuleEntry (pdcCallUnivSeqNum mod) cp ruleEntry
    Just ruleEntry = findRuleEntry pdcRuleId mod
    id = pdcRuleAttr $ pdcRuleType $ pdcRuleEntryHeader ruleEntry
    Just selectors = if ucid id == "NullAttr" then Just [] else fmap pdcRecordEntries $ findRecordDataTypeEntry id mod
    inlineAC :: PDCAttrContent -> PDCAttrContent
    inlineAC (PDCAttrContentActionCall acc) = maybe err PDCAttrContentInlineAction $ do
        a <- findActionEntry name mod
        return (pdcActionBody a)
      where
        name = lcid (pdcActionCallName acc)
        err = error $ "Language.PDC.Interpreter.EvalRepr.call2node.inlineAC: not found action " ++ name
    inlineAC acb = acb

    {-
    selectors = maybe [] id $ do
      msgAttrTypeEntry <- findMsgAttrTypeEntry (pdcMsgType p) mod
      msgDataTypeEntry <- findRecordDataTypeEntry (pdcMsgTypeType msgAttrTypeEntry) mod
      return (pdcRecordEntries msgDataTypeEntry)
-}


