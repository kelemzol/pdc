
{-# LANGUAGE RecordWildCards
           , ViewPatterns
           , TypeFamilies
           , FlexibleContexts
           #-}

module Language.PDC.Interpreter.Eval (evalNode, EvalNodeRes(..)) where

import Data.Maybe
import Data.Either
import Data.List

import Control.Monad

import Language.PDC.Repr
import Language.PDC.Interpreter.Env
import Language.PDC.Interpreter.Msg
import Language.PDC.Interpreter.EvalRepr
import Language.PDC.Interpreter.Msg
import Language.PDC.Interpreter.Env

import Debug.Trace


data EvalNodeRes
  = EvalNodeFail
    { failedPattern :: PDCRulePattern
    , failedMsg :: Maybe PDCMsgP
    , boundEnv :: BoundEnv
    }
  | EvalNodeSuccess
    { boundEnv :: BoundEnv
    }
  deriving (Eq, Show)

findSucces :: [EvalNodeRes] -> Maybe EvalNodeRes
findSucces [] = Nothing
findSucces ((EvalNodeFail {..}):es) = findSucces es
findSucces (e@(EvalNodeSuccess {..}):_) = Just e

evalNode :: Node -> [PDCMsgP] -> BoundEnv -> EvalNodeRes
evalNode Leaf _ benv = EvalNodeSuccess { boundEnv = benv }
evalNode Node {..} [] benv = EvalNodeFail { failedPattern = pattern, failedMsg = Nothing, boundEnv = benv }
evalNode Node {..} (m:ms) benv = case findSucces ress of
    Nothing -> head ress
    (Just succ) -> succ
  where
    ress = map (matcher benv m ms) branches

matcher :: BoundEnv -> PDCMsgP -> [PDCMsgP] -> (PDCMsgP, Node) -> EvalNodeRes
matcher benv msg msglist (pattern, node)
  | directMatch pattern msg = evalNode node msglist benv
  | partialMatch pattern msg
  , (Just benv') <- matchBounded benv pattern msg = evalNode node msglist benv'
  | otherwise = EvalNodeFail { failedPattern = PDCMsgPattern pattern, failedMsg = Just msg, boundEnv = benv }











{-
type Step c a = c a -> Either (PDCMsgP, PDCMsgP) (StepRes (c a))

type family StepRes a
type instance StepRes (Configuration a) = Configuration a
type instance StepRes (Branch a) = [Branch (ReturnType a)]

type family ReturnType a
type instance ReturnType PDCMsgP = PDCMsgP
type instance ReturnType PDCSeqP = PDCSeqP
type instance ReturnType PDCMergeP = PDCMergeP

type instance ReturnType PDCOneOfP = PDCRulePattern
type instance ReturnType PDCRulePattern = PDCRulePattern

type CStep a = Step Configuration a
type BStep a = PDCMsgP -> Step Branch a
-}

{-
class (Eq (StatusType e)) => Status (e :: * -> *) where
    isSuccess :: e a -> Bool
    isRunning :: e a -> Bool
    isFailed  :: e a -> Bool

    type StatusType e
    getStatus :: e a -> StatusType e
    
isSomeStatus :: (Status e) => [StatusType e] -> e a -> Bool
isSomeStatus es e = or $ map (\ a -> a == (getStatus e)) es

type family ReducedPatternType a
type instance ReducedPatternType PDCRulePattern = PDCRulePattern
type instance ReducedPatternType PDCMsgP = PDCMsgP
type instance ReducedPatternType PDCSeqP = PDCSeqP
type instance ReducedPatternType PDCOneOfP = PDCRulePattern
type instance ReducedPatternType PDCMergeP = PDCRulePattern

type BranchStep a = PDCMsgP -> Branch a -> [Branch (ReducedPatternType a)]

data BranchStatus
  = RunningBranch
  | SuccessBranch
  | FailedBranch
  deriving (Eq, Show)

data Branch a
  = Branch
    { boundEnv :: BoundEnv
    , pattern  :: a
    , status   :: BranchStatus
    }
  deriving (Eq, Show)

instance Status Branch where
    isSuccess = isSomeStatus [SuccessBranch] -- (==) Success . status
    isRunning = isSomeStatus [RunningBranch] -- (==) Running . status
    isFailed  = isSomeStatus [FailedBranch] -- (==) Failed . status

    type StatusType Branch = BranchStatus
    getStatus = status


data Configuration a
  = Configuration
    { branches :: [Branch a]
    , msglist  :: [PDCMsgP]
    , configStatus :: ConfigStatus
    }
  deriving (Eq, Show)

data ConfigStatus
  = RunningConfig
  | SuccessConfig
  | MsgNotEnough
  | FailedConfig
  deriving (Eq, Show)

instance Status Configuration where
    isSuccess = isSomeStatus [SuccessConfig]
    isRunning = isSomeStatus [RunningConfig]
    isFailed  = isSomeStatus [MsgNotEnough, FailedConfig]

    type StatusType Configuration = ConfigStatus
    getStatus = configStatus

prettyBranch :: Branch PDCRulePattern -> String
prettyBranch (Branch {..}) = "[branch " ++ (show status) ++ " " ++ (prettyPDCRulePattern pattern) ++ "]"

prettyConfiguration :: Configuration PDCRulePattern -> String
prettyConfiguration (Configuration {..}) = "[conf " ++ (show configStatus) ++ " " ++ (concat $ map prettyBranch branches)
    ++ "||" ++ (concat $ map (prettyPDCRulePattern . PDCMsgPattern) msglist) ++ "]"



eval :: PDCRuleE -> [PDCMsgP] -> Bool
eval PDCRuleE {..} msgs = getRes (configRunner conf)
  where
    getRes conf
      | isSuccess conf = True
      | isRunning conf = False
      | isFailed  conf = False
    conf = Configuration { branches = [Branch emptyBoundEnv pdcRulePattern RunningBranch]
                         , msglist = msgs
                         , configStatus = RunningConfig
                         }

configRunner :: Configuration PDCRulePattern -> Configuration PDCRulePattern
configRunner conf 
  | isRunning conf = let conf' = step $ trace ("state:"++(prettyConfiguration conf)) conf
                     in maybe (configRunner conf')
                              (const conf' {configStatus = SuccessConfig})
                              (findSuccesBranch conf')
  | otherwise      = conf

step :: Configuration PDCRulePattern -> Configuration PDCRulePattern
step conf@(Configuration {..})
  | [] <- msglist     = conf { configStatus = MsgNotEnough }
--  | [] <- branches    = conf { configStatus = FailedConfig }
  | (m:ms) <- msglist = let brss = forM (filterFailed branches) (stepRulePattern m)
                        in conf { msglist = ms
                                , branches = concat brss
                                , configStatus = if concat brss == [] then FailedConfig else configStatus
                                }

filterFailed :: [Branch a] -> [Branch a]
filterFailed = filter (not . isFailed)

findSuccesBranch :: Configuration a -> Maybe (Branch a)
findSuccesBranch = find isSuccess . branches

stepRulePattern :: BranchStep PDCRulePattern
stepRulePattern m conf = case pattern conf of
    (PDCMsgPattern msg) -> let brs = stepMsgPattern m conf { pattern = msg }
                           in map (\ b -> b { pattern = PDCMsgPattern (pattern b) }) brs
    (PDCSeqPattern seq) -> let brs = stepSeqPattern m conf { pattern = seq }
                           in map (\ b -> b { pattern = PDCSeqPattern (pattern b) }) brs
    (PDCOneOfPattern oneof) -> stepOneOfPattern m conf { pattern = oneof }
    (PDCMergePattern mer) -> stepMergePattern m conf { pattern = mer }
        -- let brs = stepMergePattern m conf { pattern = mer }
           --                  in map (\ b -> b { pattern = PDCMergePattern (pattern b) }) brs

tracebrss a = trace ("    brss:" ++ (show $ map prettyBranch a)) a

traceMergeof a
  = let tm (p, b) = trace ("  pattern: " ++ prettyPDCRulePattern p) (p, tracebrss b)
    in map tm a

stepMergePattern :: BranchStep PDCMergeP
stepMergePattern m br 
  | ps <- pdcRulePatternsMerge (pattern br)
--    = let brss    = forM ps (\ p -> stepRulePattern m br { pattern = p })
    = let brss    = map (\ p -> stepRulePattern m br { pattern = p }) ps
          mergeof = traceMergeof $ zip ps brss :: [(PDCRulePattern, [Branch PDCRulePattern])]
--      in undefined
          
          to :: (PDCRulePattern, [Branch PDCRulePattern]) -> [Branch PDCRulePattern]
          to (p, brs)
              = let newstatus
--                      | and (map isSuccess brs) = SuccessBranch
                      | and (map isFailed brs)  = FailedBranch
                      | otherwise               = RunningBranch
                in map (to' p newstatus) brs
          to' :: PDCRulePattern -> BranchStatus -> Branch PDCRulePattern -> Branch PDCRulePattern
          to' p newstatus nbr
              = let newentities =  to'' ps p (pattern nbr)
                in nbr { pattern = PDCMergePattern (pattern br) { pdcRulePatternsMerge = newentities }
                       , status = newstatus
                       }
          to'' :: [PDCRulePattern] -> PDCRulePattern -> PDCRulePattern -> [PDCRulePattern]
          to'' orig from to = foldr (\ p pl -> if p == from then to:pl else p:pl ) [] orig
      in concat $ map to mergeof


stepOneOfPattern :: BranchStep PDCOneOfP
stepOneOfPattern m br
  | ps <- pdcRulePatternsOneOf (pattern br)
    = let brss = forM ps (\ p -> stepRulePattern m br { pattern = p })
      in (concat brss)


stepMsgPattern :: BranchStep PDCMsgP
stepMsgPattern m br@(Branch {..}) = case matchBounded boundEnv pattern m of
    (Just be) -> [br { boundEnv = be, status = SuccessBranch }]
    Nothing   -> [br { status = FailedBranch }]

stepSeqPattern :: BranchStep PDCSeqP
stepSeqPattern m br
  | [] <- pdcRulePatternsSeq (pattern br) = [br { status = FailedBranch }]
  | ((PDCStartInstantlyPattern _):ps) <- pdcRulePatternsSeq (pattern br)
    = stepSeqPattern m br { pattern = (pattern br) { pdcRulePatternsSeq = ps } }
  | (p:ps) <- pdcRulePatternsSeq (pattern br)
    = let brs = stepRulePattern m br { pattern = p }
          newStatus b [] = getStatus b
          newStatus b _ = if isFailed b then FailedBranch else RunningBranch
      in map (\ b -> b { pattern = (pattern br) { pdcRulePatternsSeq = if isSuccess b then ps else (pattern b):ps }
                       , status = newStatus b ps 
                       }) brs
--      in map (\ b -> b { pattern = (pattern br) { pdcRulePatternsSeq = (pattern b):ps } }) brs

class Stappable a where
    stappable :: a -> Bool
-}
{-
instance Stappable PDCSeqP where
    stappable ()
-}

{-
eval :: PDCRuleE -> [PDCMsgP] -> Bool
eval PDCRuleE {..} msglist = isRight $ configRunner (Configuration [Branch emptyBoundEnv pdcRulePattern False] msglist)

configRunner :: Configuration PDCRulePattern -> Either (PDCMsgP, PDCMsgP) [PDCMsgP]
configRunner conf = case step conf of
    (Left e)  -> Left e
    (Right conf') -> maybe (configRunner conf') (const (Right $ msglist $ conf')) (findSuccesBranch conf')


step :: Configuration PDCRulePattern -> Either (PDCMsgP, PDCMsgP) (Configuration PDCRulePattern)
step conf@(Configuration {..})
  | [] <- msglist     = return conf
  | (m:ms) <- msglist = do
      brss <- forM branches (stepRulePattern m)
      return conf { msglist = ms, branches = concat brss }

stepRulePattern :: BStep PDCRulePattern
stepRulePattern m conf = do
    case pattern conf of
        (PDCMsgPattern msg) -> do
            brs <- stepMsgPattern m conf { pattern = msg }
            return $ map (\ b -> b { pattern = PDCMsgPattern (pattern b) }) brs
        (PDCSeqPattern seq) -> do
            brs <- stepSeqPattern m conf { pattern = seq }
            return $ map (\ b -> b { pattern = PDCSeqPattern (pattern b) }) brs
        (PDCOneOfPattern oneof) -> do
            -- brs <- stepOneOfPattern m conf { pattern = oneof }
            -- return $ map (\ b -> b { pattern = PDCOneOfPattern (pattern b) }) brs
            stepOneOfPattern m conf { pattern = oneof }
        (PDCMergePattern mer) -> do
            brs <- stepMergePattern m conf { pattern = mer }
            return $ map (\ b -> b { pattern = PDCMergePattern (pattern b) }) brs

stepOneOfPattern :: BStep PDCOneOfP
stepOneOfPattern m br
  | ps <- pdcRulePatternsOneOf (pattern br) = do
      brss <- forM (trace'' "stepOneOfPattern.ps" ps) (\ p -> trace'' "p->" $ stepRulePattern m br { pattern = p })
      -- return $ map tobr (concat brss)
      return $ trace'' "stepOneOfPattern.return" (concat brss)
      -- (\ b -> b { pattern = PDCOneOfPattern (PDCOneOfP (sourceInfoOneOf $ pattern br) [pattern b]) }) (concat brss)
    where
      tobr b = b { pattern = (PDCOneOfP (sourceInfoOneOf $ pattern br) [pattern b]) }

stepMergePattern :: BStep PDCMergeP
stepMergePattern m br
  | ps <- pdcRulePatternsMerge (pattern br) = do
      brs <- forM ps (\ p -> stepRulePattern m br { pattern = p })
      return $ undefined
      -- map (\ (mr, o) -> o {  } ) (zip brs ps)

stepSeqPattern :: BStep PDCSeqP
stepSeqPattern m conf
  | ((PDCStartInstantlyPattern _):ps) <- pdcRulePatternsSeq (pattern conf)
    = stepSeqPattern m conf { pattern = (pattern conf) { pdcRulePatternsSeq = ps } }
  | (p:ps) <- pdcRulePatternsSeq (pattern conf) = do
      brs <- stepRulePattern m conf { pattern = p }
      return $ map (\ b -> b { pattern = (pattern conf) { pdcRulePatternsSeq = (pattern b):ps } }) brs

stepMsgPattern :: BStep PDCMsgP
stepMsgPattern m conf@(Branch {..}) = do
      boundEnv' <- case matchBounded boundEnv pattern m of
          (Just be) -> return be
          Nothing -> Left (pattern, m)
      return [conf { boundEnv = boundEnv', finished = True }]

-}





