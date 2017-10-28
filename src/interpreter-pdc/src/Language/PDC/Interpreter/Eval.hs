
{-# LANGUAGE RecordWildCards
           , ViewPatterns
           #-}

module Language.PDC.Interpreter.Eval (eval) where

import Data.Maybe
import Data.Either

import Control.Monad

import Language.PDC.Repr
import Language.PDC.Interpreter.Env
import Language.PDC.Interpreter.Msg

{-
eval :: PDCRuleE -> [PDCMsgP] -> Bool
eval PDCRuleE {..} msglist = let (be, res, remain) = evalPattern emptyBoundEnv pdcRulePattern msglist
                             in res
-}
{-
evalPattern :: BoundEnv -> PDCRulePattern -> [PDCMsgP] -> (BoundEnv, Bool, [PDCMsgP])
evalPattern be (PDCStartInstantlyPattern _) ml = (be, True, ml)
evalPattern be (PDCSeqPattern seq) ml = evalSeqPattern be seq ml
evalPattern be (PDCOneOfPattern one) ml = evalOneOfPattern be one ml
evalPattern be (PDCMsgPattern msg) ml = evalMsgPattern be msg ml

evalSeqPattern :: BoundEnv -> PDCSeqP -> [PDCMsgP] -> (BoundEnv, Bool, [PDCMsgP])
evalSeqPattern be (PDCSeqP {..}) ml = foldl evalSeq (be, True, ml) pdcRulePatternsSeq
  where
    evalSeq :: (BoundEnv, Bool, [PDCMsgP]) -> PDCRulePattern -> (BoundEnv, Bool, [PDCMsgP])
    evalSeq res@(_, False, _) _ = res
--    evalSeq _ (be, _, []) = (be, False, [])
    evalSeq (be, _, remain) p = evalPattern be p remain

evalOneOfPattern :: BoundEnv -> PDCSeqP -> [PDCMsgP] -> (BoundEnv, Bool, [PDCMsgP])
evalOneOfPattern

evalMsgPattern :: BoundEnv -> PDCMsgP -> [PDCMsgP] -> (BoundEnv, Bool, [PDCMsgP])
evalMsgPattern be msg remain@(x:xs) = maybe (be, False, remain) (\be' -> (be', True, xs)) (matchBounded be msg x)
-}

type Step a = Configuration a -> Either (PDCMsgP, PDCMsgP) (Configuration a)

data Branch a
  = Branch
    { boundEnv' :: BoundEnv
    , pattern'  :: a
    }
  deriving (Eq, Show)

data Configuration a
  = Configuration
    { boundEnv :: BoundEnv
    , pattern  :: a
    , msglist  :: [PDCMsgP]
    , finished :: Bool
    }
  deriving (Eq, Show)

eval :: PDCRuleE -> [PDCMsgP] -> Bool
eval PDCRuleE {..} msglist = isRight $ configRunner (Configuration emptyBoundEnv pdcRulePattern msglist False)

configRunner :: Configuration PDCRulePattern -> Either (PDCMsgP, PDCMsgP) [PDCMsgP]
configRunner conf = case stepRulePattern conf of
    (Left e) -> Left e
    (Right c) -> if finished c then Right (msglist c) else configRunner c

stepRulePattern :: Step PDCRulePattern
stepRulePattern conf
  | True <- finished conf = return conf
  --  | [] <- pattern = return conf
  | (m:ms) <- msglist conf = do
    case pattern conf of
        (PDCStartInstantlyPattern _) -> return conf { finished = True }
        (PDCMsgPattern msg) -> do
            conf' <- stepMsgPattern conf { pattern = msg }
            return conf' { pattern = PDCMsgPattern (pattern conf') }
        (PDCSeqPattern seq) -> do
            conf' <- stepSeqPattern conf { pattern = seq }
            return conf' { pattern = PDCSeqPattern (pattern conf') }
        (PDCOneOfPattern oneof) -> do
            conf' <- stepOneOfPattern conf { pattern = oneof }
            return conf' { pattern = PDCOneOfPattern (pattern conf') }
        (PDCMergePattern mer) -> do
            conf' <- stepMergePattern conf { pattern = mer }
            return conf' { pattern = PDCMergePattern (pattern conf') }

stepOneOfPattern :: Step PDCOneOfP
stepOneOfPattern conf
  | True <- finished conf = return conf
  | ps <- pdcRulePatternsOneOf (pattern conf)
  , (m:ms) <- msglist conf = do
    return conf


stepMergePattern :: Step PDCMergeP
stepMergePattern conf
  | True <- finished conf = return conf
  | ps <- pdcRulePatternsMerge (pattern conf)
  , (m:ms) <- msglist conf = do
      confs <- forM ps (\ p -> stepRulePattern conf { pattern = p })
      return conf

stepSeqPattern :: Step PDCSeqP
stepSeqPattern conf
  | True <- finished conf = return conf
  | (p:ps) <- pdcRulePatternsSeq (pattern conf)
  , (m:ms) <- msglist conf = do
      conf' <- stepRulePattern conf { pattern = p }
      return conf' { pattern = (pattern conf) { pdcRulePatternsSeq = (pattern conf'):ps } }

stepMsgPattern :: Step PDCMsgP
stepMsgPattern conf@(Configuration {..})
  | True <- finished = return conf
--  | [] <- pattern = return conf
  | (m:ms) <- msglist = do
      boundEnv' <- case matchBounded boundEnv pattern m of
          (Just be) -> return be
          Nothing -> Left (pattern, m)
      return conf { msglist = ms, boundEnv = boundEnv', finished = True }







