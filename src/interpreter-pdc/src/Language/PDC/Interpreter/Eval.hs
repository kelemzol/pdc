
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

matcher :: BoundEnv -> PDCMsgP -> [PDCMsgP] -> (Edge, Node) -> EvalNodeRes
matcher benv msg msglist (pattern, node)
  | [MsgEdgeE msgPattern] <- pattern
  , directMatch msgPattern msg
  = evalNode node msglist benv

  | [MsgEdgeE msgPattern] <- pattern
  , partialMatch msgPattern msg
  , (Just benv') <- matchBounded benv msgPattern msg
  = evalNode node msglist benv'

  | [ActEdgeE actPattern] <- pattern
  , Just () <- actionMatch actPattern
  = evalNode node (msg:msglist) benv

  | otherwise = EvalNodeFail { failedPattern = toRulePattern pattern, failedMsg = Just msg, boundEnv = benv }

actionMatch :: PDCAttrContent -> Maybe ()
actionMatch _ = Just ()


