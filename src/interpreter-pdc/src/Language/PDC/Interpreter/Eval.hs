
{-# LANGUAGE RecordWildCards
           , ViewPatterns
           , TypeFamilies
           , FlexibleContexts
--           , PartialTypeSignatures
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
import qualified Language.PDC.Interpreter.Scope as Scope
import Language.PDC.Interpreter.EvalAttrContent

import qualified Debug.Trace as Trace


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

containMsgEdgeE :: Edge -> Bool
containMsgEdgeE = all (not . isMsgEdgeE)
  where
    isMsgEdgeE (MsgEdgeE _) = True
    isMsgEdgeE _ = False

filterRunnableEdgesWithoutMsg :: [(Edge, Node)] -> [(Edge, Node)]
filterRunnableEdgesWithoutMsg = filter (containMsgEdgeE . fst)

evalNode :: Node -> [PDCMsgP] -> BoundEnv -> ScopeEnv -> EvalNodeRes
evalNode Leaf _ benv sce = EvalNodeSuccess { boundEnv = benv }
evalNode Node {..} [] benv sce = case filterRunnableEdgesWithoutMsg branches of
    [] -> EvalNodeFail { failedPattern = pattern, failedMsg = Nothing, boundEnv = benv }
    branches' -> case findSucces ress of
        Nothing -> head ress
        (Just succ) -> succ
      where
        ress = map (matcher benv sce (error "Language.PDC.Interpreter.Eval.evalNode: empty msglist, none msgEdgeE, msg") []) branches'
evalNode Node {..} (m:ms) benv sce = case findSucces ress of
    Nothing -> head ress
    (Just succ) -> succ
  where
      ress = map (matcher benv sce m ms) branches

matcher :: BoundEnv -> ScopeEnv -> PDCMsgP -> [PDCMsgP] -> (Edge, Node) -> EvalNodeRes
matcher benv sce msg msglist (pattern, node)
  | ((MsgEdgeE msgPattern):_) <- pattern
  , directMatch msgPattern msg
  , Evaluated sce' <- evalMsgAction sce msg
  = evalNode node msglist benv sce'

  | ((MsgEdgeE msgPattern):_) <- pattern
  , partialMatch msgPattern msg
  , (Just benv') <- matchBounded benv msgPattern msg
  , Evaluated sce' <- evalMsgAction sce msg
  = evalNode node msglist benv' sce'

  | ((MsgEdgeE msgPattern):_) <- pattern
  = EvalNodeFail { failedPattern = toRulePattern pattern, failedMsg = Just msg, boundEnv = benv }

--  | [ActEdgeE actPattern] <- pattern
--  , Just () <- actionMatch actPattern
--  = evalNode node (msg:msglist) benv

  | ((ScopeEdgeE ScopeClose):tl) <- pattern = matcher benv (Scope.popScope sce) msg msglist (tl, node)
  | ((ScopeEdgeE ScopeThisBack):tl) <- pattern = matcher benv (Scope.thisBack sce) msg msglist (tl, node)
  | ((ScopeEdgeE (ScopeOpen l)):tl) <- pattern = matcher benv (Scope.pushScope sce (createScope l)) msg msglist (tl, node)
  | ((ActEdgeE action):tl) <- pattern = case evalAttrContent (sce) action of
      (Evaluated sce') -> matcher benv sce' msg msglist (tl, node)
      Discard -> EvalNodeFail { failedPattern = toRulePattern pattern, failedMsg = Just msg, boundEnv = benv }
  | [] <- pattern = evalNode node msglist benv sce
--  | otherwise = evalNode node msglist benv

  | otherwise = EvalNodeFail { failedPattern = toRulePattern pattern, failedMsg = Just msg, boundEnv = benv }




createScope :: [PDCVarTypeBinding] -> Scope.Scope ElemType
createScope l = Scope.createScope (map to l)
  where
    to :: PDCVarTypeBinding -> (String, ElemType)
    to (PDCVarTypeBinding {..}) = (lcid pdcVarTypeBindingVarName, value (ucid pdcVarTypeBindingTypeName))
    value "Integer" = IntegerET 0
    value "String" = StringET ""
    value "Bool" = BoolET False


