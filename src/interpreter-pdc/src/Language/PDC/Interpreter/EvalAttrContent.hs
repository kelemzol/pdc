
{-# LANGUAGE RecordWildCards
, ViewPatterns
, TypeFamilies
, FlexibleContexts
#-}

module Language.PDC.Interpreter.EvalAttrContent where

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
import Language.PDC.Interpreter.Scope

import qualified Debug.Trace as Trace


data ElemType
  = IntegerET Integer
  | BoolET Bool
  | StringET String
  deriving(Eq, Show)

type ScopeEnv = ScopeH ElemType

data EvalAttrContentRes
    = Evaluated ScopeEnv
    | Discard

eacBind :: EvalAttrContentRes -> (ScopeEnv -> EvalAttrContentRes) -> EvalAttrContentRes
eacBind (Discard) _ = Discard
eacBind (Evaluated sc) fn = fn sc

evalMsgAction :: ScopeEnv -> PDCMsgP -> EvalAttrContentRes
evalMsgAction sce msg
  | Just ac <- pdcMsgContent msg = evalAttrContent sce ac
  | otherwise = Evaluated sce


evalAttrContent :: ScopeEnv -> PDCAttrContent -> EvalAttrContentRes
evalAttrContent sce (PDCAttrContentActionCall ac) = evalActionCall sce ac -- PDCActionCall
evalAttrContent sce (PDCAttrContentInlineAction ab) = evalActionBody sce ab -- PDCActionBody

evalActionCall :: ScopeEnv -> PDCActionCall -> EvalAttrContentRes
evalActionCall = error $ "Language.PDC.Interpreter.EvalAttrContent.evalActionCall: Action call is not supported"

evalActionBody :: ScopeEnv -> PDCActionBody -> EvalAttrContentRes
evalActionBody sce (PDCActionBody {..}) = evalStatements (Evaluated sce) pdcActionStatements

evalStatements :: EvalAttrContentRes -> [PDCActionStatement] -> EvalAttrContentRes
evalStatements sce sts = foldl evalStatement sce sts

evalStatement :: EvalAttrContentRes -> PDCActionStatement -> EvalAttrContentRes
evalStatement Discard _ = Discard
evalStatement (Evaluated sce) (PDCDiscardStatement _) = Discard
evalStatement (Evaluated sce) (PDCAssignStatement as) = Evaluated (evalAssignStatement sce as)
evalStatement (Evaluated sce) (PDCIfStatement is)     = evalIfStatement sce is
evalStatement (Evaluated sce) (PDCWhileStatement ws)  = evalWhileStatement sce ws

evalAssignStatement :: ScopeEnv -> PDCAssignS -> ScopeEnv
evalAssignStatement sce (PDCAssignS {..}) = snd $ operate selectors (const value) sce
  where
    (qualifiers, selectors) = leftValue pdcAssignLeftExpr
    value = evalExpression sce pdcAssignRightExpr

evalIfStatement :: ScopeEnv -> PDCIfS -> EvalAttrContentRes
evalIfStatement sce (PDCIfS {..}) = if condition == (BoolET True) then evalActionBody sce pdcIfBody else Evaluated sce
  where
    condition = evalExpression sce pdcIfCondition

evalWhileStatement :: ScopeEnv -> PDCWhileS -> EvalAttrContentRes
evalWhileStatement sce o@(PDCWhileS {..}) = if condition /= (BoolET True) then (evalActionBody sce pdcWhileBody) `eacBind` (flip evalWhileStatement o) else Evaluated sce
    where
        condition = evalExpression sce pdcWhileCondition


leftValue :: PDCExpression -> ([String], [String])
leftValue e = ([], leftValue' e)
  where
    leftValue' (PDCBinOperatorExpression (PDCBinOperatorE { pdcBinOp = PDCMemberBO, pdcBinOpLeft = (PDCIdExpression i), ..})) = lcid i : (leftValue' pdcBinOpRight)
    leftValue' (PDCIdExpression i) = [lcid i]
    leftValue' _ = error $ "Language.PDC.Interpreter.EvalAttrContent.leftValue': non memberselection"

evalExpression :: ScopeEnv -> PDCExpression -> ElemType
evalExpression sce (PDCIdExpression i) = fst $ operate [lcid i] id sce
evalExpression sce (PDCStringLiteralExpression (PDCStringLiteralE {..})) =   StringET pdcStringLiteralStr
evalExpression sce (PDCIntegerLiteralExpression (PDCIntegerLiteralE {..})) = IntegerET pdcIntegerLiteralInt
evalExpression sce (PDCBoolLiteralExpression (PDCBoolLiteralE {..})) = BoolET pdcBoolLiteralBool
evalExpression sce o@(PDCBinOperatorExpression (PDCBinOperatorE { pdcBinOp = PDCMemberBO })) = let (qs,ss) = leftValue o in fst $ operate ss id sce
evalExpression sce o@(PDCBinOperatorExpression (PDCBinOperatorE { pdcBinOp = otherOp, .. })) = evalBinOp otherOp
                                                                                                         (evalExpression sce pdcBinOpLeft)
                                                                                                         (evalExpression sce pdcBinOpRight)



evalBinOp :: PDCBinOperator -> ElemType -> ElemType -> ElemType
evalBinOp PDCEqBO (IntegerET a) (IntegerET b) = BoolET (a == b)
evalBinOp PDCNEqBO (IntegerET a) (IntegerET b) = BoolET (a /= b)
evalBinOp PDCMinusBO (IntegerET a) (IntegerET b) = IntegerET (a - b)
evalBinOp PDCPlusBO (IntegerET a) (IntegerET b) = IntegerET (a + b)
evalBinOp PDCEqBO (StringET a) (StringET b) = BoolET (a == b)
evalBinOp PDCNEqBO (StringET a) (StringET b) = BoolET (a /= b)
evalBinOp PDCEqBO (BoolET a) (BoolET b) = BoolET (a == b)
evalBinOp PDCNEqBO (BoolET a) (BoolET b) = BoolET (a /= b)
evalBinOp o a b = error $ "Language.PDC.Interpreter.EvalAttrContent.evalBinOp: Operator or type missmatch: " ++ show a ++ " " ++ show o ++ " " ++ show b


{-
operate :: LH qualifier selector => [qualifier] -> [selector] -> (elem -> elem) -> ScopeLH qualifier selector elem -> ScopeLH qualifier selector elem
data PDCAssignS
  = PDCAssignS
    { sourceInfoAssignStatement :: SourceInfo
    , pdcAssignLeftExpr :: PDCExpression
    , pdcAssignRightExpr :: PDCExpression
    }


PDCActionBody
  = PDCActionBody
    { sourceInfoActionBody :: SourceInfo
    , pdcActionStatements :: [PDCActionStatement]
    }
-}