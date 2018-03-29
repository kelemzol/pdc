{-# LANGUAGE RecordWildCards
           , FlexibleInstances
           , MultiParamTypeClasses
           , MonoLocalBinds
           #-}

module Language.PDC.Interpreter.Scope where

import Language.PDC.Repr
import qualified Data.Map as M

data ScopeProfile
  = ScopeProfile
    { noQlfrImplClimb :: Bool
    }
  deriving (Eq, Show)

type ScopeH a = [Scope a]

data Scope a
  = SingletonScope a
  | MultiScope [a]
  | ThisScope
  | BlockScope
  deriving(Eq, Show)

data ScLevel qualifier selector elem
  = ScLevel
    { lvlQlfrs :: [qualifier]
    , lvl :: M.Map selector (Leaf selector elem)
    }
  deriving(Eq, Show) -- qualifier Map selector elem

data Leaf selector elem
  = Elem elem
  | Leaf (M.Map selector (Leaf selector elem))
  deriving(Eq, Show)

type ScopeLH qualifier selector elem = ScopeH (ScLevel qualifier selector elem)

type StrScopeLH elem = ScopeLH String String elem





-- Init
-- ----------------------------------------------------------------------------

emptyScopeH :: ScopeH a
emptyScopeH = [ThisScope]

newScopeH :: Scope a -> ScopeH a
newScopeH sch = [ThisScope, sch]

singletonScopeH :: a -> ScopeH a
singletonScopeH id = [ThisScope , SingletonScope id]

multiScopeH :: [a] -> ScopeH a
multiScopeH ids = [ThisScope, MultiScope ids]

-- Scope
-- ----------------------------------------------------------------------------

pushScope :: ScopeH a -> Scope a -> ScopeH a
pushScope (ThisScope:o) sc = ThisScope:sc:o
pushScope o sc = sc:o

popScope :: ScopeH a -> ScopeH a
popScope (ThisScope:_:o) = ThisScope:o
popScope (_:o) = o

-- Block
-- ----------------------------------------------------------------------------

blockScopeH :: ScopeH a -> ScopeH a
blockScopeH sch = BlockScope:(thisTop sch)

blockPushScopeH :: ScopeH a -> Scope a -> ScopeH a
blockPushScopeH sch sc = sc:BlockScope:(thisTop sch)

-- This
-- ----------------------------------------------------------------------------

thisTop :: ScopeH a -> ScopeH a
thisTop o = ThisScope:(filter neqThisScope o)
  where
    neqThisScope ThisScope = False
    neqThisScope _ = True

thisBack :: ScopeH a -> ScopeH a
thisBack (ThisScope:sc:BlockScope:sch) = sc:BlockScope:ThisScope:sch
thisBack (ThisScope:sc:sch) = sc:ThisScope:sch
thisBack (sc:sch) = sc:(thisBack sch)
thisBack o = o


-- Util
-- ----------------------------------------------------------------------------



operateLH :: LH qualifier selector => [qualifier] -> [selector] -> (elem -> elem) -> ScopeLH qualifier selector elem -> ScopeLH qualifier selector elem
operateLH _ [] _ _               = error $ "Language.PDC.Interpreter.Scope.operateLH: empty selector list"
operateLH _ sls _ []             = error $ "Language.PDC.Interpreter.Scope.operateLH: empty scope; not found: `" ++ show sls ++ "` selectors"
operateLH _ sls _ (BlockScope:_) = error $ "Language.PDC.Interpreter.Scope.operateLH: block scope; not found: `" ++ show sls ++ "` selectors"
operateLH [] sls fn sc = noQlfrOperate sls fn sc
operateLH qlfrs sls fn sc = qlfrOperate qlfrs sls fn sc

noQlfrOperate :: LH qualifier selector => [selector] -> (elem -> elem) -> ScopeLH qualifier selector elem -> ScopeLH qualifier selector elem
noQlfrOperate (sl:sls) fn (sc@(SingletonScope l):scs)
  | (Just _, map) <- M.updateLookupWithKey (\ k v -> slUpdate sls fn v) sl (lvl l)
              = (SingletonScope (l {lvl = map})):scs
  | otherwise = sc:(noQlfrOperate sls fn scs)

slUpdate :: (Show selector, Ord selector) => [selector] -> (elem -> elem) -> Leaf selector elem -> Maybe (Leaf selector elem)
slUpdate [] fn (Elem e) = Just (Elem (fn e))
slUpdate [] _ _ = Nothing
slUpdate sls fn (Elem _) = Nothing
slUpdate (sl:sls) fn (Leaf m)
  | (Just _, m') <- M.updateLookupWithKey (\ k v -> slUpdate sls fn v) sl m
              = Just (Leaf m')
  | otherwise = Nothing
{-
data Leaf selector elem
  = Elem elem
  | Leaf (M.Map selector (Leaf selector elem))
-}

qlfrOperate :: LH qualifier selector => [qualifier] -> [selector] -> (elem -> elem) -> ScopeLH qualifier selector elem -> ScopeLH qualifier selector elem
qlfrOperate [] sls fn scs = noQlfrOperate sls fn scs
qlfrOperate (qlfr:qlfrs) sls fn (sc@(SingletonScope l):scs)
  | True <- elem qlfr (lvlQlfrs l) = qlfrOperate qlfrs sls fn scs
  | otherwise = error $ "Language.PDC.Interpreter.Scope.qlfrOperate: wrong qualifier `" ++ show qlfr ++ "` on level: `" ++ show (lvlQlfrs l) ++ "`"


class This a where
    this :: a -> Bool
instance This [Char] where
    this "this" = True
    this _ = False

class (Eq qualifier, Ord selector, This selector, Show qualifier, Show selector) => LH qualifier selector
instance (Eq qualifier, Ord selector, This selector, Show qualifier, Show selector) => LH qualifier selector
