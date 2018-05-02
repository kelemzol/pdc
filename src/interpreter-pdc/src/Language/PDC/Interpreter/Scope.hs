{-# LANGUAGE RecordWildCards
           , FlexibleInstances
           , MultiParamTypeClasses
           , MonoLocalBinds
           #-}

module Language.PDC.Interpreter.Scope where

import Language.PDC.Repr
import qualified Data.Map as M

import Debug.Trace

data Scope a
  = SingletonScope (M.Map String a)
  | ThisScope
  | BlockScope
  deriving (Eq, Show)

type ScopeH a = [Scope a]


-- Init
-- ----------------------------------------------------------------------------

emptyScopeH :: ScopeH a
emptyScopeH = [ThisScope]

newScopeH :: Scope a -> ScopeH a
newScopeH sch = [ThisScope, sch]

--singletonScopeH :: a -> ScopeH a
--singletonScopeH a = [ThisScope , SingletonScope a]

--multiScopeH :: [a] -> ScopeH a
--multiScopeH ids = [ThisScope, MultiScope ids]

-- Scope
-- ----------------------------------------------------------------------------

pushScope :: ScopeH a -> Scope a -> ScopeH a
pushScope (ThisScope:o) sc = ThisScope:sc:o
pushScope o sc = sc:o

popScope :: ScopeH a -> ScopeH a
popScope (ThisScope:_:o) = ThisScope:o
popScope (_:o) = o

createScope :: [(String, a)] -> Scope a
createScope l = SingletonScope (M.fromList l)

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


operate :: (Show elem) => [String] -> (elem -> elem) -> ScopeH elem -> (elem, ScopeH elem)
operate [] _ _               = error $ "Language.PDC.Interpreter.Scope.operate: empty selector list"
operate sls _ []             = error $ "Language.PDC.Interpreter.Scope.operate: empty scope; not found: `" ++ show sls ++ "` selectors"
operate sls _ (BlockScope:_) = error $ "Language.PDC.Interpreter.Scope.operate: block scope; not found: `" ++ show sls ++ "` selectors"
operate [sel] fn (sc@(SingletonScope map):scs)
  | (Just value', map') <- M.updateLookupWithKey (\ _ value -> Just (fn value)) sel map = (value', (SingletonScope map':scs))
  | otherwise = let (e, scs') = operate [sel] fn scs in (e, sc:scs')
operate [sel] fn (sc@(ThisScope):scs) = let (e, scs') = operate [sel] fn scs in (e, sc:scs')
operate ("this":sls) fn (sc@(ThisScope):scs) = let (e, scs') = operate sls fn scs in (e, sc:scs')
operate ("this":sls) fn (sc@(_):scs) = let (e, scs') = operate ("this":sls) fn scs in (e, sc:scs')
operate sels _ sc = error $ "Language.PDC.Interpreter.Scope.noQlfrOperate: undefined error; selectors: " ++ (show sels) ++ "; scope: " ++ (showScope sc)

showScope :: (Show elem) => [Scope elem] -> String
showScope [] = []
showScope ((SingletonScope map):scs) = "{Singleton:" ++ (show map) ++ "}" ++ (showScope scs)
showScope (ThisScope:scs) = "{ThisScope}" ++ (showScope scs)
showScope (BlockScope:scs) = "{BlockScope}" ++ (showScope scs)



{-
class This a where
  this :: a -> Bool
instance This [Char] where
  this "this" = True
  this _ = False

class (Eq qualifier, Ord selector, This selector, Show qualifier, Show selector) => LH qualifier selector
instance (Eq qualifier, Ord selector, This selector, Show qualifier, Show selector) => LH qualifier selector
-}



{-
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

type StrLeaf = Leaf String




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

createScope :: [(String, a)] -> Scope (ScLevel String String a)
createScope l = SingletonScope (ScLevel [] (M.fromList (map to l)))
  where
    to :: (String, a) -> (String, Leaf String a)
    to (sel, val) = (sel, Elem val)

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



operateLH :: LH qualifier selector => [qualifier] -> [selector] -> (elem -> elem) -> ScopeLH qualifier selector elem -> (elem, ScopeLH qualifier selector elem)
operateLH _ [] _ _               = error $ "Language.PDC.Interpreter.Scope.operateLH: empty selector list"
operateLH _ sls _ []             = error $ "Language.PDC.Interpreter.Scope.operateLH: empty scope; not found: `" ++ show sls ++ "` selectors"
operateLH _ sls _ (BlockScope:_) = error $ "Language.PDC.Interpreter.Scope.operateLH: block scope; not found: `" ++ show sls ++ "` selectors"
operateLH [] sls fn sc = trace (show sls ++ ", " ++ (show $ length sc)) $ noQlfrOperate sls fn sc
operateLH qlfrs sls fn sc = qlfrOperate qlfrs sls fn sc

noQlfrOperate :: LH qualifier selector => [selector] -> (elem -> elem) -> ScopeLH qualifier selector elem -> (elem, ScopeLH qualifier selector elem)
noQlfrOperate (sl:sls) fn (sc@(SingletonScope l):scs)
  | (Just e, map) <- trace ("map: " ++ (show sl)) $ M.updateLookupWithKey (\ k v -> slUpdate sls fn v) sl (lvl l)
      = case e of
          (Elem e') -> (e', (SingletonScope (l {lvl = map})):scs)
          _ -> error $ "Language.PDC.Interpreter.Scope.noQlfrOperate: updated not primitive leaf"
  | otherwise = let (e, scs) = noQlfrOperate sls fn scs in (e, sc:scs)
noQlfrOperate (sl:sls) fn (sc@(ThisScope):scs) = let (e, scs) = trace ("thisscope") $ noQlfrOperate sls fn scs in (e, sc:scs)
noQlfrOperate _ _ [] = error $ "Language.PDC.Interpreter.Scope.noQlfrOperate: empty scope list"
noQlfrOperate _ _ _ = error $ "Language.PDC.Interpreter.Scope.noQlfrOperate: undefined error"

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

qlfrOperate :: LH qualifier selector => [qualifier] -> [selector] -> (elem -> elem) -> ScopeLH qualifier selector elem -> (elem, ScopeLH qualifier selector elem)
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
-}
