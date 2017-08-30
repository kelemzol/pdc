
{-# LANGUAGE ViewPatterns
           , RecordWildCards
           #-}

module Language.PDC.SemanticChecker where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Either
import Data.Typeable
import Data.Maybe

import Data.Traversable
import Data.Foldable

import Language.PDC.Repr

type SemanticChecker slogen level a = ReaderT level (WriterT [Issue slogen] IO) a

-- type PDCModuleSemanticChecker = SemanticChecker PDCModule

runSemanticChecker :: SemanticChecker slogen level a -> level -> IO (a, [Issue slogen])
runSemanticChecker sc level = runWriterT (runReaderT sc level)

data Issue slogen
  = Hint slogen String
  | Warning slogen String
  | Error slogen String
  deriving (Eq, Ord, Show)

isHint :: Issue a -> Bool
isHint (Hint _ _) = True
isHint _ = False

isWarning :: Issue a -> Bool
isWarning (Warning _ _) = True
isWarning _ = False

isError :: Issue a -> Bool
isError (Error _ _) = True
isError _ = False


data IssueEnv
  = IssueEnv
    { 
    }
  deriving (Eq, Ord, Show)

{-
hintAssert :: slogen -> Bool -> String -> SemanticChecker slogen level ()
hintAssert = assert Hint

warningAssert :: slogen -> Bool -> String -> SemanticChecker slogen level ()
warningAssert = assert Warning

errorAssert :: slogen -> Bool -> String -> SemanticChecker slogen level ()
errorAssert = assert Error
-}

class Verdict a where
    verdict :: a -> Bool

instance Verdict Bool where
    verdict = id
instance Verdict (Either l r) where
    verdict = isRight
instance Verdict [a] where
    verdict = null
instance Verdict (Maybe a) where
    verdict = isNothing

may :: (a -> Bool) -> a -> Maybe a
may c a = if not (c a) then Just a else Nothing



errorAssert1 :: (Verdict v) => slogen -> (level -> v) -> (v -> String) -> SemanticChecker slogen level ()
errorAssert1 slogen cond desc = ask >>= flip (assert Error slogen) desc . cond
--    lvl <- ask
--    assert Error slogen (cond lvl) desc


assert :: (Verdict v) => (slogen -> String -> Issue slogen) -> slogen -> v -> (v -> String) -> SemanticChecker slogen level ()
assert i slogen (v@(verdict -> False)) descf = tell [i slogen (descf v)]
assert i _ _ _ = return ()


down :: (level -> downLevel) -> SemanticChecker slogen downLevel a -> SemanticChecker slogen level a
down to sc = do
    lvl <- ask
    (a, is) <- liftIO $ runSemanticChecker sc (to lvl)
    tell is
    return a

downA :: (level -> downLevel) -> (level -> info) -> (info -> SemanticChecker slogen downLevel a) -> SemanticChecker slogen level a
downA to inf sc = do
    lvl <- ask
    (a, is) <- liftIO $ runSemanticChecker (sc (inf lvl)) (to lvl)
    tell is
    return a

downTA :: (Traversable t, Monoid a) => (level -> t downLevel) -> (level -> info) -> (info -> SemanticChecker slogen downLevel a) -> SemanticChecker slogen level a
downTA to inf sc = do
  lvl <- ask
  tt <- forM (to lvl) $ \ lvl' -> liftIO $ runSemanticChecker (sc (inf lvl)) lvl'
  let (a, is) = fold tt
  tell is
  return a

downRA :: (Traversable t, Monoid a) => (level -> t level) -> (level -> info) -> (info -> SemanticChecker slogen level a) -> SemanticChecker slogen level a
downRA to inf sc = do
  lvl <- ask
  tt <- forM (to lvl) $ \ lvl' -> liftIO $ runSemanticChecker (sc (inf lvl)) lvl'
  let (a, is) = fold tt
  tell is
  return a


