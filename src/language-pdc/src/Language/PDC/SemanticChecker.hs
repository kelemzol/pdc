
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

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BL

type SemanticChecker ms level a = ReaderT level (WriterT [Issue ms] IO) a

-- type PDCModuleSemanticChecker = SemanticChecker PDCModule

runSemanticChecker :: SemanticChecker slogen level a -> level -> IO (a, [Issue slogen])
runSemanticChecker sc level = runWriterT (runReaderT sc level)

data Issue ms
  = Hint ms
  | Warning ms
  | Error ms
  deriving (Eq, Ord, Show)

isHint :: Issue a -> Bool
isHint (Hint _) = True
isHint _ = False

isWarning :: Issue a -> Bool
isWarning (Warning _) = True
isWarning _ = False

isError :: Issue a -> Bool
isError (Error _) = True
isError _ = False


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

class Message a where
    message2readable :: a -> String
    message2workable :: a -> JSON.Value
    workable2message :: JSON.Value -> a

    message2string :: a -> String
    string2message :: String -> a

    message2string = BL.unpack . JSON.encode . message2workable
    string2message = workable2message . fromJust . JSON.decode . BL.pack



errorAssert1 :: (Verdict v, Message ms) => (level -> v) -> (v -> ms) -> SemanticChecker ms level ()
errorAssert1 cond desc = ask >>= flip (assert Error) desc . cond
--    lvl <- ask
--    assert Error slogen (cond lvl) desc


assert :: (Verdict v, Message ms) => (ms -> Issue ms) -> v -> (v -> ms) -> SemanticChecker ms level ()
assert i (v@(verdict -> False)) descf = tell [i (descf v)]
assert i _ _ = return ()


down :: (Message ms)
     => (level -> downLevel)
     -> SemanticChecker ms downLevel a
     -> SemanticChecker ms level a
down to sc = do
    lvl <- ask
    (a, is) <- liftIO $ runSemanticChecker sc (to lvl)
    tell is
    return a

downA :: (Message ms)
      => (level -> downLevel)
      -> (level -> info)
      -> (info -> SemanticChecker ms downLevel a)
      -> SemanticChecker ms level a
downA to inf sc = do
    lvl <- ask
    (a, is) <- liftIO $ runSemanticChecker (sc (inf lvl)) (to lvl)
    tell is
    return a

downTA :: (Traversable t, Monoid a, Message ms)
       => (level -> t downLevel)
       -> (level -> info)
       -> (info -> SemanticChecker ms downLevel a)
       -> SemanticChecker ms level a
downTA to inf sc = do
  lvl <- ask
  tt <- forM (to lvl) $ \ lvl' -> liftIO $ runSemanticChecker (sc (inf lvl)) lvl'
  let (a, is) = fold tt
  tell is
  return a

downRA :: (Traversable t, Monoid a, Message ms) => (level -> t level) -> (level -> info) -> (info -> SemanticChecker ms level a) -> SemanticChecker ms level a
downRA to inf sc = do
  lvl <- ask
  tt <- forM (to lvl) $ \ lvl' -> liftIO $ runSemanticChecker (sc (inf lvl)) lvl'
  let (a, is) = fold tt
  tell is
  return a


