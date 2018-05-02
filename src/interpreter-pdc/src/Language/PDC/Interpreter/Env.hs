
{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Interpreter.Env where

import qualified Data.Map as M
import Data.List

import Language.PDC.Repr



data BoundEnv
  = BoundEnv
    { bmap :: M.Map String String
    }
  deriving (Show, Read)

instance Eq BoundEnv where
    (==) (BoundEnv m1) (BoundEnv m2) = let (a, b) = (M.toList m1, M.toList m2) in containAll a b && containAll b a
      where
        containAll a b = and [ contain as b | as <- a ]
        contain (ak, av) b = or [ if (isPrefixOf "phantom" ak) && (isPrefixOf "phantom" bk) then av == bv else ak == bk && av == bv  | (bk, bv) <- b ]



emptyBoundEnv :: BoundEnv
emptyBoundEnv = BoundEnv { bmap = M.empty }

isBounded :: BoundEnv -> PDCId -> Bool
isBounded BoundEnv {..} PDCId {..} = M.member pdcid bmap

bound :: BoundEnv -> PDCId -> PDCId -> BoundEnv
bound be@BoundEnv {..} i1 i2 = if isBounded be i1 then be else (be { bmap = M.insert (pdcid i1) (pdcid i2) bmap })

getBounded :: BoundEnv -> PDCId -> Maybe PDCId
getBounded BoundEnv {..} k = fmap (\i -> k {pdcid = i, ulcase = UC}) (M.lookup (pdcid k) bmap)

toList = M.toList . bmap

updateIdEnv :: BoundEnv -> (String -> String) -> BoundEnv
updateIdEnv BoundEnv {..} f = BoundEnv $ M.fromList $ map update $ M.toList bmap
  where
    update (k,v) = (f k, v)
