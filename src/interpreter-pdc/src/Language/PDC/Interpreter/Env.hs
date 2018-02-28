
{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Interpreter.Env where

import qualified Data.Map as M

import Language.PDC.Repr



data BoundEnv
  = BoundEnv
    { bmap :: M.Map String String
    }
  deriving (Eq, Show)

emptyBoundEnv :: BoundEnv
emptyBoundEnv = BoundEnv { bmap = M.empty }

isBounded :: BoundEnv -> PDCId -> Bool
isBounded BoundEnv {..} PDCId {..} = M.member pdcid bmap

bound :: BoundEnv -> PDCId -> PDCId -> BoundEnv
bound be@BoundEnv {..} i1 i2 = if isBounded be i1 then be else (be { bmap = M.insert (pdcid i1) (pdcid i2) bmap })

getBounded :: BoundEnv -> PDCId -> Maybe PDCId
getBounded BoundEnv {..} k = fmap (\i -> k {pdcid = i, ulcase = UC}) (M.lookup (pdcid k) bmap)

toList = M.toList . bmap
