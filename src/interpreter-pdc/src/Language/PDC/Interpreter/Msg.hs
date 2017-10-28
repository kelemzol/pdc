
{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Interpreter.Msg where

import Data.Maybe

import Language.PDC.Repr
import Language.PDC.Interpreter.Env

import Debug.Trace

trace' a = trace (show a) a
trace'' str a = trace ("\n" ++ str ++ ":" ++ (show a)) a

directMatch :: PDCMsgP -> PDCMsgP -> Bool
directMatch a b = and [ pdcid (pdcMsgFrom a) == pdcid (pdcMsgFrom b)
                      , pdcid (pdcMsgTo a)   == pdcid (pdcMsgTo b)
                      , pdcid (pdcMsgType a) == pdcid (pdcMsgType b)
                      ]

getBoundable :: PDCMsgP -> PDCMsgP -> Maybe [(PDCId, PDCId)]
getBoundable a b = let (fa, fb, ta, tb) = (pdcMsgFrom a, pdcMsgFrom b, pdcMsgTo a, pdcMsgTo b)
    in case (pdcid fa == pdcid fb, pdcid ta == pdcid tb, ulcase fa, ulcase ta) of
    (True, True, _, _) -> Just []
    (False, True, LC, _) -> Just [(fa, fb)]
    (True, False, _, LC) -> Just [(ta, tb)]
    (False, False, LC, LC) -> Just [(fa, fb), (ta, tb)]
    _ -> Nothing

partialMatch :: PDCMsgP -> PDCMsgP -> Bool
partialMatch a b = isJust (getBoundable a b)

boundIds :: BoundEnv -> [PDCId] -> PDCMsgP -> PDCMsgP
boundIds be ids m = foldr (boundId be) m ids

boundId ::  BoundEnv -> PDCId -> PDCMsgP -> PDCMsgP
boundId be id msg@PDCMsgP {..} = msg { pdcMsgFrom = flat (getBounded be) pdcMsgFrom
                                     , pdcMsgTo = flat (getBounded be) pdcMsgTo
                                     }
  where
    flat f a = case f a of
        (Just a') -> a'
        Nothing -> a

matchBounded :: BoundEnv -> PDCMsgP -> PDCMsgP -> Maybe BoundEnv
matchBounded be a b = do
    boundables <- getBoundable a b
    let newEnv = foldr (\(x,y) b -> bound b x y) be boundables
        a'     = boundIds newEnv (map fst boundables) a
    if directMatch a' b then Just newEnv else Nothing


