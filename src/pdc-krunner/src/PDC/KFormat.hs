
{-# LANGUAGE ViewPatterns
           , RecordWildCards
           #-}

module PDC.KFormat (kFormat, kFormatIO) where

import Language.PDC.Parser

kFormatIO :: FilePath -> FilePath -> IO ()
kFormatIO i o = do
    content <- readFile i
    writeFile o (kFormat content)

kFormat :: String -> String
kFormat = concat . (gen genKFormat) . tokenize

gen :: ([Token] -> ([Token], String)) -> [Token] -> [String]
gen g [] = []
gen g st = s:(gen g st')
  where
    (st', s) = g st

genKFormat :: [Token] -> ([Token], String)
genKFormat ((TkWs   {..}):o)           = (o, tkws)
genKFormat ((TkRule {..}):o)           = (o, "@rule")
genKFormat ((TkStart {..}):o)          = (o, "@start")
genKFormat ((TkSeq {..}):o)            = (o, "@seq")
genKFormat ((TkOptional {..}):o)       = (o, "@optional")
genKFormat ((TkOneOf {..}):o)          = (o, "@one-of")
genKFormat ((TkManyOf {..}):o)         = (o, "@many-of")
genKFormat ((TkInstantly {..}):o)      = (o, "@instantly")
genKFormat (msg [] -> Just (o, [i,_,j,_,m])) = (o, "@MSG(" ++ idKF i ++ ", " ++ idKF j ++ ", " ++ idKF m ++ ")")
genKFormat ((TkComma {..}):o)          = (o, ",")
genKFormat ((TkBraceOpen {..}):o)      = (o, "{")
genKFormat ((TkBraceClose {..}):o)     = (o, "}")
genKFormat ((TkBracketOpen {..}):o)    = (o, "(")
genKFormat ((TkBracketClose {..}):o)   = (o, ")")
genKFormat (i@(TkIdLC {..}):o)         = (o, idKF i)
genKFormat (i@(TkIdUC {..}):o)         = (o, idKF i)
genKFormat o = error $ "Language.PDC.KFormat.genKFormat: " ++ show o

idKF :: Token -> String
idKF (TkIdUC {..}) = "@UC(String2Id(\"" ++ (idNorm tkid) ++ "\"))"
idKF (TkIdLC {..}) = "@LC(" ++ (idNorm tkid) ++ ")"
idKF o = error $ "Language.PDC.KFormat.idKF: " ++ show o

idNorm :: String -> String
idNorm [] = []
idNorm ('-':os) = "__d__" ++ (idNorm os)
idNorm (o:os) = o:(idNorm os)

msg :: [Token] -> [Token] -> Maybe ([Token], [Token])
msg m ((isTkWs   -> True):o)      = msg m o
msg [] (t@(isTkId -> True):o)     = msg [t] o
msg [i] (t@(TkArrow {..}):o)      = msg [i,t] o
msg [i,a] (t@(isTkId -> True):o)  = msg [i,a,t] o
msg [i,a,j] (t@(TkColon {..}):o)  = msg [i,a,j,t] o
msg [i,a,j,c] (t@(isTkId -> True):o) = Just (o, [i,a,j,c,t])
msg _ _ = Nothing


{-
= TkWs           { pos :: Pos, tkws :: String }
  | TkRule         { pos :: Pos }
  | TkStart        { pos :: Pos }
  | TkSeq          { pos :: Pos }
  | TkOptional     { pos :: Pos }
  | TkOneOf        { pos :: Pos }
  | TkManyOf       { pos :: Pos }
  | TkArrow        { pos :: Pos }
  | TkColon        { pos :: Pos }
  | TkComma        { pos :: Pos }
  | TkBraceOpen    { pos :: Pos }
  | TkBraceClose   { pos :: Pos }
  | TkBracketOpen  { pos :: Pos }
  | TkBracketClose { pos :: Pos }
  | TkIdLC         { pos :: Pos, tkid :: String }
  | TkIdUC         { pos :: Pos, tkid :: String }
-}