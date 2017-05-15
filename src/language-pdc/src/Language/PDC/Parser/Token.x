{
module Language.PDC.Parser.Token ( Pos(..)
                                 , Token(..)
                                 , tokenize
                                 , printTokens
                                 , printTokensWithPos
                                 , printTokensGen
                                 , showTok
                                 , filterWs
                                 , insertWs
                                 , isTkWs
                                 , isTkId
                                 ) where
}

%wrapper "posn"

$digit = 0-9 -- digits
$alpha = [a-zA-Z\-] -- alphabetic characters
$uc = [A-Z]
$lc = [a-z]

tokens :-
    $white+                            { vtok TkWs }
    "rule"                             { tok TkRule }
    "start"                            { tok TkStart }
    "seq"                              { tok TkSeq }
    "optional"                         { tok TkOptional }
    "one-of"                           { tok TkOneOf }
    "many-of"                          { tok TkManyOf }
    "instantly"                        { tok TkInstantly }
    "->"                               { tok TkArrow }
    ":"                                { tok TkColon }
    ","                                { tok TkComma }
    "{"                                { tok TkBraceOpen }
    "}"                                { tok TkBraceClose }
    "("                                { tok TkBracketOpen }
    ")"                                { tok TkBracketClose }
    $uc [$alpha $digit \_ \']*         { vtok TkIdUC }
    $lc [$alpha $digit \_ \']*         { vtok TkIdLC }

{


tok f p s = f (apos2pos p s)
vtok f p s = f (apos2pos p s) s


data Pos
  = Pos
    { posLine :: Int
    , posColumn :: Int
    , posAt :: String
    , origSrc :: String
    }
  deriving (Eq, Ord, Show)

apos2pos :: AlexPosn -> String -> Pos
apos2pos (AlexPn _ l c) s = Pos l c ("[l:" ++ show l ++ ",c:" ++ show c ++ "]") s

data Token
  = TkWs           { pos :: Pos, tkws :: String }
  | TkRule         { pos :: Pos }
  | TkStart        { pos :: Pos }
  | TkSeq          { pos :: Pos }
  | TkOptional     { pos :: Pos }
  | TkOneOf        { pos :: Pos }
  | TkManyOf       { pos :: Pos }
  | TkInstantly    { pos :: Pos }
  | TkArrow        { pos :: Pos }
  | TkColon        { pos :: Pos }
  | TkComma        { pos :: Pos }
  | TkBraceOpen    { pos :: Pos }
  | TkBraceClose   { pos :: Pos }
  | TkBracketOpen  { pos :: Pos }
  | TkBracketClose { pos :: Pos }
  | TkIdLC         { pos :: Pos, tkid :: String }
  | TkIdUC         { pos :: Pos, tkid :: String }
  deriving (Eq, Ord, Show)

prettyTokens :: [Token] -> String
prettyTokens = concat . map (origSrc . pos)

tokenize = alexScanTokens

printTokensWithPos = printTokensGen show
printTokens = printTokensGen showTok

showTok = m (takeWhile (/=' ') . show)
  where
    m _ (TkIdUC _ str) = "TkIdUC(" ++ str ++ ")"
    m _ (TkIdLC _ str) = "TkIdLC(" ++ str ++ ")"
    m sf o = sf o

printTokensGen showFun = mapM_ (putStrLn . showFun) . tokenize

filterWs = filter (not . isTkWs)
isTkWs (TkWs _ _) = True
isTkWs _ = False

isTkId (TkIdLC _ _) = True
isTkId (TkIdUC _ _) = True
isTkId _ = False

insertWs = concat . map (\x -> [x, TkWs (Pos 0 0 "" "") " "])




{-
alexScanTokens str = go (alexStartPos,’\n’,[],str)
    where go inp@(pos,_,_,str) = case alexScan inp 0 of
        AlexEOF -> []
        AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
        AlexSkip inp' len -> go inp'
        AlexToken inp' len act -> act pos (take len str) : go inp'
-}
}

