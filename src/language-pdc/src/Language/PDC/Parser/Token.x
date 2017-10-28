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
                                 , tokeneq
                                 ) where
}

%wrapper "posn"

$digit = 0-9 -- digits
$alpha = [a-zA-Z\-] -- alphabetic characters
$uc = [A-Z]
$lc = [a-z]

tokens :-
    $white+                            { vtok TkWs }
    "module"                           { tok TkModule }
    "rule"                             { tok TkRule }
    "start"                            { tok TkStart }
    "seq"                              { tok TkSeq }
    "optional"                         { tok TkOptional }
    "one-of"                           { tok TkOneOf }
    "more-of"                          { tok TkMoreOf }
    "many-of"                          { tok TkManyOf }
    "interleave"                       { tok TkInterleave }
    "instantly"                        { tok TkInstantly }
    "merge"                            { tok TkMerge }
    "export"                           { tok TkExport }
    "->"                               { tok TkArrow }
    ":"                                { tok TkColon }
    ","                                { tok TkComma }
    "{"                                { tok TkBraceOpen }
    "}"                                { tok TkBraceClose }
    "("                                { tok TkBracketOpen }
    ")"                                { tok TkBracketClose }
    "<"                                { tok TkAngleOpen }
    ">"                                { tok TkAngleClose }
    "["                                { tok TkSquareOpen }
    "]"                                { tok TkSquareClose }
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
  | TkModule       { pos :: Pos }
  | TkRule         { pos :: Pos }
  | TkStart        { pos :: Pos }
  | TkSeq          { pos :: Pos }
  | TkOptional     { pos :: Pos }
  | TkOneOf        { pos :: Pos }
  | TkMoreOf       { pos :: Pos }
  | TkManyOf       { pos :: Pos }
  | TkInterleave   { pos :: Pos }
  | TkInstantly    { pos :: Pos }
  | TkMerge       { pos :: Pos }
  | TkExport       { pos :: Pos }
  | TkArrow        { pos :: Pos }
  | TkColon        { pos :: Pos }
  | TkComma        { pos :: Pos }
  | TkBraceOpen    { pos :: Pos }
  | TkBraceClose   { pos :: Pos }
  | TkBracketOpen  { pos :: Pos }
  | TkBracketClose { pos :: Pos }
  | TkAngleOpen    { pos :: Pos }
  | TkAngleClose   { pos :: Pos }
  | TkSquareOpen   { pos :: Pos }
  | TkSquareClose  { pos :: Pos }
  | TkIdLC         { pos :: Pos, tkid :: String }
  | TkIdUC         { pos :: Pos, tkid :: String }
  deriving (Eq, Ord, Show)

tokeneq :: Token -> Token -> Bool
tokeneq (TkWs _ _) (TkWs _ _) = True
tokeneq (TkModule _) (TkModule _) = True
tokeneq (TkRule _) (TkRule _) = True
tokeneq (TkStart _) (TkStart _) = True
tokeneq (TkSeq _) (TkSeq _) = True
tokeneq (TkOptional _) (TkOptional _) = True
tokeneq (TkOneOf _) (TkOneOf _) = True
tokeneq (TkMoreOf _) (TkMoreOf _) = True
tokeneq (TkManyOf _) (TkManyOf _) = True
tokeneq (TkInstantly _) (TkInstantly _) = True
tokeneq (TkMerge _) (TkMerge _) = True
tokeneq (TkExport _) (TkExport _) = True
tokeneq (TkArrow _) (TkArrow _) = True
tokeneq (TkColon _) (TkColon _) = True
tokeneq (TkComma _) (TkComma _) = True
tokeneq (TkBraceOpen _) (TkBraceOpen _) = True
tokeneq (TkBraceClose _) (TkBraceClose _) = True
tokeneq (TkBracketOpen _) (TkBracketOpen _) = True
tokeneq (TkBracketClose _) (TkBracketClose _) = True
tokeneq (TkAngleOpen _) (TkAngleOpen _) = True
tokeneq (TkAngleClose _) (TkAngleClose _) = True
tokeneq (TkSquareOpen _) (TkSquareOpen _) = True
tokeneq (TkSquareClose _) (TkSquareClose _) = True
tokeneq (TkIdLC _ _) (TkIdLC _ _) = True
tokeneq (TkIdUC _ _) (TkIdUC _ _) = True
tokeneq _ _ = False

prettyTokens :: [Token] -> String
prettyTokens = concat . map (origSrc . pos)

tokenize = filterWs . alexScanTokens

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

