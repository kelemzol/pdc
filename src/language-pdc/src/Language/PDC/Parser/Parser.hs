
{-# LANGUAGE RecordWildCards
           , FlexibleContexts
           #-}


module Language.PDC.Parser.Parser ( moduleParser
                                  , moduleParserIO
                                  , msgListParser
                                  , msgListParserIO
                                  ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Char
import qualified Text.Parsec.Token as P

import Language.PDC.Parser.Token
import Language.PDC.Repr



tok t f = token (origSrc . pos) (tpos2ppos . pos) (\ t' -> if tokeneq t t' then Just (f t') else Nothing)

u = undefined

idTok c = tok (c u u) tkid

normTok c = tok (c u) (const ())

tpos2ppos :: Pos -> SourcePos
tpos2ppos (Pos {..}) = newPos "Language.PDC.Parser.Parser.<stream>" posLine posColumn

type PDCParser = Parsec [Token] ()

type BasicParser a = String -> String -> Either ParseError a


rawParser :: PDCParser a -> BasicParser a
rawParser rp fn = parse rp fn . tokenize

moduleParser :: BasicParser PDCModule
moduleParser = rawParser parseModule -- parse parseModule fn . tokenize

msgListParser :: BasicParser [PDCMsgP]
msgListParser = rawParser (many parseMsgP) -- parse (many parseMsgP) fn . tokenize


parserIO :: BasicParser a -> String -> IO (Either String a)
parserIO bp fn = do
    content <- readFile fn
    case bp fn content of
        (Left e) -> return $ Left (show e)
        (Right m) -> return $ Right m

moduleParserIO :: String -> IO (Either String PDCModule)
moduleParserIO = parserIO moduleParser

msgListParserIO :: String -> IO (Either String [PDCMsgP])
msgListParserIO = parserIO msgListParser

getSourceInfoP :: PDCParser SourceInfo
getSourceInfoP = SourceInfo <$> getPosition


tkUc, tkLc :: PDCParser String
tkModule, tkRule, tkStart, tkSeq, tkOptional, tkOneOf, tkMoreOf, tkManyOf, tkUnSeq, tkInstantly, tkMerge :: PDCParser ()
tkExport, tkArrow, tkColon, tkComma, tkBraceOpen, tkBraceClose, tkBracketOpen, tkBracketClose, tkAngleOpen, tkAngleClose, tkSquareOpen, tkSquareClose :: PDCParser ()
brace, bracket, angle, square :: PDCParser a -> PDCParser a

tkUc           = idTok TkIdUC
tkLc           = idTok TkIdLC
tkModule       = normTok TkModule
tkRule         = normTok TkRule
tkStart        = normTok TkStart
tkSeq          = normTok TkSeq
tkOptional     = normTok TkOptional
tkOneOf        = normTok TkOneOf
tkMoreOf       = normTok TkMoreOf
tkManyOf       = normTok TkManyOf
tkUnSeq        = normTok TkUnSeq
tkInstantly    = normTok TkInstantly
tkMerge        = normTok TkMerge
tkExport       = normTok TkExport
tkProc         = normTok TkProc
tkArrow        = normTok TkArrow
tkColon        = normTok TkColon
tkComma        = normTok TkComma
tkBraceOpen    = normTok TkBraceOpen
tkBraceClose   = normTok TkBraceClose
tkBracketOpen  = normTok TkBracketOpen
tkBracketClose = normTok TkBracketClose
tkAngleOpen    = normTok TkAngleOpen
tkAngleClose   = normTok TkAngleClose
tkSquareOpen   = normTok TkSquareOpen
tkSquareClose  = normTok TkSquareClose
brace p        = id <$> tkBraceOpen *> p <* tkBraceClose
bracket p      = id <$> tkBracketOpen *> p <* tkBracketClose
angle p        = id <$> tkAngleOpen *> p <* tkAngleClose
square p       = id <$> tkSquareOpen *> p <* tkSquareClose

parseModule :: PDCParser PDCModule
parseModule = PDCModule <$> getSourceInfoP <*> (tkModule *> parsePDCId) <*> (many parsePDCModuleEntry)

parsePDCId :: PDCParser PDCId
parsePDCId = (try (PDCId <$> getSourceInfoP <*> tkUc <*> (pure UC)))
              <|> (PDCId <$> getSourceInfoP <*> tkLc <*> (pure LC))
              <?> "PDC-id"

parsePDCModuleEntry :: PDCParser PDCModuleEntry
parsePDCModuleEntry = (try (PDCExportEntry <$> parsePDCExportE))
                      <|> (PDCRuleEntry <$> parsePDCRuleE)
                      <?> "PDC-module-entry"

parsePDCExportE :: PDCParser PDCExportE
parsePDCExportE = PDCExportE <$> getSourceInfoP <*> (tkExport *> parsePDCId)

parsePDCRuleE :: PDCParser PDCRuleE
parsePDCRuleE = PDCRuleE <$> getSourceInfoP <*> (tkRule *> parsePDCId) <*> parsePDCRuleType <*> implicitSeqPattern

parsePDCRuleType :: PDCParser PDCRuleType
parsePDCRuleType = PDCRuleType <$> getSourceInfoP
                               <*> (try (angle (parsePDCRuleTemplParam `sepBy` tkComma)) <|> (pure []))
                               <*> (bracket ((PDCProcParam <$> parsePDCId) `sepBy` tkComma))

parsePDCRuleTemplParam :: PDCParser PDCTemplParam
parsePDCRuleTemplParam  = PDCTemplProcParam <$> parsePDCRuleTemplProcP

parsePDCRuleTemplProcP :: PDCParser PDCTemplProcP
parsePDCRuleTemplProcP = PDCTemplProcP <$> (tkProc *> parsePDCId)

parsePDCRulePattern :: PDCParser PDCRulePattern
parsePDCRulePattern = (try (PDCSeqPattern <$> parsePDCSeqP))
                  <|> (try (PDCStartPattern <$> parseStartP))
                  <|> (try (PDCStartInstantlyPattern <$> parseStartInstantlyP))
                  <|> (try (PDCOneOfPattern <$> parseOneOfP))
                  <|> (try (PDCMoreOfPattern <$> parseMoreOfP))
                  <|> (try (PDCManyofPattern <$> parseManyOfP))
                  <|> (try (PDCUnSeqPattern <$> parseUnSeqP))
                  <|> (try (PDCOptionalPattern <$> parseOptionalP))
                  <|> (try (PDCMergePattern <$> parseMergeP))
                  <|> (try (PDCMsgPattern <$> parseMsgP))
                  <|>      (PDCCallPattern <$> parseCallP)
                  <?> "PDC-rule-pattern"

implicitSeqPattern :: PDCParser PDCRulePattern
implicitSeqPattern = PDCSeqPattern <$> (PDCSeqP <$> getSourceInfoP <*> blockPattern)

blockOrSingleton :: PDCParser PDCRulePattern
blockOrSingleton = try implicitSeqPattern
                   <|> parsePDCRulePattern

blockPattern :: PDCParser [PDCRulePattern]
blockPattern = (brace (many parsePDCRulePattern))

blockPatternConstructor :: (SourceInfo -> [PDCRulePattern] -> b) -> PDCParser () -> PDCParser b
blockPatternConstructor c t = c <$> getSourceInfoP <*> (t *> blockPattern)

parsePDCSeqP = blockPatternConstructor PDCSeqP tkSeq
parseStartP = PDCStartP <$> getSourceInfoP <*> (tkStart *> blockOrSingleton)
parseStartInstantlyP = PDCStartInstantlyP <$> (getSourceInfoP <* tkStart <* tkInstantly)
parseOneOfP = blockPatternConstructor PDCOneOfP tkOneOf
parseMoreOfP = blockPatternConstructor PDCMoreOfP tkMoreOf
parseManyOfP = blockPatternConstructor PDCManyOfP tkManyOf
parseUnSeqP = blockPatternConstructor PDCUnSeqP tkUnSeq
parseMergeP = blockPatternConstructor PDCMergeP tkMerge
parseOptionalP = PDCOptionalP <$> getSourceInfoP <*> (tkOptional *> blockOrSingleton)
parseMsgP = PDCMsgP <$> getSourceInfoP <*> parsePDCId <*> (tkArrow *> parsePDCId) <*> (tkColon *> parsePDCId) <*> (pure ())
parseCallP = PDCCallP <$> getSourceInfoP <*> parsePDCId <*> (angle (parsePDCId `sepBy` tkComma))


