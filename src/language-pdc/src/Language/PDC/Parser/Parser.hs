
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
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P

import Control.Monad.IO.Class
import Control.Concurrent.MVar

import Language.PDC.Parser.Token
import Language.PDC.Repr



tok t f = token (origSrc . pos) (tpos2ppos . pos) (\ t' -> if tokeneq t t' then Just (f t') else Nothing)

u = undefined

idTok c = tok (c u u) tkws

normTok c = tok (c u) (const ())

emptyPos = newPos "empty" 0 0

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
moduleParserIO str = do
    mvar <- newMVar 0
    res <- parserIO moduleParser str
    return (fmap (\ m -> m { pdcCallUnivSeqNum = Just mvar } ) res)

msgListParserIO :: String -> IO (Either String [PDCMsgP])
msgListParserIO = parserIO msgListParser

getSourceInfoP :: PDCParser SourceInfo
getSourceInfoP = SourceInfo <$> getPosition


tkUc, tkLc, tkStringLit, tkIntegerLit :: PDCParser String
tkModule, tkRule, tkStart, tkSeq, tkOptional, tkOneOf, tkMoreOf, tkManyOf, tkUnSeq, tkInstantly, tkMerge, tkPre, tkPost :: PDCParser ()
tkExport, tkArrow, tkColon, tkComma, tkBraceOpen, tkBraceClose, tkBracketOpen, tkBracketClose, tkAngleOpen, tkAngleClose, tkSquareOpen, tkSquareClose :: PDCParser ()
tkType, tkRecord, tkMsg, tkAttr, tkBegin, tkAction, tkIf, tkWhile, tkDiscard, tkAt, tkEq, tkNEq, tkAssign, tkDot, tkMinus, tkPlus, tkFalse, tkTrue :: PDCParser ()
brace, bracket, angle, square :: PDCParser a -> PDCParser a

tkUc           = idTok TkIdUC
tkLc           = idTok TkIdLC
tkStringLit    = idTok TkStringLit
tkIntegerLit   = idTok TkIntegerLit
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
tkType         = normTok TkType      
tkRecord       = normTok TkRecord    
tkMsg          = normTok TkMsg       
tkAttr         = normTok TkAttr
tkBegin        = normTok TkBegin     
tkAction       = normTok TkAction
tkIf           = normTok TkIf        
tkWhile        = normTok TkWhile     
tkDiscard      = normTok TkDiscard
tkPre          = normTok TkPre
tkPost         = normTok TkPost
tkDot          = normTok TkDot
tkAt           = normTok TkAt        
tkEq           = normTok TkEq        
tkNEq          = normTok TkNEq       
tkAssign       = normTok TkAssign
tkMinus        = normTok TkMinus
tkPlus         = normTok TkPlus
tkFalse        = normTok TkFalse
tkTrue         = normTok TkTrue
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
parseModule = PDCModule <$> getSourceInfoP <*> (tkModule *> parsePDCId) <*> (many parsePDCModuleEntry) <*> (pure Nothing)

parsePDCId :: PDCParser PDCId
parsePDCId = (try (PDCId <$> getSourceInfoP <*> tkUc <*> (pure UC)))
              <|> (PDCId <$> getSourceInfoP <*> tkLc <*> (pure LC))
              <?> "PDC-id"

parseUCId :: PDCParser UCId
parseUCId = UCId <$> getSourceInfoP <*> tkUc
            <?> "Id start with upper case"

parseLCId :: PDCParser LCId
parseLCId = LCId <$> getSourceInfoP <*> tkLc
            <?> "Id start with lower case"

parsePDCModuleEntry :: PDCParser PDCModuleEntry
parsePDCModuleEntry = (try (PDCExportEntry <$> parsePDCExportE))
                  <|> (try (PDCDataTypeEntry <$> parsePDCDataTypeE))
                  <|> (try (PDCActionEntry <$> parsePDCActionTypeE))
                  <|>      (PDCRuleEntry <$> parsePDCRuleE)
                  <?> "PDC-module-entry"

parsePDCActionTypeE :: PDCParser PDCActionE
parsePDCActionTypeE = PDCActionE <$> getSourceInfoP <*> parsePDCActionHeader <*> parsePDCActionBody

parsePDCActionHeader :: PDCParser PDCActionHeader
parsePDCActionHeader = PDCActionHeader <$> getSourceInfoP <*> (tkAction *> parseLCId) -- <*> parsePDCActionType

-- parsePDCActionType :: PDCParser PDCActionType
-- parsePDCActionType = PDCActionType <$> getSourceInfoP
--                                    <*> (try (angle (parsePDCActionTemplParam `sepBy` tkComma)) <|> (pure []))
--                                    <*> (bracket (parsePDCActionCallParam `sepBy` tkComma))
{-
parsePDCActionTemplParam :: PDCParser PDCActionTemplParam
parsePDCActionTemplParam = ({-try-} (PDCActionTemplTypeParam <$> parsePDCTemplTypeP))
--                     <|>      (PDCRuleTemplRuleParam <$> parsePDCRuleHeader) -- parsePDCRuleTemplRuleP)
                     <?> "PDC-rule-template-parameter"

parsePDCTemplTypeP :: PDCParser PDCTemplTypeP
parsePDCTemplTypeP = PDCTemplTypeP <$> getSourceInfoP <*> (tkType *> parseUCId)

parsePDCActionCallParam :: PDCParser PDCActionCallParam
parsePDCActionCallParam = PDCActionCallParam <$> getSourceInfoP <*> parseLCId <*> (tkColon *> parseUCId)
-}
parsePDCActionBody :: PDCParser PDCActionBody
parsePDCActionBody = PDCActionBody <$> getSourceInfoP <*> (brace (many parsePDCActionStatement))

parsePDCActionStatement :: PDCParser PDCActionStatement
parsePDCActionStatement = ((PDCAssignStatement <$> parsePDCAssignS))
                      <|> ({-try-} (PDCIfStatement <$> parsePDCIfS))
                      <|> ({-try-} (PDCWhileStatement <$> parsePDCWhileS))
                      <|>      (PDCDiscardStatement <$> parsePDCDiscardS)
                      <?> "PDC-action-statement"

parsePDCAssignS :: PDCParser PDCAssignS
parsePDCAssignS = PDCAssignS <$> getSourceInfoP <*> parsePDCExpression <*> (tkAssign *> parsePDCExpression) <?> "assign"

parsePDCIfS :: PDCParser PDCIfS
parsePDCIfS = PDCIfS <$> getSourceInfoP <*> (tkIf *> parsePDCExpression) <*> parsePDCActionBody <*> (pure Nothing)

parsePDCWhileS :: PDCParser PDCWhileS
parsePDCWhileS = PDCWhileS <$> getSourceInfoP <*> (tkWhile *> parsePDCExpression) <*> parsePDCActionBody

parsePDCDiscardS :: PDCParser PDCDiscardS
parsePDCDiscardS = PDCDiscardS <$> getSourceInfoP <*> (tkDiscard *> parsePDCExpression)

parsePDCExpression :: PDCParser PDCExpression
parsePDCExpression = buildExpressionParser table term
                 <?> "expression"
  where
    term = ({-try-} (bracket parsePDCExpression))
       <|> ({-try-} (PDCIdExpression <$> parseLCId))
       <|> ({-try-} (PDCStringLiteralExpression <$> parsePDCStringLiteralE))
       <|> ({-try-} (PDCIntegerLiteralExpression <$> parsePDCIntegerLiteralE))
       <|> ({-try-} (PDCBoolLiteralExpression <$> parsePDCBoolLiteralE))
       <?> "term expression"
    table = [ [memberOp]
            , [eqOp, nEqOp]
            , [minusOp, plusOp]
            ]
    binop tok op assoc = Infix (getSourceInfoP >>= \pos ->  tok >> return (\ a b -> PDCBinOperatorExpression (PDCBinOperatorE pos a b op)) ) assoc
    --binop tok op assoc = Infix (do {tok; return (\ a b -> PDCBinOperatorExpression (PDCBinOperatorE undefined a b op)) }) assoc
    eqOp     = binop tkEq    PDCEqBO     AssocNone
    nEqOp    = binop tkNEq   PDCNEqBO    AssocNone
    minusOp  = binop tkMinus PDCMinusBO  AssocLeft
    plusOp   = binop tkPlus  PDCPlusBO   AssocLeft
    memberOp = binop tkDot   PDCMemberBO AssocRight


parsePDCStringLiteralE :: PDCParser PDCStringLiteralE
parsePDCStringLiteralE = PDCStringLiteralE <$> getSourceInfoP <*> tkStringLit

parsePDCIntegerLiteralE :: PDCParser PDCIntegerLiteralE
parsePDCIntegerLiteralE = PDCIntegerLiteralE <$> getSourceInfoP <*> (read <$> tkIntegerLit)

parsePDCBoolLiteralE :: PDCParser PDCBoolLiteralE
parsePDCBoolLiteralE = PDCBoolLiteralE <$> getSourceInfoP <*> ((tkFalse >> (return False)) <|> (tkTrue >> (return True)))

parsePDCDataTypeE :: PDCParser PDCDataTypeE
parsePDCDataTypeE = (try (PDCRecordTypeEntry <$> parsePDCRecordTypeE))
                <|> ({-try-} (PDCMsgTypeEntry <$> parsePDCMsgTypeE))
--                <|>      (PDCRuleEntry <$> parsePDCRuleE)
                <?> "PDC-type-definition"

parsePDCRecordTypeE :: PDCParser PDCRecordTypeE
parsePDCRecordTypeE = PDCRecordTypeE <$> getSourceInfoP <*> (tkType *> tkRecord *> parseUCId) <*> brace (many1 parseVarTypeBinding)

parsePDCMsgTypeE :: PDCParser PDCMsgTypeE
parsePDCMsgTypeE = PDCMsgTypeE <$> getSourceInfoP <*> (tkType *> tkMsg *> parseUCId) <*> parseUCId

parseVarTypeBinding :: PDCParser PDCVarTypeBinding
parseVarTypeBinding = PDCVarTypeBinding <$> getSourceInfoP <*> parseLCId <*> (tkColon *> parseUCId)

parsePDCExportE :: PDCParser PDCExportE
parsePDCExportE = PDCExportE <$> getSourceInfoP <*> (tkExport *> parsePDCId)

parsePDCRuleE :: PDCParser PDCRuleE
parsePDCRuleE = PDCRuleE <$> getSourceInfoP <*> parsePDCRuleHeader <*> implicitSeqPattern

parsePDCRuleHeader :: PDCParser PDCRuleHeader
parsePDCRuleHeader = PDCRuleHeader <$> getSourceInfoP <*> (tkRule *> parsePDCId) <*> parsePDCRuleType

parsePDCRuleType :: PDCParser PDCRuleType
parsePDCRuleType = PDCRuleType <$> getSourceInfoP
                               <*> (try (angle (parsePDCRuleTemplParam `sepBy` tkComma)) <|> (pure []))
                               <*> (bracket ((PDCProcParam <$> parsePDCId) `sepBy` tkComma))
                               <*> ((try (tkAttr *> parseUCId)) <|> pure (UCId (SourceInfo emptyPos) "NullAttr"))

parsePDCRuleTemplParam :: PDCParser PDCRuleTemplParam
parsePDCRuleTemplParam = (try (PDCRuleTemplProcParam <$> parsePDCRuleTemplProcP))
                     <|>      (PDCRuleTemplRuleParam <$> parsePDCRuleHeader) -- parsePDCRuleTemplRuleP)
                     <?> "PDC-rule-template-parameter"

parsePDCRuleTemplProcP :: PDCParser PDCTemplProcP
parsePDCRuleTemplProcP = PDCTemplProcP <$> getSourceInfoP <*> (tkProc *> parsePDCId)

-- parsePDCRuleTemplRuleP :: PDCParser PDCTemplRuleP
-- parsePDCRuleTemplRuleP = PDCTemplRuleP <$> parsePDCRuleHeader


parsePDCRulePattern :: PDCParser PDCRulePattern
parsePDCRulePattern = ({-try-} (PDCSeqPattern <$> parsePDCSeqP))
                  <|> ({-try-} (PDCStartPattern <$> parseStartP))
                  <|> ({-try-} (PDCStartInstantlyPattern <$> parseStartInstantlyP))
                  <|> ({-try-} (PDCOneOfPattern <$> parseOneOfP))
                  <|> ({-try-} (PDCMoreOfPattern <$> parseMoreOfP))
                  <|> ({-try-} (PDCManyofPattern <$> parseManyOfP))
                  <|> ({-try-} (PDCUnSeqPattern <$> parseUnSeqP))
                  <|> ({-try-} (PDCOptionalPattern <$> parseOptionalP))
                  <|> ({-try-} (PDCMergePattern <$> parseMergeP))
                  <|> (try (PDCMsgPattern <$> parseMsgP))
                  <|>      (PDCCallPattern <$> parseCallP)
--                  <|> ({-try-} (PDCActionPattern <$> (tkAction *> parsePDCAttrContent)))
                  <?> "PDC-rule-pattern"

implicitSeqPattern :: PDCParser PDCRulePattern
implicitSeqPattern = PDCSeqPattern <$> (PDCSeqP <$> getSourceInfoP <*> blockPattern)

blockOrSingleton :: PDCParser PDCRulePattern
blockOrSingleton = (try implicitSeqPattern)
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
parseMsgP = PDCMsgP <$> getSourceInfoP <*> parsePDCId <*> (tkArrow *> parsePDCId) <*> (tkColon *> parsePDCId) <*> (optionMaybe parsePDCAttrContent) <?> "message pattern"
parseCallP = PDCCallP <$> getSourceInfoP
                      <*> parsePDCId
                      <*> (try (angle (parsePDCId `sepBy` tkComma)) <|> (pure []))
                      <*> (optionMaybe (tkPre *> parsePDCAttrContent))
                      <*> (optionMaybe ((tkPost *> parsePDCAttrContent) <|> parsePDCAttrContent))
                      <?> "call pattern"
    
--    (try (PDCCallP <$> getSourceInfoP <*> parsePDCId <*> (try (angle (parsePDCId `sepBy` tkComma)) <|> (pure [])) <*> pure Nothing <*> (optionMaybe parsePDCAttrContent)))
--             <|>  (PDCCallP <$> getSourceInfoP <*> parsePDCId <*> (try (angle (parsePDCId `sepBy` tkComma)) <|> (pure [])) <*> (optionMaybe (tkPre *> parsePDCAttrContent)) <*> (optionMaybe (tkPost *> parsePDCAttrContent)))
--             <?> "call pattern"

parsePDCAttrContent :: PDCParser PDCAttrContent
parsePDCAttrContent = ({-try-} (PDCAttrContentInlineAction <$> parsePDCActionBody))
                  <|> ({-try-} (PDCAttrContentActionCall <$> parsePDCActionCall))
                  <?> "attribute content"

parsePDCActionCall :: PDCParser PDCActionCall
parsePDCActionCall = PDCActionCall <$> getSourceInfoP <*> (tkAt *> parseLCId)
                                                      <*> ((try (angle (parseUCId `sepBy` tkComma))) <|> (pure []))
                                                      <*> ((try (bracket (parsePDCExpression `sepBy` tkComma))) <|> (pure []))

