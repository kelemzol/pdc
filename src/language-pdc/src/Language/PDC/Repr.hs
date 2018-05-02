
{-# LANGUAGE RecordWildCards
           , DeriveGeneric
           , OverloadedStrings
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable
--           , UndecidableInstances
           #-}

module Language.PDC.Repr where

import Data.Maybe
import Data.Typeable
import Data.Data
import qualified Text.Parsec.Pos as P
import qualified Data.Aeson as JSON
import qualified Control.Applicative as JSON (empty)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)

import Control.Concurrent.MVar

data PDCModule
  = PDCModule
    { sourceInfoModule  :: SourceInfo
    , pdcModuleName     :: PDCId
    , pdcModuleEntries  :: [PDCModuleEntry]
    , pdcCallUnivSeqNum :: Maybe (MVar Integer) -- its a workaraound for universal q.
    }
  deriving (Eq, {- Ord, Show, Data, -} Typeable, Generic)
-- instance JSON.ToJSON PDCModule
-- instance JSON.FromJSON PDCModule

data PDCId
  = PDCId
    { sourceInfoId      :: SourceInfo
    , pdcid             :: String
    , ulcase            :: ULCase
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCId
instance JSON.FromJSON PDCId

data ULCase = UC | LC
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON ULCase
instance JSON.FromJSON ULCase

data UCId
  = UCId
    { sourceInfoUCId :: SourceInfo
    , ucid :: String
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON UCId
instance JSON.FromJSON UCId

data LCId
  = LCId
    { sourceInfoLCId :: SourceInfo
    , lcid :: String
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON LCId
instance JSON.FromJSON LCId


data PDCModuleEntry
  = PDCRuleEntry        PDCRuleE
  | PDCExportEntry      PDCExportE
  | PDCDataTypeEntry    PDCDataTypeE
  | PDCActionEntry      PDCActionE
  | PDCFunctionEntry    PDCFunctionE
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCModuleEntry
instance JSON.FromJSON PDCModuleEntry

data PDCActionE
  = PDCActionE
    { sourceInfoActionEntry :: SourceInfo
    , pdcActionHeader :: PDCActionHeader
    , pdcActionBody :: PDCActionBody
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCActionE
instance JSON.FromJSON PDCActionE
    
data PDCActionHeader
  = PDCActionHeader
    { sourceInfoActionHeader :: SourceInfo
    , pdcActionName :: LCId
    , pdcActionType :: PDCActionType
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCActionHeader
instance JSON.FromJSON PDCActionHeader

data PDCActionType
  = PDCActionType
   { sourceInfoActionType :: SourceInfo
   , pdcActionTemplParams :: [PDCActionTemplParam]
   , pdcActionCallPArams :: [PDCActionCallParam]
   }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCActionType
instance JSON.FromJSON PDCActionType

data PDCActionTemplParam
  = PDCActionTemplTypeParam PDCTemplTypeP
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCActionTemplParam
instance JSON.FromJSON PDCActionTemplParam

data PDCActionCallParam
  = PDCActionCallParam
    { sourceInfoActionCallParam :: SourceInfo
    , pdcActionCallParamVar :: LCId
    , pdcActionCallParamType :: UCId
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCActionCallParam
instance JSON.FromJSON PDCActionCallParam

data PDCActionBody
  = PDCActionBody
    { sourceInfoActionBody :: SourceInfo
    , pdcActionStatements :: [PDCActionStatement]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCActionBody
instance JSON.FromJSON PDCActionBody

data PDCActionStatement
  = PDCAssignStatement PDCAssignS
  | PDCIfStatement PDCIfS
  | PDCWhileStatement PDCWhileS
  | PDCDiscardStatement PDCDiscardS
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCActionStatement
instance JSON.FromJSON PDCActionStatement

data PDCAssignS
  = PDCAssignS
    { sourceInfoAssignStatement :: SourceInfo
    , pdcAssignLeftExpr :: PDCExpression
    , pdcAssignRightExpr :: PDCExpression
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCAssignS
instance JSON.FromJSON PDCAssignS

data PDCIfS
  = PDCIfS
    { sourceInfoIfStatement :: SourceInfo
    , pdcIfCondition :: PDCExpression
    , pdcIfBody :: PDCActionBody
    , pdcIfElseStatement :: Maybe PDCIfS
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCIfS
instance JSON.FromJSON PDCIfS

data PDCWhileS
  = PDCWhileS
    { sourceInfoWhileStatement :: SourceInfo
    , pdcWhileCondition :: PDCExpression
    , pdcWhileBody :: PDCActionBody
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCWhileS
instance JSON.FromJSON PDCWhileS

data PDCDiscardS
  = PDCDiscardS
    { sourceInfoDiscardStatement :: SourceInfo
    , pdcDiscardMessage :: PDCExpression
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCDiscardS
instance JSON.FromJSON PDCDiscardS

data PDCExpression
  = PDCIdExpression LCId
  | PDCStringLiteralExpression PDCStringLiteralE
  | PDCIntegerLiteralExpression PDCIntegerLiteralE
  | PDCBoolLiteralExpression PDCBoolLiteralE
  | PDCBinOperatorExpression PDCBinOperatorE
  | PDCUnOperatorExpression PDCUnOperatorE
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCExpression
instance JSON.FromJSON PDCExpression

data PDCStringLiteralE
  = PDCStringLiteralE
    { sourceInfoStringLiteralExpression :: SourceInfo
    , pdcStringLiteralStr :: String
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCStringLiteralE
instance JSON.FromJSON PDCStringLiteralE

data PDCIntegerLiteralE
  = PDCIntegerLiteralE
    { sourceInfoIntegerLiteralExpression :: SourceInfo
    , pdcIntegerLiteralInt :: Integer
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCIntegerLiteralE
instance JSON.FromJSON PDCIntegerLiteralE

data PDCBoolLiteralE
  = PDCBoolLiteralE
    { sourceInfoBoolLiteralExpression :: SourceInfo
    , pdcBoolLiteralBool :: Bool
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCBoolLiteralE
instance JSON.FromJSON PDCBoolLiteralE

data PDCBinOperatorE
  = PDCBinOperatorE
    { sourceInfoBinOperatorExpression :: SourceInfo
    , pdcBinOpLeft :: PDCExpression
    , pdcBinOpRight :: PDCExpression
    , pdcBinOp :: PDCBinOperator
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCBinOperatorE
instance JSON.FromJSON PDCBinOperatorE

data PDCBinOperator
  = PDCMemberBO
  | PDCEqBO
  | PDCNEqBO
  | PDCMinusBO
  | PDCPlusBO
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCBinOperator
instance JSON.FromJSON PDCBinOperator

data PDCUnOperatorE
  = PDCUnOperatorE
    { sourceInfoUnOperatorExpression :: SourceInfo
    , pdcUnOpOperand :: PDCExpression
    , pdcUnOp :: PDCUnOperator
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCUnOperatorE
instance JSON.FromJSON PDCUnOperatorE

data PDCUnOperator
  = PDCNotUO
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCUnOperator
instance JSON.FromJSON PDCUnOperator

data PDCFunctionE
  = PDCFunctionE
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCFunctionE
instance JSON.FromJSON PDCFunctionE

data PDCDataTypeE
  = PDCRecordTypeEntry PDCRecordTypeE
  | PDCMsgTypeEntry PDCMsgTypeE
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCDataTypeE
instance JSON.FromJSON PDCDataTypeE

data PDCRecordTypeE
  = PDCRecordTypeE
    { sourceInfoRecordTypeEntry :: SourceInfo
    , pdcRecordTypeName :: UCId
    , pdcRecordEntries :: [PDCVarTypeBinding]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCRecordTypeE
instance JSON.FromJSON PDCRecordTypeE

data PDCMsgTypeE
  = PDCMsgTypeE
    { sourceInfoMsgTypeEntry :: SourceInfo
    , pdcMsgTypeMsg :: UCId
    , pdcMsgTypeType :: UCId
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCMsgTypeE
instance JSON.FromJSON PDCMsgTypeE

data PDCVarTypeBinding
  = PDCVarTypeBinding
    { sourceInfoVarTypeBinding :: SourceInfo
    , pdcVarTypeBindingVarName :: LCId
    , pdcVarTypeBindingTypeName :: UCId
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCVarTypeBinding
instance JSON.FromJSON PDCVarTypeBinding

data PDCExportE
  = PDCExportE
    { sourceInfoExport  :: SourceInfo
    , pdcExportId       :: PDCId
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCExportE
instance JSON.FromJSON PDCExportE

data PDCRuleE
  = PDCRuleE
    { sourceInfoRuleEntry :: SourceInfo
    , pdcRuleEntryHeader :: PDCRuleHeader
    , pdcRulePattern    :: PDCRulePattern
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCRuleE
instance JSON.FromJSON PDCRuleE

data PDCRuleHeader
  = PDCRuleHeader
    { sourceInfoRuleHeader :: SourceInfo
    , pdcRuleName       :: PDCId
    , pdcRuleType       :: PDCRuleType
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCRuleHeader
instance JSON.FromJSON PDCRuleHeader

data PDCRuleType
  = PDCRuleType
    { sourceInfoRuleType :: SourceInfo
    , pdcRuleTempParams :: [PDCRuleTemplParam]
    , pdcRuleProcParams :: [PDCProcParam]
    , pdcRuleAttr       :: UCId
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCRuleType
instance JSON.FromJSON PDCRuleType

data PDCRuleTemplParam
  = PDCRuleTemplProcParam PDCTemplProcP
  | PDCRuleTemplRuleParam PDCRuleHeader
  | PDCRuleTemplTypeParam PDCTemplTypeP
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCRuleTemplParam
instance JSON.FromJSON PDCRuleTemplParam
  
data PDCTemplProcP
  = PDCTemplProcP
    { sourceInfoTemplProcP :: SourceInfo
    , pdcTemplProcParamId :: PDCId
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCTemplProcP
instance JSON.FromJSON PDCTemplProcP

data PDCTemplTypeP
  = PDCTemplTypeP
    { sourceInfoTemplTypeP :: SourceInfo
    , pdcTemplTypeParamId :: UCId
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCTemplTypeP
instance JSON.FromJSON PDCTemplTypeP

{-
data PDCTemplRuleP
  = PDCTemplRuleP
    { pdcTemplRuleParamId :: PDCId
    , pdcTemplRuleParamType :: PDCRuleType
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCTemplRuleP
instance JSON.FromJSON PDCTemplRuleP
-}

data PDCProcParam
  = PDCProcParam
    { pdcIdProcParam    :: PDCId
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCProcParam
instance JSON.FromJSON PDCProcParam

data PDCRulePattern
  = PDCSeqPattern       PDCSeqP
  | PDCUnSeqPattern     PDCUnSeqP
  | PDCStartPattern     PDCStartP
  | PDCStartInstantlyPattern PDCStartInstantlyP
  | PDCOneOfPattern     PDCOneOfP
  | PDCMoreOfPattern    PDCMoreOfP
  | PDCManyofPattern    PDCManyOfP
  | PDCOptionalPattern  PDCOptionalP
  | PDCCallPattern      PDCCallP
  | PDCMergePattern     PDCMergeP
  | PDCMsgPattern       PDCMsgP
--  | PDCActionPattern    PDCAttrContent
--  | PDCScopeAction      PDCScopeA
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCRulePattern
instance JSON.FromJSON PDCRulePattern
{-
data PDCScopeA
  = PDCScopeClose
  | PDCScopeOpen
  | PDCScopeThisBack
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCScopeA
instance JSON.FromJSON PDCScopeA
-}

data PDCMsgP
  = PDCMsgP
    { sourceInfoMsg     :: SourceInfo
    , pdcMsgFrom        :: PDCId
    , pdcMsgTo          :: PDCId
    , pdcMsgType        :: PDCId
    , pdcMsgContent     :: Maybe PDCAttrContent
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCMsgP
instance JSON.FromJSON PDCMsgP

data PDCAttrContent
  = PDCAttrContentActionCall PDCActionCall
  | PDCAttrContentInlineAction PDCActionBody
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCAttrContent
instance JSON.FromJSON PDCAttrContent

data PDCActionCall
  = PDCActionCall
    { sourceInfoActionCall :: SourceInfo
    , pdcActionCallName :: LCId
    , pdcActionCallTmplParams :: [UCId]
    , pdcActionCallParams :: [PDCExpression]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCActionCall
instance JSON.FromJSON PDCActionCall

data PDCStartInstantlyP
  = PDCStartInstantlyP
    { sourceInfoStartInstantly :: SourceInfo
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCStartInstantlyP
instance JSON.FromJSON PDCStartInstantlyP

data PDCSeqP
  = PDCSeqP
    { sourceInfoSeq     :: SourceInfo
    , pdcRulePatternsSeq :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCSeqP
instance JSON.FromJSON PDCSeqP

data PDCStartP
  = PDCStartP
    { sourceInfoStart   :: SourceInfo
    , pdcRulePatternStart :: PDCRulePattern
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCStartP
instance JSON.FromJSON PDCStartP

data PDCOneOfP
  = PDCOneOfP
    { sourceInfoOneOf   :: SourceInfo
    , pdcRulePatternsOneOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCOneOfP
instance JSON.FromJSON PDCOneOfP

data PDCMoreOfP
  = PDCMoreOfP
    { sourceInfoMoreOf  :: SourceInfo
    , pdcRulePatternsMoreOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCMoreOfP
instance JSON.FromJSON PDCMoreOfP

data PDCManyOfP
  = PDCManyOfP
    { sourceInfoManyOf  :: SourceInfo
    , pdcRulePatternsManyOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCManyOfP
instance JSON.FromJSON PDCManyOfP
  
data PDCUnSeqP
  = PDCUnSeqP
    { sourceInfoUnSeq  :: SourceInfo
    , pdcRulePatternsUnSeq :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCUnSeqP
instance JSON.FromJSON PDCUnSeqP
  
data PDCOptionalP
  = PDCOptionalP
    { sourceInfoOptional :: SourceInfo
    , pdcRulePatternOptional :: PDCRulePattern
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCOptionalP
instance JSON.FromJSON PDCOptionalP
  
data PDCCallP
  = PDCCallP
    { sourceInfoCall    :: SourceInfo
    , pdcRuleId         :: PDCId
    , pdcTmplPrmsCall   :: [PDCId]
    , pdcPreCallContent :: Maybe PDCAttrContent
    , pdcCallContent    :: Maybe PDCAttrContent
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCCallP
instance JSON.FromJSON PDCCallP
  
data PDCMergeP
  = PDCMergeP
    { sourceInfoMerge    :: SourceInfo
    , pdcRulePatternsMerge :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON PDCMergeP
instance JSON.FromJSON PDCMergeP

data SourceInfo
  = SourceInfo
    { parsecSorceInfo :: P.SourcePos
    }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
instance JSON.ToJSON SourceInfo
instance JSON.FromJSON SourceInfo

instance JSON.ToJSON P.SourcePos where
  toJSON si = JSON.object [ "sourceLine" JSON..= (P.sourceLine si)
                          , "sourceColumn" JSON..= (P.sourceColumn si)
                          , "sourceName" JSON..= (P.sourceName si)
                          ]

instance JSON.FromJSON P.SourcePos where
  parseJSON (JSON.Object o) = P.newPos
                         <$> o JSON..: "sourceName"
                         <*> o JSON..: "sourceLine"
                         <*> o JSON..: "sourceColumn"
  parseJSON _ = JSON.empty


class GetId a where
    getId :: a -> String

instance GetId PDCId where
    getId (PDCId {..}) = pdcid
instance GetId String where
    getId = id
instance GetId UCId where
    getId (UCId {..}) = ucid
instance GetId LCId where
    getId (LCId {..}) = lcid

    --instance (GetRuleName a) => GetId a where
--    getId = pdcid . getRuleName



class GetRuleName a where
    getRuleName :: a -> PDCId

instance GetRuleName PDCExportE where
    getRuleName = pdcExportId
instance GetRuleName PDCRuleE where
    getRuleName = pdcRuleName . pdcRuleEntryHeader
instance GetRuleName PDCRuleHeader where
    getRuleName = pdcRuleName
instance GetRuleName PDCModuleEntry where
    getRuleName (PDCExportEntry e) = getRuleName e
    getRuleName (PDCRuleEntry e) = getRuleName e



class GetSourceInfo a where
    getSourceInfo :: a -> SourceInfo

instance GetSourceInfo PDCModule where
    getSourceInfo = sourceInfoModule
instance GetSourceInfo PDCId where 
    getSourceInfo = sourceInfoId
instance GetSourceInfo PDCExportE where 
    getSourceInfo = sourceInfoExport
instance GetSourceInfo PDCRuleE where 
    getSourceInfo = sourceInfoRuleEntry
instance GetSourceInfo PDCRuleType where 
    getSourceInfo = sourceInfoRuleType
instance GetSourceInfo PDCMsgP where 
    getSourceInfo = sourceInfoMsg
instance GetSourceInfo PDCStartInstantlyP where 
    getSourceInfo = sourceInfoStartInstantly
instance GetSourceInfo PDCSeqP where 
    getSourceInfo = sourceInfoSeq
instance GetSourceInfo PDCStartP where 
    getSourceInfo = sourceInfoStart
instance GetSourceInfo PDCOneOfP where 
    getSourceInfo = sourceInfoOneOf
instance GetSourceInfo PDCMoreOfP where 
    getSourceInfo = sourceInfoMoreOf
instance GetSourceInfo PDCManyOfP where 
    getSourceInfo = sourceInfoManyOf
instance GetSourceInfo PDCUnSeqP where 
    getSourceInfo = sourceInfoUnSeq
instance GetSourceInfo PDCOptionalP where 
    getSourceInfo = sourceInfoOptional
instance GetSourceInfo PDCCallP where 
    getSourceInfo = sourceInfoCall


class ToRulePattern a where
    toRulePattern :: a -> PDCRulePattern

instance ToRulePattern PDCSeqP where
    toRulePattern = PDCSeqPattern
instance ToRulePattern PDCUnSeqP where
    toRulePattern = PDCUnSeqPattern
instance ToRulePattern PDCStartP where
    toRulePattern = PDCStartPattern
instance ToRulePattern PDCStartInstantlyP where
    toRulePattern = PDCStartInstantlyPattern
instance ToRulePattern PDCOneOfP where
    toRulePattern = PDCOneOfPattern
instance ToRulePattern PDCMoreOfP where
    toRulePattern = PDCMoreOfPattern
instance ToRulePattern PDCManyOfP where
    toRulePattern = PDCManyofPattern
instance ToRulePattern PDCOptionalP where
    toRulePattern = PDCOptionalPattern
instance ToRulePattern PDCCallP where
    toRulePattern = PDCCallPattern
instance ToRulePattern PDCMergeP where
    toRulePattern = PDCMergePattern
instance ToRulePattern PDCMsgP where
    toRulePattern = PDCMsgPattern
--instance ToRulePattern PDCAttrContent where
--    toRulePattern = PDCActionPattern



stringSourceInfo :: (GetSourceInfo a) => a -> String
stringSourceInfo = pretty . parsecSorceInfo . getSourceInfo
  where
    pretty s = (P.sourceName s) ++ ":" ++ (show $ P.sourceLine s) ++ ":" ++ (show $ P.sourceColumn s)

maybeRuleEntry :: PDCModuleEntry -> Maybe PDCRuleE
maybeRuleEntry (PDCRuleEntry e) = Just e
maybeRuleEntry _ = Nothing

maybeExportEntry :: PDCModuleEntry -> Maybe PDCExportE
maybeExportEntry (PDCExportEntry e) = Just e
maybeExportEntry _ = Nothing

maybeDataTypeEntry :: PDCModuleEntry -> Maybe PDCDataTypeE
maybeDataTypeEntry (PDCDataTypeEntry e) = Just e
maybeDataTypeEntry _ = Nothing

maybeRecordDataTypeEntry :: PDCDataTypeE -> Maybe PDCRecordTypeE
maybeRecordDataTypeEntry (PDCRecordTypeEntry e) = Just e
maybeRecordDataTypeEntry _ = Nothing

maybeMsgAttrTypeEntry :: PDCDataTypeE -> Maybe PDCMsgTypeE
maybeMsgAttrTypeEntry (PDCMsgTypeEntry e) = Just e
maybeMsgAttrTypeEntry _ = Nothing



filterRuleEntries :: [PDCModuleEntry] -> [PDCRuleE]
filterRuleEntries = catMaybes . map maybeRuleEntry

filterExportEntries :: [PDCModuleEntry] -> [PDCExportE]
filterExportEntries = catMaybes . map maybeExportEntry

filterDataTypeEntries :: [PDCModuleEntry] -> [PDCDataTypeE]
filterDataTypeEntries = catMaybes . map maybeDataTypeEntry

filterRecordDataTypeEntries :: [PDCDataTypeE] -> [PDCRecordTypeE]
filterRecordDataTypeEntries = catMaybes . map maybeRecordDataTypeEntry

filterMsgAttrTypeEntries :: [PDCDataTypeE] -> [PDCMsgTypeE]
filterMsgAttrTypeEntries = catMaybes . map maybeMsgAttrTypeEntry



prettyPDCRulePattern :: PDCRulePattern -> String
prettyPDCRulePattern  (PDCMsgPattern (PDCMsgP{..})) = "(" ++ (pdcid pdcMsgFrom) ++ "->" ++ (pdcid pdcMsgTo) ++ ":" ++ (pdcid pdcMsgType) ++ ")"
prettyPDCRulePattern  (PDCSeqPattern (PDCSeqP{..})) = "seq{" ++ (concat $ map prettyPDCRulePattern pdcRulePatternsSeq) ++ "}"
prettyPDCRulePattern  (PDCUnSeqPattern (PDCUnSeqP{..})) = "unseq{" ++ (concat $ map prettyPDCRulePattern pdcRulePatternsUnSeq) ++ "}"
prettyPDCRulePattern  (PDCStartInstantlyPattern (PDCStartInstantlyP{..})) = "start instantly"
prettyPDCRulePattern  (PDCOneOfPattern (PDCOneOfP{..})) = "one-of{" ++ (concat $ map prettyPDCRulePattern pdcRulePatternsOneOf) ++ "}"
prettyPDCRulePattern  (PDCManyofPattern (PDCManyOfP{..})) = "many-of{" ++ (concat $ map prettyPDCRulePattern pdcRulePatternsManyOf) ++ "}"
prettyPDCRulePattern  (PDCMoreOfPattern (PDCMoreOfP{..})) = "more-of{" ++ (concat $ map prettyPDCRulePattern pdcRulePatternsMoreOf) ++ "}"
prettyPDCRulePattern  (PDCMergePattern (PDCMergeP{..})) = "merge{" ++ (concat $ map prettyPDCRulePattern pdcRulePatternsMerge) ++ "}"
prettyPDCRulePattern  (PDCOptionalPattern (PDCOptionalP{..})) = "optinal{" ++ (prettyPDCRulePattern pdcRulePatternOptional) ++ "}"
prettyPDCRulePattern  (PDCStartPattern (PDCStartP{..})) = "start{" ++ (prettyPDCRulePattern pdcRulePatternStart) ++ "}"
prettyPDCRulePattern  (PDCCallPattern (PDCCallP{..})) = "call{" ++ (pdcid pdcRuleId) ++ "}"
--prettyPDCRulePattern  (PDCActionPattern _) = "action{..}"
--prettyPDCRulePattern  (PDCScopeOut) = "end-call"


--prettyPDCRulePattern  (PDCCallPattern (PDCCallP{..}))