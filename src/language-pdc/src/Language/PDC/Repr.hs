
{-# LANGUAGE RecordWildCards
           #-}

module Language.PDC.Repr where

import Data.Maybe
import qualified Text.Parsec.Pos as P


data PDCModule
  = PDCModule
    { sourceInfoModule  :: SourceInfo
    , pdcModuleName     :: PDCId
    , pdcModuleEntries  :: [PDCModuleEntry]
    }
  deriving (Eq, Ord, Show)

data PDCId
  = PDCId
    { sourceInfoId      :: SourceInfo
    , pdcid             :: String
    , ulcase            :: ULCase
    }
  deriving (Eq, Ord, Show)

data ULCase = UC | LC
  deriving (Eq, Ord, Show)

data PDCModuleEntry
  = PDCRuleEntry        PDCRuleE
  | PDCExportEntry      PDCExportE
  deriving (Eq, Ord, Show)

data PDCExportE
  = PDCExportE
    { sourceInfoExport  :: SourceInfo
    , pdcExportId       :: PDCId
    }
  deriving (Eq, Ord, Show)

data PDCRuleE
  = PDCRuleE
    { sourceInfoRuleEntry :: SourceInfo
    , pdcRuleName       :: PDCId
    , pdcRuleType       :: PDCRuleType
    , pdcRulePattern    :: PDCRulePattern
    }
  deriving (Eq, Ord, Show)

data PDCRuleType
  = PDCRuleType
    { sourceInfoRuleType :: SourceInfo
    , pdcRuleTempParams :: [PDCTempParam]
    , pdcRuleProcParams :: [PDCProcParam]
    }
  deriving (Eq, Ord, Show)

data PDCTempParam
  = PDCTempParam
    { pdcIdTempParam    :: PDCId
    }
  deriving (Eq, Ord, Show)

data PDCProcParam
  = PDCProcParam
    { pdcIdProcParam    :: PDCId
    }
  deriving (Eq, Ord, Show)


data PDCRulePattern
  = PDCSeqPattern       PDCSeqP
  | PDCStartPattern     PDCStartP
  | PDCStartInstantlyPattern PDCStartInstantlyP
  | PDCOneOfPattern     PDCOneOfP
  | PDCMoreOfPattern    PDCMoreOfP
  | PDCManyofPattern    PDCManyOfP
  | PDCInterleavePattern PDCInterleaveP
  | PDCOptionalPattern  PDCOptionalP
  | PDCCallPattern      PDCCallP
  | PDCMsgPattern       PDCMsgP
  deriving (Eq, Ord, Show)

data PDCMsgP
  = PDCMsgP
    { sourceInfoMsg     :: SourceInfo
    , pdcMsgFrom        :: PDCId
    , pdcMsgTo          :: PDCId
    , pdcMsgType        :: PDCId
    , pdcMsgContent     :: ()
    }
  deriving (Eq, Ord, Show)

data PDCStartInstantlyP
  = PDCStartInstantlyP
    { sourceInfoStartInstantly :: SourceInfo
    }
  deriving (Eq, Ord, Show)

data PDCSeqP
  = PDCSeqP
    { sourceInfoSeq     :: SourceInfo
    , pdcRulePatternsSeq :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show)

data PDCStartP
  = PDCStartP
    { sourceInfoStart   :: SourceInfo
    , pdcRulePatternStart :: PDCRulePattern
    }
  deriving (Eq, Ord, Show)

data PDCOneOfP
  = PDCOneOfP
    { sourceInfoOneOf   :: SourceInfo
    , pdcRulePatternsOneOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show)

data PDCMoreOfP
  = PDCMoreOfP
    { sourceInfoMoreOf  :: SourceInfo
    , pdcRulePatternsMoreOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show)

data PDCManyOfP
  = PDCManyOfP
    { sourceInfoManyOf  :: SourceInfo
    , pdcRulePatternsManyOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show)

data PDCInterleaveP
  = PDCInterleaveP
    { sourceInfoInterleave  :: SourceInfo
    , pdcRulePatternsInterleaveOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show)

data PDCOptionalP
  = PDCOptionalP
    { sourceInfoOptional :: SourceInfo
    , pdcRulePatternOptional :: PDCRulePattern
    }
  deriving (Eq, Ord, Show)

data PDCCallP
  = PDCCallP
    { sourceInfoCall    :: SourceInfo
    , pdcRuleId         :: PDCId
    }
  deriving (Eq, Ord, Show)

data SourceInfo
  = SourceInfo
    { parsecSorceInfo :: P.SourcePos
    }
  deriving (Eq, Ord, Show)


class GetRuleName a where
    getRuleName :: a -> PDCId

instance GetRuleName PDCExportE where
    getRuleName = pdcExportId
instance GetRuleName PDCRuleE where
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
instance GetSourceInfo PDCInterleaveP where 
    getSourceInfo = sourceInfoInterleave
instance GetSourceInfo PDCOptionalP where 
    getSourceInfo = sourceInfoOptional
instance GetSourceInfo PDCCallP where 
    getSourceInfo = sourceInfoCall



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

filterRuleEntries :: [PDCModuleEntry] -> [PDCRuleE]
filterRuleEntries = catMaybes . map maybeRuleEntry

filterExportEntries :: [PDCModuleEntry] -> [PDCExportE]
filterExportEntries = catMaybes . map maybeExportEntry




