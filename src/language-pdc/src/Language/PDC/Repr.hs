
{-# LANGUAGE RecordWildCards
           , DeriveGeneric
           , OverloadedStrings           
           #-}

module Language.PDC.Repr where

import Data.Maybe
import qualified Text.Parsec.Pos as P
import qualified Data.Aeson as JSON
import qualified Control.Applicative as JSON (empty)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)


data PDCModule
  = PDCModule
    { sourceInfoModule  :: SourceInfo
    , pdcModuleName     :: PDCId
    , pdcModuleEntries  :: [PDCModuleEntry]
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCModule
instance JSON.FromJSON PDCModule

data PDCId
  = PDCId
    { sourceInfoId      :: SourceInfo
    , pdcid             :: String
    , ulcase            :: ULCase
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCId
instance JSON.FromJSON PDCId

data ULCase = UC | LC
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON ULCase
instance JSON.FromJSON ULCase

data PDCModuleEntry
  = PDCRuleEntry        PDCRuleE
  | PDCExportEntry      PDCExportE
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCModuleEntry
instance JSON.FromJSON PDCModuleEntry

data PDCExportE
  = PDCExportE
    { sourceInfoExport  :: SourceInfo
    , pdcExportId       :: PDCId
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCExportE
instance JSON.FromJSON PDCExportE

data PDCRuleE
  = PDCRuleE
    { sourceInfoRuleEntry :: SourceInfo
    , pdcRuleName       :: PDCId
    , pdcRuleType       :: PDCRuleType
    , pdcRulePattern    :: PDCRulePattern
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCRuleE
instance JSON.FromJSON PDCRuleE

data PDCRuleType
  = PDCRuleType
    { sourceInfoRuleType :: SourceInfo
    , pdcRuleTempParams :: [PDCTempParam]
    , pdcRuleProcParams :: [PDCProcParam]
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCRuleType
instance JSON.FromJSON PDCRuleType

data PDCTempParam
  = PDCTempParam
    { pdcIdTempParam    :: PDCId
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCTempParam
instance JSON.FromJSON PDCTempParam

data PDCProcParam
  = PDCProcParam
    { pdcIdProcParam    :: PDCId
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCProcParam
instance JSON.FromJSON PDCProcParam

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
  | PDCMergePattern     PDCMergeP
  | PDCMsgPattern       PDCMsgP
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCRulePattern
instance JSON.FromJSON PDCRulePattern

data PDCMsgP
  = PDCMsgP
    { sourceInfoMsg     :: SourceInfo
    , pdcMsgFrom        :: PDCId
    , pdcMsgTo          :: PDCId
    , pdcMsgType        :: PDCId
    , pdcMsgContent     :: ()
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCMsgP
instance JSON.FromJSON PDCMsgP

data PDCStartInstantlyP
  = PDCStartInstantlyP
    { sourceInfoStartInstantly :: SourceInfo
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCStartInstantlyP
instance JSON.FromJSON PDCStartInstantlyP

data PDCSeqP
  = PDCSeqP
    { sourceInfoSeq     :: SourceInfo
    , pdcRulePatternsSeq :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCSeqP
instance JSON.FromJSON PDCSeqP

data PDCStartP
  = PDCStartP
    { sourceInfoStart   :: SourceInfo
    , pdcRulePatternStart :: PDCRulePattern
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCStartP
instance JSON.FromJSON PDCStartP

data PDCOneOfP
  = PDCOneOfP
    { sourceInfoOneOf   :: SourceInfo
    , pdcRulePatternsOneOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCOneOfP
instance JSON.FromJSON PDCOneOfP

data PDCMoreOfP
  = PDCMoreOfP
    { sourceInfoMoreOf  :: SourceInfo
    , pdcRulePatternsMoreOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCMoreOfP
instance JSON.FromJSON PDCMoreOfP

data PDCManyOfP
  = PDCManyOfP
    { sourceInfoManyOf  :: SourceInfo
    , pdcRulePatternsManyOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCManyOfP
instance JSON.FromJSON PDCManyOfP
  
data PDCInterleaveP
  = PDCInterleaveP
    { sourceInfoInterleave  :: SourceInfo
    , pdcRulePatternsInterleaveOf :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCInterleaveP
instance JSON.FromJSON PDCInterleaveP
  
data PDCOptionalP
  = PDCOptionalP
    { sourceInfoOptional :: SourceInfo
    , pdcRulePatternOptional :: PDCRulePattern
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCOptionalP
instance JSON.FromJSON PDCOptionalP
  
data PDCCallP
  = PDCCallP
    { sourceInfoCall    :: SourceInfo
    , pdcRuleId         :: PDCId
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCCallP
instance JSON.FromJSON PDCCallP
  
data PDCMergeP
  = PDCMergeP
    { sourceInfoMerge    :: SourceInfo
    , pdcRulePatternsMerge :: [PDCRulePattern]
    }
  deriving (Eq, Ord, Show, Generic)
instance JSON.ToJSON PDCMergeP
instance JSON.FromJSON PDCMergeP

data SourceInfo
  = SourceInfo
    { parsecSorceInfo :: P.SourcePos
    }
  deriving (Eq, Ord, Show, Generic)
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




