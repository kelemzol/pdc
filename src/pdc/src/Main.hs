
{-# LANGUAGE RecordWildCards
           #-}

module Main where

import Options.Applicative
import Control.Monad
import Data.Semigroup ((<>))

import Language.PDC.Parser
import Language.PDC.SemanticChecker
import Language.PDC.PDCSemanticChecker

data Options
  = Options
    { inputRuleFile :: String
    , inputMsgListFile :: String
    , msgfileParser :: String
    , debugMode :: Bool
    , showWarnings :: Bool
    , showHints :: Bool
    }
  deriving (Eq, Ord, Show)

debug :: (Show a) => Options -> String -> a -> IO ()
debug (Options {..}) sl a = when debugMode $ do
    putStrLn sl
    putStrLn $ show a
    putStrLn ""

work :: Options -> IO ()
work options@(Options {..}) = do
    debug options "options" options
    mod <- moduleParser inputRuleFile
    case mod of
        (Left e) -> putStrLn "Parser fail" >> putStrLn e
        (Right mod) -> do
            debug options "module" mod
            issues <- runPDCSemanticChecker mod
            debug options "issues" issues
            putStrLn (unlines $ concat $ map showIssue $ selectIssues options issues)
    debug options "done." ()

selectIssues :: Options -> [Issue a] -> [Issue a]
selectIssues (Options {..}) is = if showHints then filter isHint is else []
                              ++ if showWarnings then filter isWarning is else []
                              ++ (filter isError is)

showIssue :: Issue PDCSlogen -> [String]
showIssue (Hint sl desc) = ["\nHINT: " ++ showPDCSlogen sl, desc]
showIssue (Warning sl desc) = ["\bWARNING: " ++ showPDCSlogen sl, desc]
showIssue (Error sl desc) = ["\nERROR: " ++ showPDCSlogen sl, desc]





main :: IO ()
main = work =<< execParser opts
  where
    opts = info (optParser <**> helper)
      ( fullDesc
--     <> progDesc "Print a greeting for TARGET"
     <> header "pdc" )


optParser :: Parser Options
optParser = Options
    <$> strOption
        ( long "rule-file"
       <> metavar "FILE"
       <> help "input rule file" )
    <*> strOption
        ( long "msg-list-file"
       <> metavar "FILE"
       <> help "input msg list file" )
    <*> option auto
        ( long "msg-parser"
       <> metavar "PARSERNAME"
       <> help "parser of input msg list file [simple]"
       <> showDefault
       <> value "simple" )
    <*> switch
        ( long "debug-mode"
       <> help "enable debug mode")
    <*> switch
        ( long "warnings"
       <> help "show warnings")
    <*> switch
        ( long "hints"
       <> help "show hints")





