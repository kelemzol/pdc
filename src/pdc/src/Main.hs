
{-# LANGUAGE RecordWildCards
           #-}

module Main where

import Options.Applicative
import Control.Monad
import Data.Semigroup ((<>))
import Data.List

import Language.PDC.Parser
import Language.PDC.Repr
import Language.PDC.SemanticChecker
import Language.PDC.PDCSemanticChecker

import Language.PDC.Interpreter
import Language.PDC.Interpreter.EvalRepr


data Options
  = Options
    { inputRuleFile :: String
    , inputMsgListFile :: String
    , mainRule :: String
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
    modParseRes <- moduleParserIO inputRuleFile
    msglistParseRes <- msgListParserIO inputMsgListFile
    case (modParseRes, msglistParseRes) of
        (Left r, Left m) -> do
            putStrLn "Rule parsing fail" >> putStrLn r
            putStrLn "Msg list parsing fail" >> putStrLn m
        (Left r, _) -> putStrLn "Rule parsing fail" >> putStrLn r
        (_, Left m) -> putStrLn "Msg list parsing fail" >> putStrLn m
        (Right (PDCModule {..}), Right msglist) -> do
            case find (\ re -> pdcid (pdcRuleName re) == mainRule) $ filterRuleEntries pdcModuleEntries of
                Nothing -> putStrLn "not found main rule"
                (Just re) -> cliNode $ ast2node (pdcRulePattern re)
                    -- putStrLn $ prettyNode $ ast2node (pdcRulePattern re) -- show $ eval re msglist
            -- debug options "module" mod
            -- issues <- runPDCSemanticChecker mod
            -- debug options "issues" issues
            -- putStrLn (unlines $ concat $ map showIssue $ selectIssues options issues)
            return ()

    debug options "done." ()

selectIssues :: Options -> [Issue a] -> [Issue a]
selectIssues (Options {..}) is = if showHints then filter isHint is else []
                              ++ if showWarnings then filter isWarning is else []
                              ++ (filter isError is)

showIssue :: (Message ms) => Issue ms -> [String]
showIssue (Hint desc) = ["\nHINT: ", message2readable desc]
showIssue (Warning desc) = ["\bWARNING: ", message2readable desc]
showIssue (Error desc) = ["\nERROR: ", message2readable desc]



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
    <*> strOption
        ( long "main-rule"
       <> metavar "RULE-NAME"
       <> help "name of runnable main rule")
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





