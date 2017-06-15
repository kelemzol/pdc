
{-# LANGUAGE TupleSections
           , RecordWildCards
           , ViewPatterns
           #-}

module PDC.KRunner where

import System.Environment
import System.Process
import System.Directory
import System.FilePath
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import Control.DeepSeq

import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Text.XML.Expat.Proc
-- import Text.XML.Expat.Extended
import Data.ByteString.Lazy.Char8 (pack, unpack)

import GHC.IO.Handle

import PDC.KFormat

krunnerMain :: IO ()
krunnerMain = getArgs >>= work

data Conf
  = Conf
    { pdc_def_dir :: String
    , pdc_prog_dir :: String
    , pdc_msgl_dir :: String
    , outputStr :: String -> IO ()
    , outputStrLn :: String -> IO ()
    , run_k_debug :: Bool
    }

parseconf :: String -> Conf
parseconf = pc (Conf "" "" "" putStr putStrLn False) . map words . lines
  where
    pc :: Conf -> [[String]] -> Conf
    pc c [] = c
    pc c (["pdc-def-dir",  "=", pdd]:ol) = pc c {pdc_def_dir = pdd} ol
    pc c (["pdc-prog-dir", "=", ppd]:ol) = pc c {pdc_prog_dir = ppd} ol
    pc c (["pdc-msgl-dir", "=", pmd]:ol) = pc c {pdc_msgl_dir = pmd} ol
    pc c (["output-str", "=", "stdout"]:ol) = pc c {outputStr = putStr, outputStrLn = putStrLn} ol
    pc c (["output-str", "=", out]:ol) = pc c {outputStr = appendFile out, outputStrLn = appendFile out . (++"\n")} ol
    pc c (["run-k-debug",  "=", "true"]:ol) = pc c {run_k_debug = True} ol
    pc c (["run-k-debug",  "=", "false"]:ol) = pc c {run_k_debug = False} ol
    pc c (_:ol) = pc c ol

work :: [String] -> IO ()
-- work [] = help
-- work [_] = help
work (k:f:"--delete-temp":confs) = void $ dowork k f (Just True) confs
work (k:f:"--no-delete-temp":confs) = void $ dowork k f (Just False) confs
work (k:f:confs) = void $ dowork k f Nothing confs
work _ = help


genericExpose :: (FilePath -> String -> IO ()) -> Conf -> String -> IO ()
genericExpose work conf name = do
    let path = pdc_prog_dir conf
    content <- readFile (path ++ name ++ ".test")
    doExpose path Nothing [] (lines content)
  where
    doExpose path Nothing   _ [] = return ()
    doExpose path (Just fn) c [] = work (path++fn) (unlines (reverse c))
    doExpose path Nothing c ((words -> ("#>":"expose":fn':_)):xs) = do
        doExpose path (Just fn') [] xs
    doExpose path (Just fn) c ((words -> ("#>":"expose":fn':_)):xs) = do
        work (path++fn) (unlines (reverse c))
        doExpose path (Just fn') [] xs
    doExpose path fn c (x:xs) = doExpose path fn (x:c) xs


expose = genericExpose writeFile

deleteExposed = genericExpose (\ fn _ -> removeFile fn)


dowork k' f' del confs = do
    conf <- parseconf <$> readFile "./.pdc_krunner"
    let k = pdc_def_dir conf ++ k'
        f = pdc_prog_dir conf ++ f'
    kfile <- readFile k
    let pfile = preproc kfile
        includes = map (\(a,b) -> (a, pdc_msgl_dir conf ++ b)) $ procconf confs
    includeContents <- readIncludes includes
    let kfile' = unlines $ process pfile includeContents
        genKFile = k ++ ".proc.k"
        genPDCFile = f ++ ".proc.pdc"

    run conf ("[5/1] generate " ++ genKFile) $ do
        writeFile genKFile kfile'
    run conf ("[5/2] generate " ++ genPDCFile) $ do
        kFormatIO True f genPDCFile
    (success, _, kompile_stdout, kompile_stderr) <- runCmd conf TextFormat "[5/3] " $ "kompile " ++ genKFile ++ " --syntax-module PDC-SYNTAX --main-module PDC-SEMANTICS"
    (xmlres, krun_stdout, _, krun_stderr) <- if success
        then runCmd conf XMLFormat "[5/4] " $ "krun " ++ genPDCFile ++ " --directory " ++ (takeDirectory  k) ++ (if run_k_debug conf then " --debug" else "")
        else return (False, Text "empty", "", "")
    run conf ("[5/5] delete? [Y/_]") $ do
        d <- case del of
            Nothing -> (=="Y") <$> getLine
            (Just True) -> outputStrLn conf "Y" >> return True
            (Just False) -> outputStrLn conf "_" >> return False

        if d
          then do
            outputStrLn conf $ "remove: " ++ genKFile ++ ", " ++ genPDCFile
            removeFile genKFile
            removeFile genPDCFile
          else return ()
    return Session {..}

data Session
  = Session
    { kompile_stdout :: String
    , kompile_stderr :: String
    , krun_stdout :: UNode String
    , krun_stderr :: String
    }

data Format = TextFormat | XMLFormat

runCmd :: Conf -> Format -> String -> String -> IO (Bool, UNode String, String, String)
runCmd conf f p m = do
    outputStr conf p
    outputStrLn conf m
    (_, outH, errH, _) <- runInteractiveCommand m
    out <- hGetContents outH
    err <- hGetContents errH
    (success, xmlres) <- if err == []
      then do
        xmlres <- case f of
          TextFormat -> outputStrLn conf out >> return (Text "empty")
          XMLFormat -> do
            let (xml, mErr) = parse defaultParseOptions (pack out) :: (UNode String, Maybe XMLParseError)
            outputStrLn conf $ unpack $ format $ indent 4 xml
            return xml
        return (True, xmlres)
      else do
        outputStrLn conf "stderr:"
        outputStrLn conf err
        if out == []
          then return ()
          else do outputStrLn conf "stdout:"
                  outputStrLn conf out
        return (False, Text "empty")
    outputStrLn conf "done"
    return (success, xmlres, out, err)


getRunResult :: UNode String -> Maybe String
getRunResult = fmap textContent . findElement "rulestatus"

getErrorCode :: UNode String -> Maybe String
getErrorCode = fmap textContent . findElement "error-code"


readXML :: FilePath -> IO XML
readXML fn = do
    file <- openFile fn ReadMode
    content <- hGetContents file
    content `deepseq` hClose file
    let (xml, mErr) = parse defaultParseOptions (pack content) :: (UNode String, Maybe XMLParseError)
    return (XML xml)

getBy :: XML -> String -> Maybe String
getBy (XML xml) name = fmap textContent (findElement name xml)

newtype XML = XML (UNode String)
  deriving (Eq, Show)

run conf msg m = do
    outputStrLn conf msg
    m
    outputStrLn conf "done"

process :: [Either String String] -> [(String, String)] -> [String]
process [] _ = []
process ((Left i):o) base = (fromJust (lookup (includeValue i) base)):process o base
process ((Right l):o) base = l:process o base

procconf :: [String] -> [(String, String)]
procconf = map (cut . ([],))
  where
    cut :: (String, String) -> (String, String)
    cut (a,[]) = error $ "wrong conf: " ++ reverse a
    cut (a,'=':b) = (reverse a,b)
    cut (a,c:b) = cut (c:a,b)

preproc :: String -> [Either String String]
preproc = map partition . lines
  where
    partition a = if isInfixOf "&include&" a then Left a else Right a

readIncludes :: [(String, String)] -> IO [(String, String)]
readIncludes = mapM (\(ik, iv) -> readFile iv >>= return . (ik,) . kFormat False)

includeValue :: String -> String
includeValue (' ':val) = includeValue val
includeValue ('\t':val) = includeValue val
includeValue ('&':'i':'n':'c':'l':'u':'d':'e':'&':' ':val) = val

help :: IO ()
help = putStrLn "$ krunner k-file program [--delete-temp/--no-delete-temp] {include=file}*\n  swap the '&include& filnename' lines"
