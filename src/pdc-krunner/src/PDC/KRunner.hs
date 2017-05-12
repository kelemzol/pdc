
{-# LANGUAGE TupleSections
            , RecordWildCards
            #-}

module PDC.KRunner where

import System.Environment
import System.Process
import System.Directory
import System.FilePath
import Data.List
import Data.Maybe
import Control.Monad

import Text.XML.Expat.Tree
import Text.XML.Expat.Format
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
    }

parseconf :: String -> Conf
parseconf = pc (Conf "" "" "") . map words . lines
  where
    pc :: Conf -> [[String]] -> Conf
    pc c [] = c
    pc c (["pdc-def-dir",  "=", pdd]:ol) = pc c {pdc_def_dir = pdd} ol
    pc c (["pdc-prog-dir", "=", ppd]:ol) = pc c {pdc_prog_dir = ppd} ol
    pc c (["pdc-msgl-dir", "=", pmd]:ol) = pc c {pdc_msgl_dir = pmd} ol
    pc c (_:ol) = pc c ol

work :: [String] -> IO ()
work [] = help
work [_] = help
work (k:f:"--delete-temp":confs) = void $ dowork k f (Just True) confs
work (k:f:"--no-delete-temp":confs) = void $ dowork k f (Just False) confs
work (k:f:confs) = void $ dowork k f Nothing confs
work _ = help

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

    run ("[5/1] generate " ++ genKFile) $ do
        writeFile genKFile kfile'
    run ("[5/2] generate " ++ genPDCFile) $ do
        kFormatIO f genPDCFile
    (success, _, kompile_stdout, kompile_stderr) <- runCmd TextFormat "[5/3] " $ "kompile " ++ genKFile ++ " --syntax-module PDC-SYNTAX --main-module PDC-SEMANTICS"
    (xmlres, krun_stdout, _, krun_stderr) <- if success
        then runCmd XMLFormat "[5/4] " $ "krun " ++ genPDCFile ++ " --directory " ++ (takeDirectory  k)
        else return (False, Text "empty", "", "")
    run ("[5/5] delete? [Y/_]") $ do
        d <- case del of
            Nothing -> (=="Y") <$> getLine
            (Just True) -> putStrLn "Y" >> return True
            (Just False) -> putStrLn "_" >> return False

        if d
          then do
            putStrLn $ "remove: " ++ genKFile ++ ", " ++ genPDCFile
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

runCmd :: Format -> String -> String -> IO (Bool, UNode String, String, String)
runCmd f p m = do
    putStr p
    putStrLn m
    (_, outH, errH, _) <- runInteractiveCommand m
    out <- hGetContents outH
    err <- hGetContents errH
    (success, xmlres) <- if err == []
      then do
        xmlres <- case f of
          TextFormat -> putStrLn out >> return (Text "empty")
          XMLFormat -> do
            let (xml, mErr) = parse defaultParseOptions (pack out) :: (UNode String, Maybe XMLParseError)
            putStrLn $ unpack $ format $ indent 4 xml
            return xml
        return (True, xmlres)
      else do
        putStrLn "stderr:"
        putStrLn err
        if out == []
          then return ()
          else do putStrLn "stdout:"
                  putStrLn out
        return (False, Text "empty")
    putStrLn "done"
    return (success, xmlres, out, err)


getResult :: UNode String -> String
getResult xml = undefined

run msg m = do
    putStrLn msg
    m
    putStrLn "done"

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
readIncludes = mapM (\(ik, iv) -> readFile iv >>= return . (ik,) . kFormat)

includeValue :: String -> String
includeValue (' ':val) = includeValue val
includeValue ('\t':val) = includeValue val
includeValue ('&':'i':'n':'c':'l':'u':'d':'e':'&':' ':val) = val

help :: IO ()
help = putStrLn "$ krunner k-file program [--delete-temp/--no-delete-temp] {include=file}*\n  swap the '&include& filnename' lines"
