
{-# LANGUAGE TupleSections #-}

module PDC.KRunner where

import System.Environment
import System.Process
import System.Directory
import System.FilePath
import Data.List
import Data.Maybe

import PDC.KFormat

krunnerMain :: IO ()
krunnerMain = getArgs >>= work

work :: [String] -> IO ()
work [] = help
work [_] = help
work (k:f:confs) = do
    kfile <- readFile k
    let pfile = preproc kfile
        includes = procconf confs
    includeContents <- readIncludes includes
    let kfile' = unlines $ process pfile includeContents
        genKFile = k ++ ".proc.k"
        genPDCFile = f ++ ".proc.pdc"

    run ("[5/1] generate " ++ genKFile) $ do
        writeFile genKFile kfile'
    run ("[5/2] generate " ++ genPDCFile) $ do
        kFormatIO f genPDCFile
    runCmd "[5/3] " $ "kompile " ++ genKFile ++ " --syntax-module PDC-SYNTAX --main-module PDC-SEMANTICS"
    runCmd "[5/4] " $ "krun " ++ genPDCFile ++ " --directory " ++ (takeDirectory  k)
    run ("[5/5] delete? [Y/_]") $ do
        d <- getLine
        if d == "Y"
          then do
            putStrLn $ "remove: " ++ genKFile ++ ", " ++ genPDCFile
            removeFile genKFile
            removeFile genPDCFile
          else return ()
work _ = help

ex1 = work ["..\\..\\..\\k\\pdc-semantics.k", "..\\..\\..\\examples\\ex1.pdc", "msglist=..\\..\\..\\examples\\ex1_msglist.txt"]

-- dir = reverse . (\x-> if length x == 0 then "." else tail x) . dropWhile (/= '\\') . reverse

runCmd p m = do
    putStr p
    putStrLn m
    callCommand m
    putStrLn "done"

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
help = putStrLn "$ krunner k-file program {include=file}*\n  swap the '&include& filnename' lines"
