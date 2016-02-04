{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeSynonymInstances,
  FlexibleInstances #-}
module Main where

import Convertible
import FloGraph
import FloProgram
import HaskellProgram
import JSON
import Pretty

import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import System.Directory (
  createDirectoryIfMissing, removeDirectoryRecursive, copyFile)
import System.Environment (getArgs)
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = do
  args <- getArgs
  if null args then error "No command line argument"
  else do
    let filePath = head args
        outputPath = dropExtension filePath
        directory = takeDirectory filePath
        dump = directory </> "dump_"

    -- Read the file and convert it to a Haskell program
    byteString <- B.readFile filePath
    let fg@(FloGraph _) = fromMaybe (error "Couldn't parse file") .
                          decode $ byteString
    let fp = convert fg :: FloProgram
    let modules = convert fp
    putStrLn (showP fp)

    -- Create the dump directory and copy all the modules into it
    createDirectoryIfMissing False dump
    let modulesList = map (getFileName dump) modules
    forM_ modules $ \m -> writeFile (getFileName dump m) (showP m)
    copyFile (directory </> "Prologue.hs") $ dump </> "Prologue.hs"

    -- Run GHC
    callProcess "/usr/local/bin/ghc" $
      ["-o", outputPath, "-outputdir", dump] ++ modulesList ++
      [dump </> "Prologue.hs"]

    -- Remove the dump directory
    --removeDirectoryRecursive dump

getFileName :: String -> HaskellModule -> String
getFileName dump HaskellModule{..} = dump </> hmName ++ ".hs"
