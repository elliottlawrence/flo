{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}
module Main where

import Convertible
import FloGraph
import FloProgram
import HaskellProgram
import JSON

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

    byteString <- B.readFile filePath
    let HaskellProgram modules = convert byteString

    createDirectoryIfMissing False dump
    let modulesList = map (getFileName dump) modules
    forM_ modules $ \m -> writeFile (getFileName dump m) (show m)
    copyFile (directory </> "Prologue.hs") $ dump </> "Prologue.hs"

    callProcess "/usr/local/bin/ghc" $
      ["-o", outputPath, "-outputdir", dump] ++ modulesList ++
      [dump </> "Prologue.hs"]

    removeDirectoryRecursive dump

instance Convertible B.ByteString HaskellProgram where
  convert = convert . (convert :: FloGraph -> FloProgram) .
    fromMaybe (error "Couldn't parse file") . decode

getFileName :: String -> HaskellModule -> String
getFileName dump HaskellModule{..} = dump </> hmName ++ ".hs"
