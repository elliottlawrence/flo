{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import AbstractC.AbstractC
import AbstractC.Base
import AbstractC.Pretty
import Convertible
import FloGraph
import FloProgram
import HaskellProgram
import JSON
import Pretty
import STG

import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import System.Directory (
  copyFile, createDirectoryIfMissing, removeDirectoryRecursive, removeFile)
import System.Environment (getArgs)
import System.FilePath
import System.Process (callProcess)

main :: IO ()
main = do
  args <- getArgs
  if null args then error "No command line argument"
  else do
    -- Read the file and convert it to a FloProgram
    let filePath = head args
        baseName = dropExtension filePath
    byteString <- B.readFile filePath
    let fg@(FloGraph _) = fromMaybe (error "Couldn't parse file") .
                          decode $ byteString
        fp = convert fg :: FloProgram

    -- Write out the intermediate files
    writeFile (baseName <.> "fg") (showP fg)
    writeFile (baseName <.> "fp") (showP fp)

    (if "-hask" `elem` args
    -- Route 1: Convert to Haskell
    then useHaskellCompiler
    -- Route 2: Convert to STG
    else useSTGCompiler) fp baseName

{- Actually compile flo code using the STG machine. -}
useSTGCompiler :: FloProgram -> FilePath -> IO ()
useSTGCompiler fp filePath = do
      -- Convert a FloProgram to STG
  let stg = convert fp :: STGProgram
  writeFile (filePath <.> "stg") (showP stg)

      -- Convert to C and write it to a file
  let cProg = convert stg :: CProgram
      outputPath = filePath <.> "c"
  writeFile outputPath (showP cProg)

  -- Run gcc
  callProcess "/usr/bin/gcc" [outputPath, "-o", dropExtension outputPath]

  -- Remove the C file
  --removeFile outputPath

  putStrLn $ "Successfully compiled " ++ filePath <.> "flo"

{- Convert flo code to Haskell and use GHC to compile it. -}
useHaskellCompiler :: FloProgram -> FilePath -> IO ()
useHaskellCompiler fp filePath = do
  let directory = takeDirectory filePath
      dump = directory </> "dump_"
      inputFilePath = dump </> "Main" <.> "hs"

      -- Convert a FloProgram to a HaskellProgram
      hp = convert fp :: HaskellProgram

  -- Create the dump directory and copy all the modules into it
  createDirectoryIfMissing False dump
  writeFile inputFilePath (showP hp)

  -- Run GHC
  callProcess "/usr/local/bin/ghc"
    ["-o", filePath, "-outputdir", dump, inputFilePath]

  -- Remove the dump directory
  --removeDirectoryRecursive dump
