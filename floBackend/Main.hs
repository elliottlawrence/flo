{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Main where

import AbstractC
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
    byteString <- B.readFile filePath
    let fg@(FloGraph _) = fromMaybe (error "Couldn't parse file") .
                          decode $ byteString
        fp = convert fg :: FloProgram

    --putStrLn (showP fp)

    (if "-hask" `elem` args
    -- Route 1: Convert to Haskell
    then useHaskellCompiler
    -- Route 2: Convert to STG
    else useSTGCompiler) fp filePath

{- Actually compile flo code using the STG machine. -}
useSTGCompiler :: FloProgram -> FilePath -> IO ()
useSTGCompiler fp filePath = do
  let directory = takeDirectory filePath
      outputPath = dropExtension filePath <.> ".c"

      -- Convert a FloProgram to STG
      stg = convert fp :: STGProgram
  putStrLn (showP stg)

      -- Convert to C and write it to a file
  let cProg = convert stg :: CProgram
  writeFile outputPath (showP cProg)

  -- Run gcc
  --callProcess "/usr/bin/gcc" [outputPath, "-o", dropExtension outputPath]

  -- Remove the C file
  --removeFile outputPath

{- Convert flo code to Haskell and use GHC to compile it. -}
useHaskellCompiler :: FloProgram -> FilePath -> IO ()
useHaskellCompiler fp filePath = do
  let directory = takeDirectory filePath
      dump = directory </> "dump_"
      outputPath = dropExtension filePath
      inputFilePath = dump </> "Main" <.> ".hs"

      -- Convert a FloProgram to a HaskellProgram
      hp = convert fp :: HaskellProgram


  -- Create the dump directory and copy all the modules into it
  createDirectoryIfMissing False dump
  writeFile inputFilePath (showP hp)

  -- Run GHC
  callProcess "/usr/local/bin/ghc"
    ["-o", outputPath, "-outputdir", dump, inputFilePath]

  -- Remove the dump directory
  --removeDirectoryRecursive dump
