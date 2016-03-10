{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeSynonymInstances,
  FlexibleInstances #-}
module Main where

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
  createDirectoryIfMissing, removeDirectoryRecursive, copyFile)
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

    -- Print out the FloProgram just for funsies
    putStrLn (showP fp)

    -- Route 1: Convert to Haskell
    useHaskellCompiler fp filePath
    -- Route 2: Convert to STG
    useSTGCompiler fp

{- Actually compile flo code using the STG machine. -}
useSTGCompiler :: FloProgram -> IO ()
useSTGCompiler fp = do
  let stg = convert fp :: STGProgram
  putStrLn (showP stg)

{- Convert flo code to Haskell and use GHC to compile it. -}
useHaskellCompiler :: FloProgram -> FilePath -> IO ()
useHaskellCompiler fp filePath = do
  let directory = takeDirectory filePath
      dump = directory </> "dump_"
      outputPath = dropExtension filePath

      -- Convert a FloProgram to a HaskellProgram
      modules = convert fp

  -- Create the dump directory and copy all the modules into it
  createDirectoryIfMissing False dump
  let modulesList = map (getFileName dump) modules
  forM_ modules $ \m -> writeFile (getFileName dump m) (showP m)

  -- Run GHC
  callProcess "/usr/local/bin/ghc" $
    ["-o", outputPath, "-outputdir", dump] ++ modulesList

  -- Remove the dump directory
  --removeDirectoryRecursive dump

getFileName :: String -> HaskellModule -> String
getFileName dump HaskellModule{..} = dump </> hmName ++ ".hs"
