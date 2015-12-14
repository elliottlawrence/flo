module Main where

import FloGraph
import FloProgram
import HaskellProgram
import JSON

import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import System.Directory (createDirectory, removeDirectoryRecursive)
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
    let HaskellProgram modules = makeHaskellProgram byteString

    createDirectory dump
    forM_ modules $ \m ->
      writeFile (dump </> getHaskellModuleName m ++ "_.hs") (show m)

    callProcess "/usr/local/bin/ghc"
      ["-o", outputPath, "-outputdir", dump, dump </> "Main_.hs"]

    removeDirectoryRecursive dump

makeFloGraph :: B.ByteString -> FloGraph
makeFloGraph = fromMaybe (error "Couldn't parse file") . decode

makeHaskellProgram :: B.ByteString -> HaskellProgram
makeHaskellProgram = convertFloProgram . convertFloGraph . makeFloGraph
