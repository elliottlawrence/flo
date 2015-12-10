module Main where

import FloGraph
import FloProgram
import JSON

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args then error "No command line argument"
  else do
    byteString <- B.readFile (head args)
    let floGraph = makeFloGraph byteString
    let floProgram = convertFloGraph floGraph
    print floProgram
    return ()

makeFloGraph :: B.ByteString -> FloGraph
makeFloGraph = fromMaybe (error "Couldn't parse file") . decode
