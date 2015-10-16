module Main where

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
  return window # set UI.title "Hello World!"
  button <- UI.button # set UI.text "Click me!"
  getBody window #+ [element button]
  on UI.click button $ const $
      element button # set UI.text "I have been clicked!"
