{-# LANGUAGE ImplicitParams, OverloadedStrings #-}

module Main where

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.Theme.Light

--buttonCb :: (?assets :: Assets) => Ref Button -> IO ()
--buttonCb b' = do
--  l' <- getLabel b'
--  if (l' == "Hello world")
--    then setLabel b' "Goodbye world"
--    else setLabel b' "Hello world"

ui :: (?assets :: Assets) => IO ()
ui = do
 window <- windowNew
           (Size (Width 300) (Height 300))
           Nothing
           (Just "clock")
 begin window
 clock <- clockNew
        (Rectangle (Position (X 50) (Y 50)) (Size (Width 200) (Height 200)))
        (Just "theme clock")
 end window
 showWidget window

centerPls :: Size -> Rectangle -> Rectangle
centerPls w c = undefined

main :: IO ()
main = do
  assets <- configureTheme
  let ?assets = assets
  ui
  FL.run
  FL.flush

replMain :: IO ()
replMain = do
  assets <- configureTheme
  let ?assets = assets
  ui
  FL.replRun
