{-# LANGUAGE ImplicitParams, OverloadedStrings #-}

module Main where

import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.Theme.Light
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations

--buttonCb :: (?assets :: Assets) => Ref Button -> IO ()
--buttonCb button = do
--  putStrLn "hey"
--  undefined


ui :: (?assets :: Assets) => IO ()
ui = do
 window <- doubleWindowNew
           (Size (Width 300) (Height 300))
           Nothing
           Nothing
 begin window
 clock <- clockNew
        (Rectangle (Position (X 50) (Y 50))
        (Size (Width 200) (Height 200)))
        (Just "theme clock")
 setLabelfont clock josefinSlabSemiBold
 setLabelsize clock (FontSize 24)
 setResizable window (Just clock)
 sizeRangeWithArgs window (Size (Width 200) (Height 200))
   (OptionalSizeRangeArgs (Just 400) (Just 400)
       Nothing Nothing (Just True))
 end window
 showWidget window

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
