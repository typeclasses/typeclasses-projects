{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- base
import qualified Control.Concurrent as Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Fixed
import Data.Foldable
import Text.Printf (printf)

-- cairo
import qualified Graphics.Rendering.Cairo as Cairo

-- gtk3
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp (..), PangoRectangle (..))

-- pango
import qualified Graphics.Rendering.Pango as Pango

-- stm
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TVar)

-- time
import qualified Data.Time as Time

-- unix
import qualified System.Posix.Signals as Signals

main :: IO ()
main =
  do
    displayVar <- STM.newTVarIO ""
    _ <- Gtk.initGUI
    window :: Gtk.Window <- Gtk.windowNew
    Gtk.windowSetDefaultSize window 300 100
    drawingArea :: Gtk.DrawingArea <- Gtk.drawingAreaNew

    Gtk.set window
      [ Gtk.windowTitle := "Clock"
      , Gtk.containerChild := drawingArea
      ]

    _ <- Gtk.on window Gtk.deleteEvent $
      do
        liftIO Gtk.mainQuit
        return False

    for_ [Signals.sigINT, Signals.sigTERM] $ \s ->
        Signals.installHandler s (Signals.Catch (liftIO Gtk.mainQuit)) Nothing

    fontDescription :: Pango.FontDescription <- createFontDescription

    _ <- Gtk.on drawingArea Gtk.draw
        (render displayVar fontDescription drawingArea)

    Gtk.widgetShowAll window

    _ <- Concurrent.forkFinally
            (runClock displayVar)
            (\_ -> Gtk.mainQuit)

    _ <- Concurrent.forkFinally
            (watchClock displayVar drawingArea)
            (\_ -> Gtk.mainQuit)

    Gtk.mainGUI

createFontDescription :: IO Pango.FontDescription
createFontDescription =
  do
    fd <- Pango.fontDescriptionNew
    Pango.fontDescriptionSetFamily fd "Fira Mono"
    Pango.fontDescriptionSetSize fd 40
    return fd

render :: Gtk.WidgetClass w
    => TVar String
        -- ^ Variable containing the text to display
    -> Pango.FontDescription
        -- ^ Font to display the text in
    -> w
        -- ^ Widget we're rendering to (needed to get the size of it)
    -> Cairo.Render ()
render displayVar fontDescription widget =
  do
    displayString <- liftIO (STM.atomically (STM.readTVar displayVar))
    Gtk.Rectangle _ _ w h <- liftIO (Gtk.widgetGetAllocation widget)
    Cairo.setSourceRGB 1 0.9 1
    Cairo.paint
    layout <- Pango.createLayout displayString
    liftIO (Pango.layoutSetFontDescription layout (Just fontDescription))
    liftIO (Pango.layoutSetAlignment layout Pango.AlignCenter)
    (_, (PangoRectangle x' y' x'' y'')) <-
        liftIO (Pango.layoutGetExtents layout)
    Cairo.moveTo ((fromIntegral w) / 2 - x'' / 2 - x')
                 ((fromIntegral h) / 2 - y'' / 2 - y')
    Cairo.setSourceRGB 0.2 0.1 0.2
    Pango.showLayout layout

-- | @'runClock' t@ is an IO action that runs forever, keeping the value of @t@
-- equal to the current time.
runClock :: TVar String -> IO ()
runClock displayVar =
  forever $
    do
      time <- getLocalTimeOfDay

      let
          (clockSeconds :: Int, remainderSeconds :: Pico) =
              properFraction (Time.todSec time)
          s =
              printf "%02d:%02d:%02d"
                  (Time.todHour time) (Time.todMin time) clockSeconds

      liftIO (STM.atomically (STM.writeTVar displayVar s))
      threadDelaySeconds (1 - remainderSeconds)

-- | Get the current time of day in the system time zone.
getLocalTimeOfDay :: IO Time.TimeOfDay
getLocalTimeOfDay =
  do
    tz :: Time.TimeZone <- Time.getCurrentTimeZone
    utc :: Time.UTCTime <- Time.getCurrentTime
    return (Time.localTimeOfDay (Time.utcToLocalTime tz utc))

-- | @'watchClock' t w@ is an IO action that runs forever. Each time the value
-- of @t@ changes, it invalidates the drawing area @w@, thus forcing it to
-- redraw and update the display.
watchClock :: Gtk.WidgetClass w => TVar String -> w -> IO ()
watchClock displayVar drawingArea =
    go ""
  where
    go :: String -> IO ()
    go s =
      do
        s' <- STM.atomically (mfilter (s /=) $ STM.readTVar displayVar)
        Gtk.postGUIAsync (invalidate drawingArea)
        go s'

-- | Invalidate (force the redrawing of) an entire widget.
invalidate :: Gtk.WidgetClass w => w -> IO ()
invalidate widget =
  do
    Gtk.Rectangle x y w h <- Gtk.widgetGetAllocation widget
    Gtk.widgetQueueDrawArea widget x y w h

-- | Block for some fixed number of seconds.
threadDelaySeconds :: RealFrac n => n -> IO ()
threadDelaySeconds =
    Concurrent.threadDelay . round . (* million)

-- | One million = 10^6.
million :: Num n => n
million =
    10 ^ (6 :: Int)
