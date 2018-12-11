{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- async
import qualified Control.Concurrent.Async as Async

-- base
import Control.Applicative
import qualified Control.Concurrent as Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Fixed
import Data.Foldable
import Data.Function
import Data.Maybe
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
import Data.Time (TimeZone, UTCTime, TimeOfDay)

-- unix
import qualified System.Posix.Signals as Signals

main :: IO ()
main =
  do
    displayVar <- STM.newTVarIO ""
    Gtk.initGUI
    window :: Gtk.Window <- Gtk.windowNew
    Gtk.windowSetDefaultSize window 300 100
    drawingArea :: Gtk.DrawingArea <- Gtk.drawingAreaNew

    Gtk.set window
      [ Gtk.windowTitle := "Clock"
      , Gtk.containerChild := drawingArea
      ]

    Gtk.on window Gtk.deleteEvent $
      do
        liftIO Gtk.mainQuit
        return False

    for_ [Signals.sigINT, Signals.sigTERM] $ \s ->
        Signals.installHandler s (Signals.Catch (liftIO Gtk.mainQuit)) Nothing

    fontDescription :: Pango.FontDescription <- createFontDescription
    Gtk.on drawingArea Gtk.draw (render displayVar fontDescription drawingArea)
    Gtk.widgetShowAll window
    Async.async (runClock displayVar)
    Async.async (watchClock displayVar drawingArea)
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
    displayString <- readTVarIO displayVar
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

      writeTVarIO displayVar s
      threadDelaySeconds (1 - remainderSeconds)

-- | Get the current time of day in the system time zone.
getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay =
  do
    tz :: TimeZone <- Time.getCurrentTimeZone
    utc :: UTCTime <- Time.getCurrentTime
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

-- | A convenient wrapper for 'STM.readTVar' that lifts the operation into any
-- monad supporting 'MonadIO'.
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO =
    liftIO . STM.atomically . STM.readTVar

-- | A convenient wrapper for 'STM.writeTVar' that lifts the operation into any
-- monad supporting 'MonadIO'.
writeTVarIO :: MonadIO m => TVar a -> a -> m ()
writeTVarIO v =
    liftIO . STM.atomically . STM.writeTVar v
