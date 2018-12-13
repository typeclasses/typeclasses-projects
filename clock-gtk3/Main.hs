{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeApplications #-}

module Main (main) where

-- base
import qualified Control.Concurrent as Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Fixed

-- cairo
import qualified Graphics.Rendering.Cairo as Cairo

-- gtk3
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp ((:=)), PangoRectangle (..))

-- pango
import qualified Graphics.Rendering.Pango as Pango

-- stm
import qualified Control.Concurrent.STM as STM

-- time
import qualified Data.Time as Time

-- unix
import qualified System.Posix.Signals as Signals

data DisplayTime a =
    DisplayTime
        { displayHour :: a
        , displayMinute :: a
        , displaySecond :: a
        }
    deriving Eq

twoDigits :: Int -> String
twoDigits x | x < 0 = "??"
twoDigits x =
    case (show @Int x) of
        [a]    -> ['0', a]
        [a, b] -> [a, b]
        _      -> "??"

showDisplayTime :: DisplayTime Int -> String
showDisplayTime (DisplayTime x y z) =
    twoDigits x <> ":" <>
    twoDigits y <> ":" <>
    twoDigits z

main :: IO ()
main =
  do
    quitOnInterrupt

    displayVar <- STM.atomically (STM.newTVar Nothing)
    _ <- Gtk.initGUI

    drawingArea :: Gtk.DrawingArea <- Gtk.drawingAreaNew
    Gtk.set drawingArea
      [ Gtk.widgetWidthRequest := 300
      , Gtk.widgetHeightRequest := 100
      ]

    frame <- Gtk.frameNew
    Gtk.set frame
      [ Gtk.containerChild := drawingArea
      , Gtk.frameLabel := "What time is it"
      , Gtk.frameLabelXAlign := 0.5
      , Gtk.frameLabelYAlign := 1
      ]

    window :: Gtk.Window <- Gtk.windowNew
    Gtk.set window
      [ Gtk.windowTitle := "Clock"
      , Gtk.containerChild := frame
      , Gtk.windowDefaultWidth := 500
      , Gtk.windowDefaultHeight := 250
      ]

    quitOnWindowClose window

    fontDescription :: Pango.FontDescription <- createFontDescription

    _ <- Gtk.on drawingArea Gtk.draw (render displayVar fontDescription)

    Gtk.widgetShowAll window

    runUntilQuit (runClock displayVar)
    runUntilQuit (watchClock displayVar drawingArea)

    Gtk.mainGUI

runUntilQuit :: IO a -> IO ()
runUntilQuit x =
  do
    _threadId <- Concurrent.forkFinally x (\_ -> Gtk.mainQuit)
    return ()

quitOnWindowClose :: Gtk.Window -> IO ()
quitOnWindowClose window =
  do
    _connectId <- Gtk.on window Gtk.deleteEvent action
    return ()
  where
    action =
      do
        liftIO (Gtk.postGUIAsync Gtk.mainQuit)
        return False

quitOnInterrupt :: IO ()
quitOnInterrupt =
  do
    _oldHandler <- Signals.installHandler Signals.sigINT quitHandler Nothing
    return ()
  where
    quitHandler :: Signals.Handler
    quitHandler = Signals.Catch (liftIO (Gtk.postGUIAsync Gtk.mainQuit))

createFontDescription :: IO Pango.FontDescription
createFontDescription =
  do
    fd <- Pango.fontDescriptionNew
    Pango.fontDescriptionSetFamily fd "Fira Mono"
    Pango.fontDescriptionSetSize fd 40
    return fd

render
    :: STM.TVar (Maybe (DisplayTime Int))
          -- ^ Variable containing the text to display
    -> Pango.FontDescription
          -- ^ Font to display the text in
    -> Cairo.Render ()

render displayVar fontDescription =
  do
    displayTimeMaybe <- liftIO (STM.atomically (STM.readTVar displayVar))
    Gtk.getClipRectangle >>= \case
      Nothing -> return ()
      Just (Gtk.Rectangle x y w h) ->
        do
          Cairo.setSourceRGB 1 0.9 1
          Cairo.paint
          layout <- Pango.createLayout (maybe "" showDisplayTime displayTimeMaybe)
          liftIO (Pango.layoutSetFontDescription layout (Just fontDescription))
          liftIO (Pango.layoutSetAlignment layout Pango.AlignCenter)
          (_, (PangoRectangle x' y' x'' y'')) <-
              liftIO (Pango.layoutGetExtents layout)
          Cairo.moveTo ((fromIntegral (x + w)) / 2 - x'' / 2 - x')
                       ((fromIntegral (y + h)) / 2 - y'' / 2 - y')
          Cairo.setSourceRGB 0.2 0.1 0.2
          Pango.showLayout layout

-- | @'runClock' t@ is an IO action that runs forever, keeping the value of @t@
-- equal to the current time.
runClock :: STM.TVar (Maybe (DisplayTime Int)) -> IO ()
runClock displayVar =
  forever $
    do
      time <- getLocalTimeOfDay

      let
          (clockSeconds :: Int, remainderSeconds :: Pico) =
              properFraction (Time.todSec time)
          displayTime =
              DisplayTime
                  { displayHour = Time.todHour time
                  , displayMinute = Time.todMin time
                  , displaySecond = clockSeconds
                  }

      liftIO (STM.atomically (STM.writeTVar displayVar (Just displayTime)))
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
watchClock :: Gtk.WidgetClass w
    => STM.TVar (Maybe (DisplayTime Int)) -> w -> IO ()
watchClock displayVar drawingArea =
    go Nothing
  where
    go :: Maybe (DisplayTime Int) -> IO ()
    go s =
      do
        s' <- STM.atomically (mfilter (s /=) (STM.readTVar displayVar))
        Gtk.postGUIAsync (invalidate drawingArea)
        go s'

-- | Invalidate (force the redrawing of) an entire widget.
invalidate :: Gtk.WidgetClass w => w -> IO ()
invalidate widget =
  do
    Gtk.Rectangle _x _y w h <- Gtk.widgetGetAllocation widget
    Gtk.widgetQueueDrawArea widget 0 0 w h

-- | Block for some fixed number of seconds.
threadDelaySeconds :: RealFrac n => n -> IO ()
threadDelaySeconds =
    Concurrent.threadDelay . round . (* million)

-- | One million = 10^6.
million :: Num n => n
million =
    10 ^ (6 :: Int)
