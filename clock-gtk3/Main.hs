{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase, DeriveFunctor, ScopedTypeVariables, TypeApplications #-}

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
import Graphics.UI.Gtk (AttrOp ((:=)))

-- linear
import Linear.V2

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

    timeVar <- STM.atomically (STM.newTVar Nothing)
    _ <- Gtk.initGUI

    drawingArea :: Gtk.DrawingArea <- Gtk.drawingAreaNew
    Gtk.set drawingArea
      [ Gtk.widgetWidthRequest := 300
      , Gtk.widgetHeightRequest := 100
      ]
    _ <- Gtk.on drawingArea Gtk.draw (render timeVar)

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
    Gtk.widgetShowAll window

    runUntilQuit (runClock timeVar)
    runUntilQuit (watchClock timeVar drawingArea)

    Gtk.mainGUI

runUntilQuit :: IO a -> IO ()
runUntilQuit x =
  do
    _threadId <- Concurrent.forkFinally x (\_ -> Gtk.mainQuit)
    return ()

render :: STM.TVar (Maybe (DisplayTime Int)) -> Cairo.Render ()
render timeVar =
  do
    renderBackground
    renderText timeVar

renderBackground :: Cairo.Render ()
renderBackground =
  do
    Cairo.setSourceRGB 1 0.9 1
    Cairo.paint

renderText :: STM.TVar (Maybe (DisplayTime Int)) -> Cairo.Render ()
renderText timeVar =
  do
    displayTimeMaybe <- liftIO (STM.atomically (STM.readTVar timeVar))
    layout <- Pango.createLayout (maybe "" showDisplayTime displayTimeMaybe)

    liftIO $
      do
        fd <- Pango.fontDescriptionNew
        Pango.fontDescriptionSetFamily fd "Fira Mono"
        Pango.fontDescriptionSetSize fd 40
        Pango.layoutSetFontDescription layout (Just fd)

    Cairo.setSourceRGB 0.2 0.1 0.2
    showPangoCenter layout

showPangoCenter :: Gtk.PangoLayout -> Cairo.Render ()
showPangoCenter layout =
    Gtk.getClipRectangle >>=
    \case
        Nothing -> return ()
        Just clip ->
          do
            (_, text) <- liftIO (Pango.layoutGetExtents layout)

            cairoMoveTo $
                rectCenter (gtkRect clip) -
                rectCenter (pangoRect text)

            Pango.showLayout layout

cairoMoveTo :: V2 Double -> Cairo.Render ()
cairoMoveTo (V2 x y) = Cairo.moveTo x y

data Rect a = Rect { rectTL :: V2 a, rectSize :: V2 a } deriving Functor

rectCenter :: (Real a, Fractional b) => Rect a -> V2 b
rectCenter x = let y = realToFrac <$> x in rectTL y + (rectSize y / 2)

gtkRect :: Gtk.Rectangle -> Rect Int
gtkRect (Gtk.Rectangle x y w h) = Rect (V2 x y) (V2 w h)

pangoRect :: Gtk.PangoRectangle -> Rect Double
pangoRect (Gtk.PangoRectangle x y w h) = Rect (V2 x y) (V2 w h)

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

-- | @'runClock' t@ is an IO action that runs forever, keeping the value of @t@
-- equal to the current time.
runClock :: STM.TVar (Maybe (DisplayTime Int)) -> IO ()
runClock timeVar =
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

      liftIO (STM.atomically (STM.writeTVar timeVar (Just displayTime)))
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
watchClock timeVar drawingArea =
    go Nothing
  where
    go :: Maybe (DisplayTime Int) -> IO ()
    go s =
      do
        s' <- STM.atomically (mfilter (s /=) (STM.readTVar timeVar))
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
