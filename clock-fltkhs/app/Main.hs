{-# LANGUAGE ImplicitParams, OverloadedStrings #-}

module Main where

import            Graphics.UI.FLTK.LowLevel.Fl_Types
import qualified  Graphics.UI.FLTK.LowLevel.FLTKHS as LowLevel
import            Graphics.UI.FLTK.Theme.Light hiding (clockNew)
import            Graphics.UI.FLTK.LowLevel.Fl_Enumerations

import            Data.Fixed (Pico)
import            Data.IORef
import            Foreign.StablePtr

import qualified  Data.Time                     as Time
import qualified  Data.Time.Clock.POSIX         as Time
import qualified  Data.Time.Zones               as Time
import qualified  Data.Text                     as T
import qualified  Graphics.UI.FLTK.LowLevel.FL  as FL

clockNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.Clock)
clockNew rectangle l' = do
  ref <- newIORef Nothing
  let
    tickClock =
      do
        cMaybe <- readIORef ref
        case cMaybe of
          Nothing -> return ()
          Just c -> tick tickClock c
  c <- LowLevel.clockCustom rectangle l' (Just drawClock)
        (Just defaultCustomWidgetFuncs
        { handleCustom = Just (clockHandle tickClock) })
  writeIORef ref (Just c)
  color <- commonColor
  LowLevel.setColor c color
  LowLevel.setBox c BorderBox
  LowLevel.setLabelfont c commonFont
  LowLevel.setLabelsize c commonFontSize
  color <- commonFillColor
  LowLevel.setSelectionColor c color
  LowLevel.setShadow c True
  return c

clockHandle :: IO () -> Ref LowLevel.Clock -> Event -> IO (Either UnknownEvent ())
clockHandle tickClock clock event =
  do
    case event of
      Show -> Right <$> tickClock
      Hide -> Right <$> (FL.removeTimeout tickClock)
      _    -> return (Left UnknownEvent)

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
         (Just "Atlanta time")
  setLabelfont clock josefinSlabSemiBold
  setLabelsize clock (FontSize 24)
  setResizable window (Just clock)
  sizeRangeWithArgs window (Size (Width 200) (Height 200))
    (OptionalSizeRangeArgs (Just 400) (Just 400)
        Nothing Nothing (Just True))
  end window
  showWidget window

tick :: Ref LowLevel.Clock -> IO ()
tick clock =
  do
    utc <- Time.getCurrentTime :: IO Time.UTCTime
    tz <- Time.loadTZFromDB "America/New_York" :: IO Time.TZ
    let
      timezone = Time.timeZoneForUTCTime tz utc :: Time.TimeZone
      local = Time.utcToLocalTime timezone utc :: Time.LocalTime
      t = Time.localTimeOfDay local :: Time.TimeOfDay

      hour = Hour (Time.todHour t)
      min = Minute (Time.todMin t)
      sec = Second (floor (Time.todSec t))
      cbt = ClockByTime hour min sec

    setValue clock (ClockSetByTime cbt)
    print t
    FL.addTimeout 1 (tick clock)

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
