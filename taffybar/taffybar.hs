import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
--import System.Taffybar.TaffyPager
import System.Taffybar.Pager (colorize)
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS2
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.NetMonitor
import System.Taffybar.Battery
-- import System.Taffybar.CPUMonitor (probe)

import System.Taffybar.Widgets.PollingLabel
import System.Process

import Graphics.UI.Gtk
import Data.Char (isSpace)
import Data.IORef
import Text.Printf

import System.Information.Battery
import System.Information.Memory
import System.Information.CPU2 (getCPUInfo)
import System.Information.Network (getNetInfo)

import Sound.ALSA.Mixer

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception as E
import Control.Monad (forever)

import Solarized

space = " "

iconPrefix = "/home/nick/.config/taffybar/icons/"

main = do
  Just batteryContext <- batteryContextNew
  let clock = textClockNew Nothing (colorize solarizedBase0 "" "%l" ++ colorize solarizedBase01 "" ":" ++ colorize solarizedBase0 "" "%M" ++ space ++ colorize solarizedBase01 "" "%p") 1
      calendar = textClockNew Nothing ("%a %e %h") 60
      log = xmonadLogNew
    -- pager = taffyPagerNew defaultPagerConfig
    --   { activeWindow = colorize solarizedBlue "" . escape . shorten 128
    --   , activeLayout = colorize solarizedViolet "" . escape
    --   , activeWorkspace = colorize solarizedMagenta "" . escape
    --   , hiddenWorkspace = colorize solarizedBase01 "" . escape
    --   , emptyWorkspace = colorize solarizedBase00 "" . escape
    --   , visibleWorkspace = colorize solarizedGreen "" . escape
    --   , urgentWorkspace = colorize solarizedOrange "" . wrap "!" "!" . escape
    --   , widgetSep = colorize solarizedCyan "" " | "
    --   }
      tray = systrayNew
      nowPlaying = mpris2New
      notifications = notifyAreaNew defaultNotificationConfig
      gmailc = shellWidgetNew "…" "gmailc" 5
      cpu = textCPUNew ("…" ++ colorize solarizedBase01 "" "%") "cpu" 1
      battery = textBatteryNew ("$percentage$" ++ colorize solarizedBase01 "" "%") 5
      memory = textMemNew 1
      volume = textVolumeNew "" "Master" 1
      netDown = downNetMonitorNew 1 "wlp3s0"
      netUp = upNetMonitorNew 1 "wlp3s0"

      clockIcon = iconImageWidgetNew $ iconPrefix ++ "clock.xpm"
      calendarIcon = iconImageWidgetNew $ iconPrefix ++ "cal.xpm"
      mailIcon = iconImageWidgetNew $ iconPrefix ++ "mail.xpm"
      cpuIcon = iconImageWidgetNew $ iconPrefix ++ "cpu.xpm"
      batteryIcon = pollingIconImageWidgetNew "" 1 $ batteryIconFunc batteryContext iconPrefix
      memoryIcon = iconImageWidgetNew $ iconPrefix ++ "mem.xpm"
      volumeIcon = iconImageWidgetNew $ iconPrefix ++ "spkr_01.xpm"
      netDownIcon = iconImageWidgetNew $ iconPrefix ++ "net_down_03.xpm"
      netUpIcon = iconImageWidgetNew $ iconPrefix ++ "net_up_03.xpm"

      separator = textWidgetNew "|"

  defaultTaffybar defaultTaffybarConfig { 
    monitorNumber = 0
  , barHeight = 16
  , startWidgets = [ log ]
  , endWidgets = [ clock, clockIcon, separator, calendar, calendarIcon, separator, battery, batteryIcon, separator, memory, memoryIcon, separator, cpu, cpuIcon, separator, netDown, netDownIcon, netUp, netUpIcon, separator, gmailc, mailIcon, separator, volume, volumeIcon, nowPlaying, tray, notifications ]
  }

-- Volume

textVolumeNew :: String -> String -> Double -> IO Widget
textVolumeNew defaultStr name interval = do
  label <- pollingLabelNew defaultStr interval $ getVolume name
  widgetShowAll label
  return $ toWidget label

getVolume :: String -> IO String
getVolume name = do
  Just control <- getControlByName "default" name
  let Just playbackVolume = playback $ volume control
  let Just playbackMute = playback $ switch control
  (_, max) <- getRange playbackVolume
  Just vol <- getChannel FrontLeft $ value $ playbackVolume
  Just mute <- getChannel FrontLeft playbackMute
  if mute == False then return $ colorize solarizedBase01 "" "Mute"
  else return $ show (round $ (fromIntegral vol / fromIntegral max) * 100) ++ colorize solarizedBase01 "" "%"

-- Net

downNetMonitorNew :: Double -> String -> IO Widget
downNetMonitorNew interval interface = do
  sample <- newIORef 0
  label <- pollingLabelNew "" interval $ getNetDown sample interval interface
  widgetShowAll label
  return $ toWidget label

upNetMonitorNew :: Double -> String -> IO Widget
upNetMonitorNew interval interface = do
  sample <- newIORef 0
  label <- pollingLabelNew "" interval $ getNetUp sample interval interface
  widgetShowAll label
  return $ toWidget label

getNetDown :: IORef Integer -> Double -> String -> IO String
getNetDown sample interval interface = do
  [new, _] <- getNetInfo interface
  old <- readIORef sample
  writeIORef sample new
  let delta = new - old
      incoming = fromIntegral delta/(interval*1e3)
  if old == 0 then return $ "…………" ++ colorize solarizedBase01 "" "KB/s"
  else return $ (take 4 $ printf "%.2f" incoming) ++ colorize solarizedBase01 "" "KB/s"

getNetUp :: IORef Integer -> Double -> String -> IO String
getNetUp sample interval interface = do
  [_, new] <- getNetInfo interface
  old <- readIORef sample
  writeIORef sample new
  let delta = new - old
      outgoing = fromIntegral delta/(interval*1e3)
  if old == 0 then return $ "…………" ++ colorize solarizedBase01 "" "KB/s"
  else return $ (take 4 $ printf "%.2f" outgoing) ++ colorize solarizedBase01 "" "KB/s"

-- CPU

textCPUNew :: String -> String -> Double -> IO Widget
textCPUNew defaultStr cpu interval = do
  label <- pollingLabelNew defaultStr interval $ cpuPct cpu interval
  widgetShowAll label
  return $ toWidget label

cpuPct :: String -> Double -> IO String
cpuPct cpu interval = do
  oldInfo <- getCPUInfo cpu
  threadDelay $ floor (interval * 1000000)
  newInfo <- getCPUInfo cpu
  return $ show (round $ fromIntegral ((cpuTotalDiff oldInfo newInfo) - (cpuIdleDiff oldInfo newInfo)) / fromIntegral (cpuTotalDiff oldInfo newInfo) * 100) ++ colorize solarizedBase01 "" "%"

cpuIdleDiff :: [Integer] -> [Integer] -> Integer
cpuIdleDiff old new = new!!3 - old!!3

cpuTotalDiff :: [Integer] -> [Integer] -> Integer
cpuTotalDiff old new = (sum $ take 8 new) - (sum $ take 8 old)

-- Memory

textMemNew :: Double -> IO Widget
textMemNew interval = do
  label <- pollingLabelNew "" interval memPct
  widgetShowAll label
  return $ toWidget label

memPct :: IO String
memPct = do
  mem <- parseMeminfo
  return $ show (round $ (memoryUsedRatio mem) * 100) ++ colorize solarizedBase01 "" "%"

-- Battery

batteryIconFunc :: BatteryContext -> String -> IO String
batteryIconFunc ctxt prefix = do
  ac <- batteryAC ctxt
  percentage <- batteryPercent ctxt
  case () of
    _ | ac == BatteryStateCharging -> return $ prefix ++ "ac_01.xpm"
      | percentage < 0.1 -> return $ prefix ++ "bat_empty_02.xpm"
      | percentage < 0.4 -> return $ prefix ++ "bat_low_02.xpm"
      | otherwise -> return $ prefix ++ "bat_full_02.xpm"

batteryPercent :: BatteryContext -> IO Double
batteryPercent ctxt = do
  info <- getBatteryInfo ctxt
  return (batteryPercentage info / 100)

batteryAC :: BatteryContext -> IO BatteryState
batteryAC ctxt = do
  info <- getBatteryInfo ctxt
  return $ batteryState info

-- Icons

pollingIconImageWidgetNew :: FilePath -> Double -> IO FilePath -> IO Widget
pollingIconImageWidgetNew path interval cmd = do
  box <- hBoxNew False 0
  icon <- imageNewFromFile path
  _ <- on icon realize $ do
    _ <- forkIO $ forever $ do
      let tryUpdate = do
            str <- cmd
            postGUIAsync $ imageSetFromFile icon str
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (interval * 1000000)
    return ()
  boxPackStart box icon PackNatural 0
  widgetShowAll box
  return $ toWidget box

iconImageWidgetNew :: FilePath -> IO Widget
iconImageWidgetNew path = do
  box <- hBoxNew False 0
  icon <- imageNewFromFile path
  boxPackStart box icon PackNatural 0
  widgetShowAll box
  return $ toWidget box

-- iconPixWidgetNew :: FilePath -> IO Widget
-- iconPixWidgetNew path = do
--   box <- hBoxNew False 0
--   myPixbuf <- pixbufNewFromFile path
--   icon <- imageNewFromPixbuf myPixbuf
--   boxPackStart box icon PackNatural 0
--   widgetShowAll box
--   return $ toWidget box

-- Text-based

textWidgetNew :: String -> IO Widget
textWidgetNew str = do
  box <- hBoxNew False 0
  label <- labelNew $ Just str
  boxPackStart box label PackNatural 0
  widgetShowAll box
  return $ toWidget box

shellWidgetNew :: String -> String -> Double -> IO Widget
shellWidgetNew defaultStr cmd interval = do
  label <- pollingLabelNew defaultStr interval $ stripStr $ readProcess cmd [] []
  widgetShowAll label
  return $ toWidget label

-- Utils

labelStr :: String -> IO String -> IO String
labelStr label ioString = do
  str <- ioString
  return $ label ++ (rstrip str)

stripStr :: IO String -> IO String
stripStr ioString = do
  str <- ioString
  return $ rstrip $ str

rstrip = reverse . dropWhile isSpace . reverse

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()
