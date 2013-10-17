import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
--import System.Taffybar.TaffyPager
import System.Taffybar.Pager (colorize)
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS2
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.NetMonitor
import System.Taffybar.Battery

import Solarized

space = " "

main = do
  let weatherCfg = (defaultWeatherConfig "EGBE") { weatherTemplate = colorize solarizedViolet "" "Coventry Airport" ++ colorize solarizedBlue "" ":" ++ space ++ colorize solarizedGreen "" "$tempC$" ++ colorize solarizedBlue "" "°C" ++ space ++ colorize solarizedViolet "" "@" ++ space ++ colorize solarizedMagenta "" "$humidity$" ++ colorize solarizedBlue "" "%" ++ space ++ colorize solarizedRed "" "$skyCondition$" }
      clock = textClockNew Nothing (colorize solarizedViolet "" "%a" ++ space ++ colorize solarizedBlue "" "%b" ++ space ++ colorize solarizedCyan "" "%_d" ++ space ++ colorize solarizedYellow "" "%H" ++ colorize solarizedViolet "" ":" ++ colorize solarizedYellow "" "%M" ++ colorize solarizedViolet "" ":" ++ colorize solarizedYellow "" "%S") 1
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
      net = netMonitorNew 1 "wlp3s0"
      weather = weatherNew weatherCfg 10
      battery = textBatteryNew (colorize solarizedRed "" "$percentage$" ++ colorize solarizedBlue "" "%" ++ colorize solarizedCyan "" " $time$") 5
  defaultTaffybar defaultTaffybarConfig { 
    monitorNumber = 0
  , barHeight = 16
  , startWidgets = [ log ]
  , endWidgets = [ clock, battery, weather, tray, net, nowPlaying, notifications ]
  }
