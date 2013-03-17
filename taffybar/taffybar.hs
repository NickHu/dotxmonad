import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS2
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.NetMonitor

import Solarized

main = do
  let weatherCfg = (defaultWeatherConfig "EGBE") { weatherTemplate = "<span fgcolor='" ++ solarizedViolet ++ "'>Coventry Airport</span><span fgcolor='" ++ solarizedBlue ++ "'>:</span> <span fgcolor='" ++ solarizedGreen ++ "'>$tempC$</span><span fgcolor='" ++ solarizedBlue ++ "'>°C</span> <span fgcolor='" ++ solarizedViolet ++ "'>@</span> <span fgcolor='" ++ solarizedMagenta ++ "'>$humidity$</span><span fgcolor='" ++ solarizedBlue ++ "'>%</span> <span fgcolor='" ++ solarizedRed ++ "'>$skyCondition$</span>"}
      clock = textClockNew Nothing ("<span fgcolor='" ++ solarizedViolet ++ "'>%a</span> <span fgcolor='" ++ solarizedBlue ++ "'>%b</span> <span fgcolor='" ++ solarizedCyan ++ "'>%_d</span> <span fgcolor='" ++ solarizedYellow ++ "'>%H</span><span fgcolor='" ++ solarizedViolet ++ "'>:</span><span fgcolor='" ++ solarizedYellow ++ "'>%M</span><span fgcolor='" ++ solarizedViolet ++ "'>:</span><span fgcolor='" ++ solarizedYellow ++ "'>%S</span>") 1
      log = xmonadLogNew
      tray = systrayNew
      nowPlaying = mpris2New
      notifications = notifyAreaNew defaultNotificationConfig
      net = netMonitorNew 1 "eth0"
      weather = weatherNew weatherCfg 10
  defaultTaffybar defaultTaffybarConfig { 
                                          monitorNumber = 1
                                        , barHeight = 16
                                        , startWidgets = [ log ]
                                        , endWidgets = [ clock, weather, tray, net, nowPlaying, notifications ]
                                        }
