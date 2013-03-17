import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
import System.Taffybar.SimpleClock
import System.Taffybar.MPRIS2
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.NetMonitor

main = do
  let weatherCfg = (defaultWeatherConfig "EGBE") { weatherTemplate = "Coventry Airport: $tempC$°C @ $humidity$% $skyCondition$"}
      clock = textClockNew Nothing "<span fgcolor='cyan'>%a %b %_d %H:%M:%S</span>" 1
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
