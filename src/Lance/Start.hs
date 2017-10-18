module Lance.Start (
    playLance
    ) where

import ClassyPrelude
import Lance.Interface

playLance :: IO ()
playLance = tellPlayer "You are dead"
