module Lance.Term (
    initLance
    ) where

import           Protolude
import           Control.Concurrent
import           System.Exit
import           UI.NCurses

initLance :: IO ()
initLance = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 3 3
        drawString "(press q to quit)"
    render
    forever $ do
        event <- getEvent w Nothing
        liftIO $ case event of
            Just (EventCharacter 'q') -> exitSuccess
            _                         -> pass
