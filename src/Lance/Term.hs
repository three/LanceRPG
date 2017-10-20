module Lance.Term (
    runLanceTerm
    ) where

import Protolude
import Data.List
import Data.Functor
import Control.Concurrent.MVar
import UI.NCurses

import Lance.Util

data StateContainer = StateContainer {
    mainWindow :: Window,
    menuWindow :: MVar Window
    }

runLanceTerm :: IO ()
runLanceTerm = runCurses $ do
    setEcho False
    main <- defaultWindow
    menu <- liftIO newEmptyMVar
    let state = StateContainer {
        mainWindow = main,
        menuWindow = menu }
    updateTermFull state
    until $ getEvent main Nothing >>= \e -> case e of
        Just (EventCharacter 'q') -> return True
        Just (EventCharacter 'Q') -> return True
        Just EventResized         -> updateTermFull state $> False
        _                         -> updateTerm     state $> False

updateTermFull :: StateContainer -> Curses ()
updateTermFull state = do
    let main = mainWindow state
    liftIO (tryTakeMVar (menuWindow state)) >>= \old -> case old of
        Just m  -> closeWindow m
        Nothing -> pass
    (sizeY, sizeX) <- updateWindow main windowSize
    let posY = max 0 ((sizeY - 20) `div` 2)
        posX = max 0 ((sizeX - 80) `div` 2)
    menu <- newWindow 20 80 posY posX
    liftIO $ putMVar (menuWindow state) menu
    updateTerm state

updateTerm :: StateContainer -> Curses ()
updateTerm state = do
    let main = mainWindow state
    menu <- liftIO $ readMVar (menuWindow state)
    updateWindow menu $ do
        drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        drawStringCentered 2 80 "Lance: An RPG"
        drawStringCentered 5 80 "Press [SPACE] to Start"
        drawStringCentered 7 80 "Press Q to Quit"
    updateWindow main $ clear >> overlay menu OverlayMerge
    render

-- TODO: Create a wrapper around many Curses functions that converts String to Text and
--       include functions like drawTextAligned, etc. Also maybe use Panels?
drawStringCentered :: Integer -> Integer -> [Char] -> Update ()
drawStringCentered y xmax mes = moveCursor y ((xmax - genericLength mes) `div` 2) >> drawString mes
