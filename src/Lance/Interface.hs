module Lance.Interface (
    tellPlayer
    ) where

import ClassyPrelude

tellPlayer :: Text -> IO ()
tellPlayer message = putStrLn $ "You hear a sound. It says, '" ++ message ++ "'"
