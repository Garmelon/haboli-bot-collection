module Main where

import           Control.Monad

import           Haboli.Bots.InfoBot
import           Haboli.Euphoria

main :: IO ()
main = void $ runClient defaultConfig $ infoBot Nothing
