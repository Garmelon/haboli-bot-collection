module Main where

import           Control.Monad

import           Haboli.Bots.TestBot
import           Haboli.Euphoria

main :: IO ()
main = void $ runClient defaultConfig $ testBot Nothing
