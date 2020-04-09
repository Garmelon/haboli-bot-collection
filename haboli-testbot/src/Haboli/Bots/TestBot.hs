{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Haboli.Bots.TestBot
  ( testBot
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text                as T
import           Data.Time
import           Lens.Micro.Platform

import           Haboli.Euphoria
import           Haboli.Euphoria.Botrulez

data BotState = BotState
  { _botStartTime :: UTCTime
  , _botListing   :: Listing
  } deriving (Show)

makeLenses ''BotState

testBot :: Maybe T.Text -> Client T.Text ()
testBot mPasswd = do
  startTime <- liftIO getCurrentTime
  initialEvents <- untilConnected $
    respondingToBounce mPasswd $
    respondingToPing nextEvent
  let initialState = BotState startTime $ newListing initialEvents
  stateVar <- liftIO $ newMVar initialState
  preferNickVia botListing stateVar "TestBot"
  botMain stateVar

botMain :: MVar BotState -> Client T.Text ()
botMain stateVar = forever $ do
  event <- respondingToCommands (getCommands stateVar) $
    respondingToPing nextEvent
  updateFromEventVia botListing stateVar event

longHelp :: T.Text
longHelp = T.concat
  [ "This bot exists to test various things."
  , "\n"
  , "\n"
  , "Made by @Garmy using https://github.com/Garmelon/haboli/."
  , "\n"
  , "Source code available at https://github.com/Garmelon/haboli-bot-collection."
  ]

getCommands :: MVar BotState -> Client e [Command T.Text]
getCommands stateVar = do
  state <- liftIO $ readMVar stateVar
  let name = state ^. botListing . lsSelfL . svNickL
  pure
    [ botrulezPingGeneral
    , botrulezPingSpecific name
    , botrulezHelpSpecific name longHelp
    , botrulezUptimeSpecific name $ state ^. botStartTime
    , botrulezKillSpecific name
    ]
