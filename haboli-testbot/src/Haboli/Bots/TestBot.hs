{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Haboli.Bots.TestBot
  ( testBot
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text                          as T
import           Data.Time
import           Lens.Micro.Platform
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Haboli.Euphoria
import           Haboli.Euphoria.Botrulez
import           Haboli.Euphoria.Command.Megaparsec

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
  event <- respondingToCommand (getCommand stateVar) $
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

getCommand :: MVar BotState -> Client e (Command T.Text)
getCommand stateVar = do
  state <- liftIO $ readMVar stateVar
  let name = state ^. botListing . lsSelfL . svNickL
  pure $ cmdSequential
    [ botrulezPingGeneral
    , botrulezPingSpecific name
    , botrulezHelpSpecific name longHelp
    , botrulezUptimeSpecific name $ state ^. botStartTime
    , botrulezKillSpecific name
    , cmdEcho name
    ]

cmdEcho :: T.Text -> Command e
cmdEcho name = cmdMega parser $ \msg text -> void $ reply msg text
  where
    parser :: Parsec () T.Text T.Text
    parser = pNick name *> space1 *> pUntilEof
