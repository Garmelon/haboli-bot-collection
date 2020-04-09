{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Haboli.Bots.InfoBot
  ( infoBot
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Map.Strict          as Map
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

infoBot :: Maybe T.Text -> Client T.Text ()
infoBot mPasswd = do
  startTime <- liftIO getCurrentTime
  initialEvents <- untilConnected $
    respondingToBounce mPasswd $
    respondingToPing nextEvent
  let initialState = BotState startTime $ newListing initialEvents
  stateVar <- liftIO $ newMVar initialState
  preferNickVia botListing stateVar "InfoBot"
  updateNick stateVar
  botMain stateVar

botMain :: MVar BotState -> Client T.Text ()
botMain stateVar = forever $ do
  event <- respondingToCommands (getCommands stateVar) $
    respondingToPing nextEvent
  updateFromEventVia botListing stateVar event
  updateNick stateVar

shortHelp :: T.Text
shortHelp = "/me counts and displays connected clients in its nick"

longHelp :: T.Text
longHelp = T.concat
  [ "This bot counts how many people (P), bots (B), lurkers (L) and bot lurkers"
  , " (N) are connected to this room and displays the count in its nick."
  , " "
  , "In case you can't mention it directly, it also listens to the nick @InfoBot."
  , "\n"
  , "!recalibrate\t\tUse this command if the count seems to be off."
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
    , botrulezPingSpecific "InfoBot"

    , botrulezHelpGeneral shortHelp
    , botrulezHelpSpecific name longHelp
    , botrulezHelpSpecific "InfoBot" longHelp

    , botrulezUptimeSpecific name $ state ^. botStartTime
    , botrulezUptimeSpecific "InfoBot" $ state ^. botStartTime

    , botrulezKillSpecific name
    , botrulezKillSpecific "InfoBot"

    , cmdRecalibrate stateVar
    ]

formatNick :: Listing -> T.Text
formatNick listing =
  let views = lsSelf listing : Map.elems (lsOthers listing)
      (bots, people) = partition (\sv -> userType (svId sv) == Bot) views
      peopleLurkers = filter (T.null . svNick) people
      botLurkers = filter (T.null . svNick) people
      p = length people - l
      b = length bots - n
      l = length peopleLurkers
      n = length botLurkers
      inner = T.intercalate " " $ map T.pack $ concat
        [ [show p ++ "P" | p > 0]
        , [show b ++ "B"]
        , [show l ++ "L" | l > 0]
        , [show n ++ "N" | n > 0]
        ]
  in  "\x0001(" <> inner <> ")"

updateNick :: MVar BotState -> Client e ()
updateNick stateVar = do
  state <- liftIO $ readMVar stateVar
  let newName = formatNick $ state ^. botListing
  preferNickVia botListing stateVar newName

cmdRecalibrate :: MVar BotState -> Command e
cmdRecalibrate stateVar = cmdGeneral "recalibrate" $ \msg -> do
  sessions <- who
  updateFromListVia botListing stateVar sessions
  updateNick stateVar
  void $ reply msg "/me has recalibrated"
