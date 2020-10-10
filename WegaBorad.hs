{-# LANGUAGE OverloadedStrings #-}

module Haboli.Euphoria.WegaBorad where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T
import           Haboli.Euphoria.Api
import           Haboli.Euphoria.Client

{- Range stuff -}

data Range = Range Char Char
  deriving (Eq)

instance Show Range where
  show (Range a b)
    | a == b    = [a]
    | otherwise = "[" ++ [a] ++ ".." ++ [b] ++ "]"

letterRange :: Char -> Char -> Range
letterRange a b =
  let realA = chr $ max (ord 'a') $ min (ord 'z') $ ord a
      realB = chr $ max (ord 'a') $ min (ord 'z') $ ord b
  in  Range realA realB

fullRange :: Range
fullRange = Range 'a' 'z'

getRangeChar :: Range -> Maybe Char
getRangeChar (Range a b)
  | a == b = Just a
  | otherwise = Nothing

splitRange :: Int -> Range -> [Range]
splitRange steps (Range a b) =
  let amount = ord b - ord a + 1
      width = amount `div` steps
      leftover = amount `mod` steps
      widths = zipWith (+) (replicate steps width) (replicate leftover 1 ++ repeat 0)
      skips = scanl (+) 0 (init widths)
  in  nub $ zipWith (\s w -> letterRange (chr $ ord a + s) (chr $ ord a + s + w - 1)) skips widths

{- Bot logic -}

data Search = Search
  { searchStartMsg :: Message
  , searchOptions  :: Map.Map Snowflake Range
  } deriving (Show)

data MyState = MyState
  { msSplitInto     :: Int
  , msCurrentSearch :: Maybe Search
  } deriving (Show)

defaultState :: MyState
defaultState = MyState 3 Nothing

type MyClient a = StateT MyState (Client ()) a

wegaBot :: MyClient ()
wegaBot = forever $ do
  event <- lift $ respondingToPing nextEvent
  case event of
    EventSnapshot _ -> void $ lift $ nick "WegaBot"
    EventSend e     -> onMessage (sendMessage e)
    _               -> pure ()

runWegaBot :: MyState -> MyClient a -> Client () a
runWegaBot start bot = fst <$> runStateT bot start

onMessage :: Message -> MyClient ()
onMessage msg
  | msgContent msg == "!wega" = startNewWega msg
  | otherwise = do
      s <- get
      for_ (msCurrentSearch s) $ \search -> do
        let maybeRange = do
              parent <- msgParent msg
              searchOptions search Map.!? parent
        for_ maybeRange $ closeInOn search msg

startNewWega :: Message -> MyClient ()
startNewWega msg = do
  startMsg <- lift $ reply msg "New character!"
  closeInOn (Search msg Map.empty) startMsg fullRange

-- | @'closeInOn' search msg range@ closes in on the currently running @search@,
-- where @msg@ has just selected @range@.
closeInOn :: Search -> Message -> Range -> MyClient ()
closeInOn search msg range =
  case getRangeChar range of
    Just char -> do
      void $ lift $ reply msg $ "You've selected " <> T.pack (show char)
      startNewWega $ searchStartMsg search
    Nothing -> do
      s <- get
      let possibleRanges = splitRange (msSplitInto s) range
      rangesWithMessageIds <- mapM (sendRange msg) possibleRanges
      let options = Map.fromList rangesWithMessageIds
      put s{msCurrentSearch = Just search{searchOptions = options}}

sendRange :: Message -> Range -> MyClient (Snowflake, Range)
sendRange msg range = do
  msg' <- lift $ reply msg $ "Reply to this message to choose " <> T.pack (show range) <> "."
  pure (msgId msg', range)
