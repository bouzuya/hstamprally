module Lib
    ( addStampCard
    , someFunc
    , stampRally1
    , stampRally1'
    )
where

import           Data.List      (last)
import           Data.Maybe     (Maybe (Just, Nothing), maybe)
import           Data.Semigroup ((<>))
import           Data.Tuple     (snd)
import           Prelude        (Integer, Show, ($), (+))
import           System.IO      (IO, putStrLn)

newtype SpotId = SpotId Integer deriving Show
newtype Spot = Spot SpotId deriving Show
newtype StampId = StampId Integer deriving Show
data Stamp = Stamp StampId Spot deriving Show
newtype StampCardId = StampCardId Integer deriving Show
data StampCard = StampCard StampCardId [Stamp] deriving Show
newtype StampRallyId = StampRallyId Integer deriving Show
data StampRally = StampRally StampRallyId [Spot] [StampCard] [Stamp] deriving Show

last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = Just $ last xs

spots1 :: [Spot]
spots1 = [Spot (SpotId 1), Spot (SpotId 2), Spot (SpotId 3)]

stampRally1 :: StampRally
stampRally1 = StampRally (StampRallyId 1) spots1 [] []

addStampCard :: StampRally -> (StampCard, StampRally)
addStampCard (StampRally id spots cards stamps) =
    (newCard, StampRally id spots newCards stamps)
  where
    lastCard  = last' cards
    newCardId = StampCardId
        $ maybe 1 (\(StampCard (StampCardId id') _) -> id' + 1) lastCard
    newCard  = StampCard newCardId []
    newCards = cards <> [newCard]

stampRally1' :: StampRally
stampRally1' = snd $ addStampCard $ snd $ addStampCard stampRally1

someFunc :: IO ()
someFunc = putStrLn "someFunc"
