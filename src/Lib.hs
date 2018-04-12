{-# LANGUAGE NoImplicitPrelude #-}
module Lib
    ( addStampCard
    , someFunc
    , stampRally1
    , stampRally1'
    )
where

import           Data.Bool      ((&&))
import           Data.Foldable  (length)
import           Data.Functor   ((<$>))
import           Data.Int       (Int)
import           Data.List      (last, (!!))
import           Data.Maybe     (Maybe (Just, Nothing), maybe)
import           Data.Semigroup ((<>))
import           Data.Tuple     (snd)
import           Data.UUID      (UUID)
import           Data.UUID.V4   (nextRandom)
import           Prelude        (Integer, Show, ($), (+), (<), (<=))
import           System.IO      (IO, print, putStrLn)

newtype SpotId = SpotId Integer deriving Show
newtype Spot = Spot SpotId deriving Show
newtype StampId = StampId UUID deriving Show
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

getSpots :: StampRally -> [Spot]
getSpots (StampRally _ spots _ _) = spots

getSpot :: StampRally -> Int -> Maybe Spot
getSpot r index = case getSpots r of
    [] -> Nothing
    xs -> if 0 <= index && index < length xs then Just (xs !! index) else Nothing

genStampId :: IO StampId
genStampId = StampId <$> nextRandom

someFunc :: IO ()
someFunc = do
    stampId <- genStampId
    print stampId
    let spot = getSpot stampRally1' 4
    print spot
    putStrLn "someFunc"
