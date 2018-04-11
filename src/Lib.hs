module Lib
    ( someFunc
    , stampRally1
    ) where

newtype SpotId = SpotId Integer deriving Show
newtype Spot = Spot SpotId deriving Show
newtype StampId = StampId Integer deriving Show
data Stamp = Stamp StampId Spot deriving Show
newtype StampCardId = StampCardId Integer deriving Show
data StampCard = StampCard StampCardId [Stamp] deriving Show
newtype StampRallyId = StampRallyId Integer deriving Show
data StampRally = StampRally StampRallyId [Spot] [StampCard] [Stamp] deriving Show

spots1 = [Spot (SpotId 1), Spot (SpotId 2), Spot (SpotId 3)]

stampRally1 :: StampRally
stampRally1 = StampRally (StampRallyId 1) spots1 [] []

someFunc :: IO ()
someFunc = putStrLn "someFunc"
