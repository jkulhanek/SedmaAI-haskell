module SillyPlayer where

import SedmaBase



data SillyPlayerState = SPS [Card] deriving Show

instance PlayerState SillyPlayerState where
    initState _ cs = SPS cs

    updateState _ _ _ Nothing (SPS (s:xs)) = SPS xs
    updateState _ _ _ (Just c) (SPS (s:xs)) = SPS (xs ++ [c])

play :: AIPlayer SillyPlayerState
play trick (SPS (x:xs)) = x