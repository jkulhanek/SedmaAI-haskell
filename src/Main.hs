module Main where

import SedmaBase
import qualified SillyPlayer
import qualified DecisionPlayer
import qualified ProbabilisticPlayer
import qualified DecisionPlayer2
import qualified DecisionPlayer3
import SedmaGamble
import SedmaReplay
import qualified SedmaDecks

countAll p1 p2 = 
    foldl (+) 0 (map (\deck -> count (replay (gamble p1 p2 deck))) SedmaDecks.decks) -
    foldl (+) 0 (map (\deck -> count (replay (gamble p2 p1 deck))) SedmaDecks.decks)

main = do
    putStrLn $ show $ countAll p1 p2
        where p1 = DecisionPlayer3.player
              p2 = DecisionPlayer2.player