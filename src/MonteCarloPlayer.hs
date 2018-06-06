module MonteCarloPlayer where

    import System.Random
    import SedmaBase
    import SedmaReplay
    import Data.Map (Map, elems, insert, (!), singleton)


    fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
    fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
        where
            (j, gen') = randomR (0, i) gen
    
    fisherYates :: RandomGen g => g -> [a] -> ([a], g)
    fisherYates gen [] = ([], gen)
    fisherYates gen l = 
        toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
        where
            toElems (x, y) = (elems x, y)
            numerate = zip [1..]
            initial x gen = (singleton 0 x, gen)

    randomPlay :: (RandomGen g) => g -> Cards -> Cards -> (Int, g)
    randomPlay randGen pCards deck = 
        case result of
            --r -> (r, newRandGen)
            r | r > 0 -> (r, newRandGen)
            otherwise -> (0, newRandGen)
        where 
            (shuffled, newRandGen) = fisherYates randGen deck
            result = count $ replay (pCards ++ shuffled)

    delete :: (Eq a) => a -> [a] -> [a]
    delete _ [] = []
    delete a (x:xs) = if x == a then xs else x:delete a xs

    exists :: (Eq a) => a -> [a] -> Bool
    exists _ [] = False
    exists elem (x:xs) = (x == elem) || exists elem xs

    getRestDeck :: Cards -> Cards
    getRestDeck cs = [(Card s r) | r <- [R7 ..],s <- [Heart ..], not $ exists (Card s r) cs]

    evaluateOptions :: (RandomGen g) => g -> Cards -> Hand -> Int -> ([(Int, Card)], g)
    evaluateOptions g _ [] _ = ([], g)
    evaluateOptions gen pCards hand iter = 
        evaluateCard gen hand
        where 
            deck = getRestDeck pCards
            evaluateCard gen' [] = ([], gen')
            evaluateCard gen' (x:xs) = ((iterres, x):listRes, genNN)
                where
                    played = pCards ++ [x]
                    rdeck = delete x deck
                    evaluateIter gn 0 = (0, gn)
                    evaluateIter gn i = (res  + playedScore, nngn)
                        where 
                            (playedScore, ngn) = randomPlay gn played rdeck
                            (res, nngn) = evaluateIter ngn (i - 1)

                    (iterres, genN) = evaluateIter gen' iter
                    (listRes, genNN) = evaluateCard genN xs

    takeBestOption :: [(Int, Card)] -> Card
    takeBestOption (f:rs) = 
        winner
        where 
            takeBestOptionAcc [] max = max
            takeBestOptionAcc (opt@(score, card):opts) max@(mScore, mCard) = 
                if score > mScore 
                    then takeBestOptionAcc opts opt 
                    else takeBestOptionAcc opts max
            (_, winner) = takeBestOptionAcc rs f







    
        
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find f (x:xs) = if f x then Just x else find f xs

    

    data Tree a = TreeNode a [Tree a] | Leaf a deriving (Show, Eq)

    data MonteCarloState = MCS {
        played :: Cards,
        hand :: Hand
    }

    iterations :: Int
    iterations = 1000

    seed :: Int
    seed = 638219470

    pickCard :: Cards -> Hand -> Card
    pickCard played hand = 
        takeBestOption options
        where
            gen = mkStdGen seed
            (options, ngen) = evaluateOptions gen played hand iterations

    instance PlayerState MonteCarloState where
        initState _ cs = MCS [] cs

        updateState trick _ pc Nothing (MCS played hand) = MCS (played ++ trick) (delete pc hand)
        updateState trick _ pc (Just c) (MCS played xs) = MCS (played ++ trick) ((delete pc xs) ++ [c])


    player :: AIPlayer MonteCarloState
    player trick (MCS played hand) =
        pickCard (played ++ trick) hand
        
