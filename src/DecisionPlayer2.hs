module DecisionPlayer2 where

    import SedmaBase
        
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find f (x:xs) = if f x then Just x else find f xs

    delete :: (Eq a) => a -> [a] -> [a]
    delete _ [] = []
    delete a (x:xs) = if x == a then xs else x:delete a xs

    data DecisionPlayerState = DPS [Card] deriving Show

    instance PlayerState DecisionPlayerState where
        initState _ cs = DPS cs

        updateState _ _ pc Nothing (DPS xs) = DPS (delete pc xs)
        updateState _ _ pc (Just c) (DPS xs) = DPS ((delete pc xs) ++ [c])

    tfind :: a -> (a -> Bool) -> [a] -> a
    tfind d _ [] = d
    tfind d f (x:xs) = if f x then x else tfind d f xs

    returnRank :: DecisionPlayerState -> Rank -> Card
    returnRank (DPS hand@(x:xs)) rank =
        case perfectCard of
            Nothing -> x
            Just c -> c
        where 
            perfectCard = find (\(Card s r) -> rank == r) hand
    
    cardCost :: Card -> Int
    cardCost (Card _ R7) = 1
    cardCost (Card _ R10) = 1
    cardCost (Card _ RA) = 1
    cardCost (Card _ _) = 0

    returnCheap :: DecisionPlayerState -> Card
    returnCheap (DPS hand@(x:xs)) =
        retCheap xs x
        where 
            retCheap [] c = c
            retCheap (x:xs) c = if cardCost c > cardCost x then retCheap xs x else retCheap xs c

    
    returnBest :: DecisionPlayerState -> Rank -> Card
    returnBest (DPS hand@(x:xs)) rank =
        case perfectCard of
            Nothing -> case seven of
                Nothing -> x
                Just c -> c
            Just c -> c
        where 
            perfectCard = find (\(Card s r) -> rank == r) hand
            seven = find (\(Card s r) -> rank == R7) hand

    countPoints [] = 0
    countPoints ((Card _ RA):xs) = 1 + countPoints xs
    countPoints ((Card _ R10):xs) = 1 + countPoints xs
    countPoints ((Card _ _):xs) = countPoints xs

    hasSeven [] = False
    hasSeven ((Card _ r):xs) | r == R7 = True
    hasSeven ((Card _ r):xs) | otherwise = hasSeven xs
        


    player :: AIPlayer DecisionPlayerState
    player [] state = returnCheap state
    player [(Card _ R7)] state = returnCheap state
    player [(Card _ x)] state = returnRank state x

    player [(Card _ R7), (Card _ RA)] state = returnRank state R7
    player [(Card _ R7), (Card _ R10)] state = returnRank state R7
    player [(Card _ R7), (Card _ _)] state = returnCheap state
    player [(Card _ _), (Card _ R7)] state = returnCheap state
    player [c1@(Card _ x1), c2] state | countPoints [c1, c2] > 1 = returnBest state x1
    player [c1@(Card _ x1), c2] state | otherwise = returnRank state x1


    player cards state | countPoints cards > 1 && hasSeven cards  = returnRank state R7
    player cards@((Card _ x):xs) state | countPoints cards > 1 = returnBest state x
    player cards@((Card _ x):xs) state | countPoints cards > 0 && not (hasSeven cards) = returnRank state x
    player cards state | otherwise  = returnCheap state

    player _ state = returnCheap state
