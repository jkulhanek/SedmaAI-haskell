module DecisionPlayer3 where

    import SedmaBase
        
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find f (x:xs) = if f x then Just x else find f xs

    hasAny :: (a -> Bool) -> [a] -> Bool
    hasAny _ _ = False
    hasAny f (x:xs) = if f x then True else hasAny f xs

    getBest :: (a -> Int) -> [a] -> a
    getBest f (l:ls) =
        gB ls (l, f l)
        where
            gB [] (a, _) = a
            gB (x:xs) (card, cost) = if f x > cost then gB xs (x, f x) else gB xs (card, cost)

    countAll :: (a -> Bool) -> [a] -> Int
    countAll _ _ = 0
    countAll f (x:xs) = if (f x) then 1 + (countAll f xs) else countAll f xs

    delete :: (Eq a) => a -> [a] -> [a]
    delete _ [] = []
    delete a (x:xs) = if x == a then xs else x:delete a xs

    data DecisionPlayerState = DPS Hand [Card] deriving Show

    instance PlayerState DecisionPlayerState where
        initState _ cs = DPS cs []

        updateState a _ pc Nothing (DPS xs b) = DPS (delete pc xs) (b ++ a)
        updateState a _ pc (Just c) (DPS xs b) = DPS ((delete pc xs) ++ [c]) (b ++ a)

    tfind :: a -> (a -> Bool) -> [a] -> a
    tfind d _ [] = d
    tfind d f (x:xs) = if f x then x else tfind d f xs

    returnRank :: DecisionPlayerState -> Rank -> Card
    returnRank state@(DPS hand@(x:xs) _) rank =
        case perfectCard of
            Nothing -> returnCheap state
            Just c -> c
        where 
            perfectCard = find (\(Card s r) -> rank == r) hand
    
    cardCost :: Card -> [Rank] -> Int
    cardCost (Card _ R7) _ = 1
    cardCost (Card _ R10) _ = 1
    cardCost (Card _ RA) _ = 1
    cardCost (Card _ _) _ = 0

    returnFirst :: DecisionPlayerState -> Card
    returnFirst (DPS hand@(x:xs) _) =
        retCheap xs x []
        where 
            retCheap [] c _ = c
            retCheap (x@(Card _ r):xs) c p = if (cardCost c p) > (cardCost x p) then retCheap xs x (r:p) else retCheap xs c (r:p)

    returnCheap :: DecisionPlayerState -> Card
    returnCheap (DPS hand@(x:xs) _) =
        retCheap xs x []
        where 
            retCheap [] c _ = c
            retCheap (x@(Card _ r):xs) c p = if (cardCost c p) > (cardCost x p) then retCheap xs x (r:p) else retCheap xs c (r:p)

    
    returnBest :: DecisionPlayerState -> Rank -> Card
    returnBest state@(DPS hand@(x:xs) _) rank =
        case perfectCard of
            Nothing -> case seven of
                Nothing -> returnCheap state
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
