module RandomPlayer where

    import SedmaBase
        
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find f (x:xs) = if f x then Just x else find f xs

    delete :: (Eq a) => a -> [a] -> [a]
    delete _ [] = []
    delete a (x:xs) = if x == a then xs else x:delete a xs


    data Tree a = TreeNode a [Tree a] | Leaf a deriving (Show, Eq)

    data MonteCarloState = DPS [Card] deriving Show

    instance PlayerState MonteCarloState where
        initState _ cs = DPS cs

        updateState _ _ pc Nothing (DPS xs) = DPS (delete pc xs)
        updateState _ _ pc (Just c) (DPS xs) = DPS ((delete pc xs) ++ [c])

    tfind :: a -> (a -> Bool) -> [a] -> a
    tfind d _ [] = d
    tfind d f (x:xs) = if f x then x else tfind d f xs


    player :: AIPlayer MonteCarloState
    player [] (DPS hand@(x:xs)) = x
    player (card@(Card suit rank) : ts) (DPS hand@(x:xs)) =
        case perfectCard of
            Nothing -> case seven of
                Nothing -> x
                Just c -> c
            Just c -> c
        where 
            perfectCard = find (\(Card s r) -> rank == r) hand
            seven = find (\(Card s r) -> rank == R7) hand
