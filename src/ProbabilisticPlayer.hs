module ProbabilisticPlayer where

    import SedmaBase
        
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find f (x:xs) = if f x then Just x else find f xs

    delete :: (Eq a) => a -> [a] -> [a]
    delete _ [] = []
    delete a (x:xs) = if x == a then xs else x:delete a xs

    -- Hand PossibleA, PossibleB, PossibleC, PossibleD
    data ProbabilisticPlayerState = DPS [Card] Cards Cards Cards Cards Player deriving Show

    removePlayed :: Cards -> ProbabilisticPlayerState -> ProbabilisticPlayerState
    removePlayed [] state = state
    removePlayed (card:cs) (DPS h a b c d f) = DPS h (delete card a) (delete card b) (delete card c) (delete card d) f

    allCards = [Card s r | s <- [Heart ..], r <- [R7 ..]]

    
    instance PlayerState ProbabilisticPlayerState where
        initState p cs = DPS cs allCards allCards allCards allCards A

        updateState trick p pc Nothing (DPS xs a b c d _) = removePlayed trick (DPS (delete pc xs) a b c d p)
        updateState trick p pc (Just nc) (DPS xs a b c d _) = removePlayed trick (DPS ((delete pc xs) ++ [nc]) a b c d p)

    tfind :: a -> (a -> Bool) -> [a] -> a
    tfind d _ [] = d
    tfind d f (x:xs) = if f x then x else tfind d f xs

    countProbability :: Rank -> [Card] -> Float
    countProbability rank xs =
        cpHelper xs 0 0
        where
            cpHelper [] pos all = pos / all
            cpHelper ((Card _ r):xs) pos all | r == rank = cpHelper xs (pos + 1) (all + 1)
            cpHelper (x:xs) pos all | otherwise = cpHelper xs pos (all + 1)

    hasCardProbability :: Player -> Rank -> ProbabilisticPlayerState -> Float
    hasCardProbability A rank (DPS _ s _ _ _ _) = countProbability rank s
    hasCardProbability B rank (DPS _ _ s _ _ _) = countProbability rank s
    hasCardProbability C rank (DPS _ _ _ s _ _) = countProbability rank s
    hasCardProbability D rank (DPS _ _ _ _ s _) = countProbability rank s

    nextPlayer :: Player -> Int -> Player
    nextPlayer c 0 = c
    nextPlayer A 1 = B
    nextPlayer B 1 = C
    nextPlayer C 1 = D
    nextPlayer D 1 = A
    nextPlayer c x = nextPlayer (nextPlayer c 1) (x - 1)

    pinter p1 p2 = p1 * p2
    punion p1 p2 = p1 + p2 - pinter p1 p2

    evaluateProbability :: [Rank] -> Rank -> ProbabilisticPlayerState -> Float

    --fourth
    evaluateProbability [x1, x2, x3] R7 state@(DPS _ _ _ _ _ p) = 1.0
    evaluateProbability [x1, x2, R7] _ state@(DPS _ _ _ _ _ p) = 0.0
    evaluateProbability [x1, R7, _] _ state@(DPS _ _ _ _ _ p) = 1.0
    evaluateProbability [x1, x2, x3] x state@(DPS _ _ _ _ _ p) | x == x1 = 1.0
    evaluateProbability [x1, x2, x3] x state@(DPS _ _ _ _ _ p) | x3 == x1 = 0.0
    evaluateProbability [x1, x2, x3] x state@(DPS _ _ _ _ _ p) | x2 == x1 = 1.0
    evaluateProbability [x1, x2, x3] x state@(DPS _ _ _ _ _ p) | otherwise = 0.0


    --third
    evaluateProbability [x1, _] R7 state@(DPS _ _ _ _ _ p) = 1 - hasCardProbability (nextPlayer p 3) R7 state
    evaluateProbability [x1, R7] _ state@(DPS _ _ _ _ _ p) = 0
    evaluateProbability [x1, _] x state@(DPS _ _ _ _ _ p) | x == x1 = (1 - hasCardProbability (nextPlayer p 3) R7 state) *
                                                                    (1 - hasCardProbability (nextPlayer p 3) x1 state)
    evaluateProbability [x1, x2] _ state@(DPS _ _ _ _ _ p)| x1 == x2 = 0
    evaluateProbability [R7, _] _ state@(DPS _ _ _ _ _ p)  = 1 - hasCardProbability (nextPlayer p 3) R7 state
    evaluateProbability [_, _] _ state@(DPS _ _ _ _ _ p)  = (1 - hasCardProbability (nextPlayer p 3) R7 state) *
                                                            (1 - hasCardProbability (nextPlayer p 3) R7 state)



    --second
    evaluateProbability [R7] R7 state@(DPS _ _ _ _ _ p) = punion (hasCardProbability (nextPlayer p 3) R7 state) (1 - (hasCardProbability (nextPlayer p 2) R7 state))
    evaluateProbability [R7] _ state@(DPS _ _ _ _ _ p) = hasCardProbability (nextPlayer p 3) R7 state
    evaluateProbability [x1] x2 state@(DPS _ _ _ _ _ p) | x1 == x2 =
        ((hasCardProbability (nextPlayer p 3) R7 state) * (hasCardProbability (nextPlayer p 3) x1 state)) +
        (
            ((hasCardProbability (nextPlayer p 3) R7 state) * (1 - (hasCardProbability (nextPlayer p 3) x1 state))) +
            ((((1 - (hasCardProbability (nextPlayer p 3) R7 state)) * (hasCardProbability (nextPlayer p 3) x1 state)) *
                (1 - (hasCardProbability (nextPlayer p 2) R7 state))
                ) +
            ((1 - (hasCardProbability (nextPlayer p 3) R7 state)) * ((1 - (hasCardProbability (nextPlayer p 3) x1 state)) *
                (
                    (1 - (hasCardProbability (nextPlayer p 2) R7 state)) *
                    (1 - (hasCardProbability (nextPlayer p 2) x1 state)) 
                ))))
        )

    evaluateProbability [x1] _ state@(DPS _ _ _ _ _ p) = 
        (punion 
            (hasCardProbability (nextPlayer p 3) R7 state)
            (pinter (hasCardProbability (nextPlayer p 3) x1 state) (1 - (hasCardProbability (nextPlayer p 2) R7 state)))
        )

    evaluateProbability [] R7 state@(DPS _ _ _ _ _ p) = 
            (1 - (hasCardProbability (nextPlayer p 3) R7 state)) *
            (punion 
                (hasCardProbability (nextPlayer p 2) R7 state)
                (1 - (hasCardProbability (nextPlayer p 1) R7 state))
            )

    evaluateProbability [] x1 state@(DPS _ _ _ _ _ p) =
        (1 - (hasCardProbability (nextPlayer p 3) R7 state)) *
        (punion
            (pinter 
                (hasCardProbability (nextPlayer p 2) R7 state)
                (1 - (hasCardProbability (nextPlayer p 1) R7 state))
            )
            (
                (1 - (hasCardProbability (nextPlayer p 3) x1 state)) *
                (punion
                    (punion
                        (hasCardProbability (nextPlayer p 2) R7 state)
                        (pinter
                            (1 - (hasCardProbability (nextPlayer p 1) R7 state))
                            (hasCardProbability (nextPlayer p 2) x1 state)
                        )
                    )
                    (
                        (1 - (hasCardProbability (nextPlayer p 1) R7 state)) *
                        (1 - (hasCardProbability (nextPlayer p 1) x1 state)) *
                        (1 - (hasCardProbability (nextPlayer p 2) R7 state)) *
                        (1 - (hasCardProbability (nextPlayer p 2) x1 state))
                    )
                )
            )
        )

    

    getRanks xs = [r | (Card s r) <- xs]

    pickBest trick state@(DPS ((cx@(Card _ crank)):cxs) _ _ _ _ _) = 
        cardret
        where
            cranks = getRanks trick
            pickBestAcc [] d = d
            pickBestAcc (x@(Card _ rank):xs) (val, card) =
                if c > val then pickBestAcc xs (c, x) else pickBestAcc xs (val, card)
                where c = evaluateProbability cranks rank state
            (res, cardret) = pickBestAcc cxs ((evaluateProbability cranks crank state), cx)
    

    play trick state = pickBest trick state


    player :: AIPlayer ProbabilisticPlayerState
    player = play
