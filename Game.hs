module Game where

import Board

data Game = Game {board :: Board, player :: Player}
instance Show Game where
    show (Game board _) = show board

data Player = RedPlayer | BluePlayer deriving (Eq)
instance Show Player where
    show RedPlayer = "🔴"
    show BluePlayer = "🔵"

createGame :: Maybe Board -> Player -> Maybe Game
createGame Nothing _ = Nothing
createGame (Just board) player = Just (Game board player)

move :: Game -> Int -> Maybe Game
move (Game board RedPlayer) rowNumber = createGame (moveBoard board BluePlayer rowNumber) BluePlayer
move (Game board BluePlayer) rowNumber = createGame (moveBoard board RedPlayer rowNumber) RedPlayer

hasMoves :: Game -> Bool
hasMoves (Game (Board reds blues) _) = (length reds + length blues) < 42

nextPlayer :: Game -> Player
nextPlayer (Game _ BluePlayer) = RedPlayer
nextPlayer (Game _ RedPlayer) = BluePlayer

-- Lists
generateWinCoalitions :: [[Int]]
generateWinCoalitions =
        -- \ -> backward
       [[x, y, z, a] | x <- [1,2,3,4,8,9,10,11,15,16,17,18], y <- [1..42], z <- [1..42], a <- [1..42], (x + 8 == y) && (y + 8 == z) && (z + 8 == a)]
        -- / -> straight
    ++ [[x, y, z, a] | x <- [4,5,6,7,11,12,13,14,18,19,20,21], y <- [1..42], z <- [1..42], a <- [1..42], (x + 6 == y) && (y + 6 == z) && (z + 6 == a)]
        -- | -> vertical
    ++ [[x, y, z, a] | x <- [1..21], y <- [1..42], z <- [1..42], a <- [1..42], (x + 7 == y) && (y + 7 == z) && (z + 7 == a)]
        -- -- -> horizontal
    ++ [[x, y, z, a] | x <- [1..39], y <- [1..42], z <- [1..42], a <- [1..42], (x + 1 == y) && (y + 1 == z) && (z + 1 == a) && (((x + 2) `mod` 7) > 2)]

contains :: (Eq a) => [a] -> [a] -> Bool
contains stack needle = stack `intersect` needle == needle

multiContains :: (Eq a) => [a] -> [[a]] -> Bool
multiContains stack needles = foldl (\acc needle -> if stack `contains` needle then True else acc) False needles

