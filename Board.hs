module Board where

import Data.List
import Player
import Cell

data Board = Board {
      reds :: [Int]
    , blues :: [Int]
} deriving (Eq)
instance Show Board where
    show board = fst $ foldl (\ (acc, i) el -> ((acc ++ " " ++ show el ++ (if i `mod` 7 == 0 then "\n" else "")), i + 1)) ("", 1) (toShowable board)

toShowable :: Board -> [Cell]
toShowable (Board [] []) = take 42 $ repeat Free
toShowable (Board reds blues) =
    let emptyBoard = take 42 $ repeat Free
    in mergeCells (replaceOnPositions emptyBoard blues Blue) (replaceOnPositions emptyBoard reds Red)

winner :: Board -> Maybe Player
winner (Board reds blues)
           | (sort reds) `multiContains` winnerCoalitions = Just RedPlayer
           | (sort blues) `multiContains` winnerCoalitions = Just BluePlayer
           | otherwise = Nothing
    where winnerCoalitions = generateWinCoalitions

moveBoard :: Board -> Player -> Int -> Maybe Board
moveBoard (Board reds blues) (player) rowNumber
    | null availableMoves = Nothing
    | player == RedPlayer = Just (Board (reds ++ [maximum availableMoves]) blues)
    | player == BluePlayer = Just (Board reds ([maximum availableMoves] ++ blues))
    where
        verticalRow = [ x * 7 + rowNumber | x <- [0..5]]
        availableMoves = verticalRow \\ (reds ++ blues)

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
