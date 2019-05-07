module Board where

import Data.List
import Game

-- Board
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
