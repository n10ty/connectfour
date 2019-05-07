module Main where

import Board
import Data.List
import Control.Monad

main = do
    let game = Game (Board [][]) BluePlayer
    tryMove game Nothing Nothing

tryMove :: Game -> Maybe Int -> Maybe Game -> IO()
tryMove prevGame Nothing Nothing = do
    print prevGame
    putStr "Choose row to throw coin (1-7)"
    print $ nextPlayer prevGame
    rowNumber <- readLn :: IO Int
    tryMove prevGame (Just rowNumber) Nothing
tryMove prevGame (Just rowNumber) Nothing
    | rowNumber > 7 || rowNumber < 1 = do
        putStrLn "Please, type number between 1 and 7!"
        tryMove prevGame Nothing Nothing
    | otherwise = tryMove prevGame Nothing newGame
    where
        newGame = move prevGame rowNumber
tryMove prevGame Nothing (Just newGame) = do
    checkWinner newGame winPlayer
    where
        winPlayer = winner (board newGame)

checkWinner :: Game -> Maybe Player -> IO()
checkWinner game (Just player) = do
    putStr "Player "
    putStr $ show player
    putStrLn " won!"
    print game
    return()
checkWinner nextGame _ = do
    tryMove nextGame Nothing Nothing
