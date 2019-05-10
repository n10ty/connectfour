module Game where

import Board
import Player

data Game = Game {board :: Board, player :: Player}
instance Show Game where
    show (Game board _) = show board

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
