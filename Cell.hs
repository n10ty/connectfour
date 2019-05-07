module Cell where

data Cell = Red | Blue | Free deriving (Eq)
instance Show Cell where
    show Red = "🔴"
    show Blue = "🔵"
    show Free = "⚪️"

mergeCells :: [Cell] -> [Cell] -> [Cell]
mergeCells xs     []     = xs
mergeCells []     ys     = ys
mergeCells (x:xs) (y:ys)
    | x == Free = y : mergeCells xs ys
    | y == Free = x : mergeCells xs ys
    | otherwise = error ("Merge error. reds = " ++ show (x:xs) ++ ", blues = " ++ show (y:ys))

replaceOnPosition :: [Cell] -> Int -> Cell -> [Cell]
replaceOnPosition [] _ _ = []
replaceOnPosition l n e
    | n > 0 && n <= length l = (take (n - 1) l) ++ [e] ++ (drop n l)
    | otherwise = l

replaceOnPositions :: [Cell] -> [Int] -> Cell -> [Cell]
replaceOnPositions l [] e = l
replaceOnPositions l [x] e = replaceOnPosition l x e
replaceOnPositions l (x:xs) e = mergeCells (replaceOnPosition l x e) (replaceOnPositions l xs e)
