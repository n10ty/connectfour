module Player where

data Player = RedPlayer | BluePlayer deriving (Eq)
instance Show Player where
    show RedPlayer = "🔴"
    show BluePlayer = "🔵"