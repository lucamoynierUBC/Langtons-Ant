module Main(main) where

import Graphics.Gloss


main :: IO ()
main = display window background drawing

data Cell = Black | White
    deriving (Show)

type Position = (Int, Int)

data Direction = Up | Down | Left | Right
    deriving(Show)

type Grid = [[Cell]]

data Gamestate = Game
    {
        antPosition :: Position,
        steps :: Int,
        antState :: Direction,
        gridState :: Grid     
    }
    deriving (Show)

gridDimension :: Int
gridDimension = 100

cellDimension :: Int
cellDimension = 5

window :: Display
window = InWindow "Langtons Ant" (gridDimension * cellDimension, gridDimension * cellDimension) (10, 10)

-- Drawing Settings
background :: Color
background = white

antRadius :: Integer
antRadius = 3

toInt :: Float -> Integer
toInt = round

getColor :: Float -> Float -> Color
getColor i j = if even $ toInt ((i * 8) + j) then orange else blue

drawTile :: Float -> Float -> Color -> Picture
drawTile a b col = translate a b $ color col $ rectangleSolid 5 5

drawing :: Picture
drawing = pictures [drawTile (row * 5) (e * 5) (getColor row e) | row <- [0 .. 8], e <- [0 .. 8]]
