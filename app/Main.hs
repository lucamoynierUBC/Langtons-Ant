module Main where

import Graphics.Gloss


data Cell = Black | White
    deriving (Show)

type Position = (Int, Int)

data Direction = N | S | W | E
    deriving(Eq, Show)

type Grid = [[Cell]]

type Ant = (Position, Direction)

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
antRadius = 2

spawnAnt :: Ant
spawnAnt = ((gridDimension `div` 2, gridDimension `div` 2), W)

turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W 
turnRight W = N

initialState :: Gamestate
initialState = Game
    {
        antPosition = (gridDimension `div` 2, gridDimension `div` 2),
        steps = 0,
        antState = W,
        gridState = replicate gridDimension (replicate gridDimension White)
    }

drawGrid :: Grid -> Picture
drawGrid grid = Pictures [ translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (drawCell cell)
                         | (y, row) <- zip [0..] grid
                         , (x, cell) <- zip [0..] row ]
  where
    cellSize = fromIntegral cellDimension
    drawCell :: Cell -> Picture
    drawCell cell = Pictures [color cellColor $ rectangleSolid cellSize cellSize, color borderColor $ rectangleWire cellSize cellSize]
        where
            cellSize = fromIntegral cellDimension
            cellColor = case cell of
                        Black -> black
                        White -> white
            borderColor = greyN 0.8 -- Light grey color for the border

drawAnt :: Ant -> Picture
drawAnt ((x, y), _) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ color red $ circleSolid antSize
  where
    cellSize = fromIntegral cellDimension
    antSize = fromIntegral antRadius

drawing :: Gamestate -> Picture
drawing gameState = pictures [drawGrid (gridState gameState), drawAnt (antPosition gameState, antState gameState)]

main :: IO ()
main = display window background (drawing initialState)