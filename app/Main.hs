module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data Cell = Black | White
    deriving (Show, Eq)

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
gridDimension = 50

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

flipCell :: Cell -> Cell
flipCell Black = White
flipCell White = Black


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



-- Make sure fps is defined properly, e.g., fps = 60
fps :: Int
fps = 30

-- Corrected moveAnt function to move ant by 1 unit disregarding seconds
moveAnt :: Float -> Gamestate -> Gamestate
moveAnt _ game = game { antPosition = newPos,  antState = newDir, gridState = newGrid}
  where
    grid = gridState game
    dir = antState game
    (x, y) = antPosition game
    currCell = (grid !! x) !! y
    newDir = if currCell == Black then turnRight dir else turnLeft dir
    newPos  | newDir == N = (x, (y-1) `mod` gridDimension)
            | newDir == S = (x, (y+1) `mod` gridDimension)
            | newDir == W = ((x-1) `mod` gridDimension, y)
            | newDir == E = ((x+1) `mod` gridDimension, y)
    newGrid = updateGrid x y (flipCell currCell) grid

updateGrid :: Int -> Int -> Cell -> Grid -> Grid
updateGrid x y newCell grid = 
  take y grid ++ 
  [take x (grid !! y) ++ [newCell] ++ drop (x+1) (grid !! y)] ++ 
  drop (y+1) grid
    

    
   

-- The correct signature for the update function used with simulate
update :: ViewPort -> Float -> Gamestate -> Gamestate
update _ seconds game = moveAnt seconds game

main :: IO ()
main = simulate window background fps initialState drawing update