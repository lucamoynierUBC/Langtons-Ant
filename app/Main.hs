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
window = InWindow "Langtons Ant" (gridDimension * cellDimension, gridDimension * cellDimension) (100, 100)

-- Drawing Settings
background :: Color
background = white

antRadius :: Integer
antRadius = 2

spawnAnt :: Ant
spawnAnt = ((gridDimension `div` 2, gridDimension `div` 2), N)

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
        antState = N,
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
drawAnt ((x, y), dir) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ rotate (directionToAngle dir) $ color red $ polygon trianglePoints
  where
    cellSize = fromIntegral cellDimension
    antSize = fromIntegral antRadius
    trianglePoints = [(-antSize, -antSize/2), (antSize, 0), (-antSize, antSize/2)]

    directionToAngle :: Direction -> Float
    directionToAngle N = 0
    directionToAngle E = 90
    directionToAngle S = 180
    directionToAngle W = -90

drawing :: Gamestate -> Picture
drawing gameState = pictures [drawGrid (gridState gameState), drawAnt (antPosition gameState, antState gameState)]

fps :: Int
fps = 1

moveAnt :: Float -> Gamestate -> Gamestate
moveAnt _ game = game { antPosition = (x', y'),  antState = newDir, gridState = newGrid, steps = (steps game) + 1}
  where
    grid = gridState game
    dir = antState game
    (x, y) = antPosition game
    currCell = (grid !! y) !! x
    newDir  | currCell == Black = turnLeft dir
            | otherwise = turnRight dir
    x'  | newDir == E = x + 1
        | newDir == W  = x - 1
        | otherwise = x
    y'  | newDir == N = y - 1
        | newDir == S = y + 1
        | otherwise = y
    newGrid = updateGrid x y currCell grid



updateGrid :: Int -> Int -> Cell -> Grid -> Grid
updateGrid x y cell grid = map updateRow (zip [0..] grid)
  where
    updateRow (rIdx, row)
      | rIdx == y = updateCol row
      | otherwise = row
    updateCol row = map updateCell (zip [0..] row)
    updateCell (cIdx, cell)
      | cIdx == x = flipCell cell
      | otherwise = cell
    

    
   

-- The correct signature for the update function used with simulate
update :: ViewPort -> Float -> Gamestate -> Gamestate
update _ seconds game = moveAnt seconds game

main :: IO ()
main = simulate window background fps initialState drawing update