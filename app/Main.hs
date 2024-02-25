-- {-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}

module Main where

{-
Langton's Ant Application

The "world" is a grid, and each square can either be black or white. The ant starts from the centre of the grid, 
and moves according to these rules (taken from Wikipedia):

At a white square, turn 90° clockwise, flip the colour of the square, move forward one unit
At a black square, turn 90° counter-clockwise, flip the colour of the square, move forward one unit

Our version of Langton's Ant allows the user to pick their own colors, instead of the default black & white
-}

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Graphics.Gloss as Gloss
-- import qualified Data.String
-- import Data.GI.Base
-- import qualified GI.Gtk as Gtk

data Cell = C1 | C2
    deriving (Eq, Show)

type Position = (Int, Int)

-- Colors of grid, default is white & black
color1 :: Gloss.Color
color1 = white

color2 :: Gloss.Color
color2 = black

-- Possible directions the ant could have
data Direction = N | S | W | E
    deriving(Eq, Show)

-- Grid is a 2-dimensional list of cells
type Grid = [[Cell]]

-- The ant is represented by its position on the grid and direction
type Ant = (Position, Direction)

-- The game state contains information regarding the ant and the grid
data Gamestate = Game
    {
        antPosition :: Position,
        steps :: Int,
        antState :: Direction,
        gridState :: Grid,
        c1 :: Color,
        c2 :: Color
    }
    deriving (Show)

-- One side of a square grid
gridDimension :: Int
gridDimension = 100

-- One side of a square cell
cellDimension :: Int
cellDimension = 5

-- Application window
window :: Display
window = InWindow "Langtons Ant" (gridDimension * cellDimension, gridDimension * cellDimension) (10, 10)

-- Drawing Settings
background :: Color
background = white

antRadius :: Integer
antRadius = 2

spawnAnt :: Ant
spawnAnt = ((gridDimension `div` 2, gridDimension `div` 2), W)

-- Turning left or right changes the ant's direction
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

-- flipCell inverts the cell's color
flipCell :: Cell -> Cell
flipCell C1 = C2
flipCell C2 = C1

-- The initial state of the game, ant is located in the middle and is facing north
initialState :: Gamestate
initialState = Game
    {
        antPosition = (gridDimension `div` 2, gridDimension `div` 2),
        steps = 0,
        antState = W,
        gridState = replicate gridDimension (replicate gridDimension C1),
        c1 = color1,
        c2 = color2
    }

-- drawGrid renders a grid made up of cells that can be either of the 2 colors taken as input.
drawGrid :: Grid -> Color -> Color -> Picture
drawGrid grid co1 co2 = Pictures [ translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) (drawCell cell)
                         | (y, row) <- zip [0..] grid
                         , (x, cell) <- zip [0..] row ]
  where
    cellSize = fromIntegral cellDimension
    drawCell :: Cell -> Picture
    drawCell cell = Pictures [color cellColor $ rectangleSolid cellSize cellSize, color borderColor $ rectangleWire cellSize cellSize]
        where
            cellSize = fromIntegral cellDimension
            cellColor = case cell of
                        C1 -> co1
                        C2 -> co2
            borderColor = greyN 0.8 -- Light grey color for the border

-- drawAnt renders the ant
drawAnt :: Ant -> Picture
drawAnt ((x, y), _) = translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ color red $ circleSolid antSize
  where
    cellSize = fromIntegral cellDimension
    antSize = fromIntegral antRadius

-- drawing renders the entire program, which includes the grid and the ant based on game state and ant state respectively
drawing :: Gamestate -> Picture
drawing gameState = pictures [drawGrid (gridState gameState) (c1 gameState) (c2 gameState), drawAnt (antPosition gameState, antState gameState)]

fps :: Int
fps = 200

-- moveAnt updates the game state depending on which type of cell the ant encounters
moveAnt :: Float -> Gamestate -> Gamestate
moveAnt _ game = game { antPosition = (x', y'),  antState = newDir, gridState = newGrid, steps = (steps game) + 1}
  where
    grid = gridState game
    dir = antState game
    (x, y) = antPosition game
    currCell = (grid !! y) !! x
    newDir  | currCell == C2 = turnLeft dir
            | otherwise = turnRight dir
    x'  | newDir == E = x + 1
        | newDir == W  = x - 1
        | otherwise = x
    y'  | newDir == N = y - 1
        | newDir == S = y + 1
        | otherwise = y
    newGrid = updateGrid x y currCell grid


-- updateGrid updates the cells (flips colors if ant lands on it, leaves them the same otherwise)
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
    

-- update gets the ant to move and continue the program/game
update :: ViewPort -> Float -> Gamestate -> Gamestate
update _ seconds game = moveAnt seconds game

-- main first gets the user to input two colors for the grid, starts the program if user makes two valid choices
main :: IO ()
main =
    do
        putStrLn "Welcome! Please pick the number of the first color from these options:\n 1. white (default)\n 2. black\n 3. red\n 4. green\n 5. blue"
        col1 <- getLine
        if col1 `elem` ["1", "2", "3", "4", "5", ""]
            then do
                putStrLn "Please pick the second color from these options:\n 1. white\n 2. black (default)\n 3. red\n 4. green\n 5. blue"
                col2 <- getLine
                if col2 `elem` ["1","2", "3","4","5",""]
                    then do
                        let configured = setColors col1 col2
                        simulate window background fps configured drawing update
                else
                    putStrLn "Invalid input. Please enter a number."
        
        else
            putStrLn "Invalid input. Please enter a number."
        
-- setColors adjusts the initial state of the game using the color inputs        
setColors :: String -> String -> Gamestate
setColors col1 col2 = initialState {c1 = convert col1 white, c2 = convert col2 black}

-- convert converts a string input to a color, if string isn't a predefined value, returns the 
-- "default" color (def)
convert :: String -> Gloss.Color -> Gloss.Color
convert c def
    | c == "1" = white
    | c == "2" = black
    | c == "3" = rose
    | c == "4" = chartreuse
    | c == "5" = azure
    | otherwise = def


-- GTK Stuff:
{-
--getColors :: (Gloss.Color, Gloss.Color)
getColors =
    do
        Gtk.init Nothing
        boxx1 <- Gtk.boxNew Gtk.OrientationVertical 75
        wind <- new Gtk.Window [#title := "Welcome! Please pick two colors."]
        on wind #destroy Gtk.mainQuit
        #resize wind 400 400
        combo1 <- Gtk.comboBoxTextNew
        Gtk.comboBoxTextAppendText combo1 "default color: white"
        Gtk.comboBoxTextAppendText combo1 "black"
        Gtk.comboBoxTextAppendText combo1 "red"
        Gtk.comboBoxTextAppendText combo1 "green"
        Gtk.comboBoxTextAppendText combo1 "blue"
        combo2 <- Gtk.comboBoxTextNew
        Gtk.comboBoxTextAppendText combo2 "default color: black"
        Gtk.comboBoxTextAppendText combo2 "white"
        Gtk.comboBoxTextAppendText combo2 "red"
        Gtk.comboBoxTextAppendText combo2 "green"
        Gtk.comboBoxTextAppendText combo2 "blue"
        cont <- Gtk.buttonNewWithLabel "Continue"
        Gtk.containerAdd boxx1 combo1
        Gtk.containerAdd boxx1 combo2
        Gtk.containerAdd boxx1 cont
        #add wind boxx1
        #showAll wind
        c1 <- Gtk.comboBoxTextGetActiveText combo1 
        c2 <- Gtk.comboBoxTextGetActiveText combo2
        on cont #clicked (Gtk.windowClose wind)
        Gtk.main
        return (c1, c2)

convert c
    | c == "default color: white" || c == "white" = white
    | c == "default color: black" || c == "black" = black
    | c == "red" = red
    | c == "green" = green
    | c == "blue" = blue
    | otherwise = white

-}