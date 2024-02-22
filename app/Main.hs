{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Gloss
import Data.GI.Base
import qualified GI.Gtk as Gtk

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
main = 
    do
        -- Pick colors, then continue to grid
        Gtk.init Nothing
        boxx1 <- Gtk.boxNew Gtk.OrientationVertical 75
        wind <- new Gtk.Window[#title := "Welcome! Please pick two colors."]
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
        c1 <- Gtk.comboBoxTextGetActiveText combo1 -- TO DO: convert these into 
        c2 <- Gtk.comboBoxTextGetActiveText combo2 --        actual colors
        on cont #clicked (Gtk.windowClose wind)
        Gtk.main
        display window background (drawing initialState)