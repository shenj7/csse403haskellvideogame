module View where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Model

-- | Number of frames to show per second.
fps :: Int
fps = 60

window :: Display
window = InWindow "Game" (Model.height, Model.width) (Model.offset, Model.offset)

background :: Color
background = white

drawing :: Picture
drawing = circle 80
