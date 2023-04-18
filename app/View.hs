module View where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Model

-- | Number of frames to show per second.
fps :: Int
fps = 60

window :: Display
window = InWindow "Game" (Model.width, Model.height) (Model.offset, Model.offset)

background :: Color
background = white

drawing :: Picture
drawing = circle 20

render :: VGame -> Picture
render game =
    pictures [playerchar]
    where
        Sprite w x y z = player game
        playerchar = uncurry translate (x, w) $ color playercolor $ circleSolid 20
        playercolor = dark blue
