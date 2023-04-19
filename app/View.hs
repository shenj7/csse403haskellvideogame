module View where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import Model

-- | Number of frames to show per second.
fps :: Int
fps = 60

window :: Display
window = InWindow "Game" (round Model.width, round Model.height) (round Model.offset, round Model.offset)

background :: Color
background = white

drawing :: Picture
drawing = circle 20

render :: VGame -> Picture
render game =
    pictures [playerchar, newentities]
    where
        playerchar = renderEntity (player game)

        clearedlist = removeEntities (entities game)
        newentities = map renderEntity newentities

renderEntity :: Entity -> Picture
renderEntity entity = picture
    where
        Entity (x, y) shade radius _ _ = entity
        picture = uncurry translate (x, y) $ color shade $ circleSolid radius
