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
    pictures [playerchar, bulletlist]
    where
        Sprite w x y z = player game
        playerchar = uncurry translate (x, w) $ color playercolor $ circleSolid 20
        playercolor = dark blue

        listofbullet = bullets game
        rlistofbullet = removeBullets listofbullet
        bulletlist = pictures $ map renderBullets rlistofbullet

renderBullets :: Bullet -> Picture
renderBullets bullet = picture
    where
        Bullet w x y z = bullet
        bulletcolor = red
        picture = uncurry translate (x, w) $ color bulletcolor $ circleSolid 5