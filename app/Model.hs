module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- | Gun Definition
-- | bullet size, speed
data GunData = GunData Float Float Float
data GunSelect = Norm | Shot | Big


-- | Entity Definition
-- | enemy position, speed
data Entity = Entity {
    pos :: (Float, Float),
    shade :: Color,
    radius :: Float,
    move :: Entity -> [Entity],
    health :: Int
}

-- | player info
playerEntity :: Entity
playerEntity = Entity {
    pos = (0, 0),
    shade = dark blue,
    radius = 20,
    move = \x -> [x],
    health = 0
}

-- 
-- enemy1 :: Entity
-- enemy1 = Entity {
--     pos = (0, 0),
--     move = enemy1move
-- }

-- | Game parameters
width, height, offset, playerSpeed, resetpos :: Float
width = 700
height = 900
offset = 10
playerSpeed = 10
resetpos = -250

-- | Game state
data VGame = Game {
    player :: Entity,
    entities :: [Entity],
    gamePaused  :: Bool,
    isShooting :: Bool,
    normalGun :: GunData,
    shotGun :: GunData,
    bigGun :: GunData,
    selectedGun :: GunSelect
}

-- | Initial state
initialState :: VGame
initialState = Game {
    player = playerEntity,
    entities = [],
    gamePaused = False,
    isShooting = False,
    normalGun = GunData 7 0 500,
    shotGun = GunData 7 100 200,
    bigGun = GunData 50 0 300,
    selectedGun = Norm
}

movePlayer :: Float -> VGame -> VGame
movePlayer seconds game = game {player = newPlayer}
    where
        newPlayer = head $ (\x -> (move x) x) (player game)

moveentities :: Float -> VGame -> VGame
moveentities seconds game = game {entities = newentities}
    where
        newentities = concat $ map (\x -> (move x) x) (entities game)

removeEntities :: [Entity] -> [Entity]
removeEntities (h:t) = newList
    where
        Entity (w, x) _ _ _ _ = h
        newList = if x >= (-width/2) - 20 && x <= width/2 + 20 && w >= (-height/2) - 20 && w <= height/2+20
            then
                h: removeEntities t
            else
                removeEntities t

removeEntities [h] = newList
    where
        Entity (w, x) _ _ _ _ = h
        newList = if x >= (-width/2) - 20 && x <= width/2 + 20 && w >= (-height/2) - 20 && w <= height/2 + 20
            then
                [h]
            else
                []

removeEntities [] = []
