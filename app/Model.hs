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
    vel :: (Float, Float),
    shade :: Color,
    radius :: Float,
    move :: Float -> Entity -> [Entity],
    health :: Int,
    damage :: Int
}

-- | player info
playerEntity :: Entity
playerEntity = Entity {
    pos = (0, 0),
    vel = (0, 0),
    shade = dark blue,
    radius = 20,
    move = standardMove,
    health = 100,
    damage = 10
}

-- | Game parameters
width, height, offset, playerSpeed, resetpos :: Float
width = 1000
height = 1000
offset = 10
playerSpeed = 150
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
    entities = [(Entity (0, 0) (0, 10) red 10 standardMove 10 10),
                (Entity (0, 100) (0, 0) green 10 standardMove 10 10)],
    gamePaused = False,
    isShooting = False,
    normalGun = GunData 7 0 500,
    shotGun = GunData 7 100 200,
    bigGun = GunData 50 0 300,
    selectedGun = Norm
}

-- | Move entities

movePlayer :: Float -> VGame -> VGame
movePlayer seconds game = game {player = newPlayer}
    where
        newPlayer = head $ (move $ player game) seconds (player game)

moveentities :: Float -> VGame -> VGame
moveentities seconds game = game {entities = newentities}
    where
        newentities = concat $ map (\x -> (move x) seconds x) (entities game)

-- | Collisions

handleCollisionsEntities :: VGame -> VGame
handleCollisionsEntities game = game {entities = newEntities}
    where
        entityList = entities game
        newEntities = handlePairs entityList

handlePairs :: [Entity] -> [Entity]
handlePairs (h:t) = newHead:restList
    where
        newList = map (\x -> handleSingleCollision x h) t
        newHead = foldl handleSingleCollision h t
        restList = handlePairs newList
handlePairs [] = []

handleSingleCollision :: Entity -> Entity -> Entity
handleSingleCollision main other = main {health = newHealth}
    where
        newHealth = if overlapEntities main other
            then (health main) - (damage other)
            else health main

overlapEntities :: Entity -> Entity -> Bool
overlapEntities e1 e2 = (distance (pos e1) (pos e2)) < ((radius e1) + (radius e2))

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
        x' = x1 - x2
        y' = y1 - y2


-- | Remove when off the screen
updateEntities :: VGame -> VGame
updateEntities game = game {entities = removeEntities $ entities game}

removeEntities :: [Entity] -> [Entity]
removeEntities (h:t) = newList
    where
        Entity (x, y) _ _ radius _ health _ = h
        newList = if (health > 0 && (x >= (-width/2) + radius/2 && x <= width/2 - radius/2 && y >= (-height/2) + radius/2 && y <= height/2 - radius/2))
            then
                h: removeEntities t
            else
                removeEntities t

removeEntities [] = []

-- | Update game
update :: Float -> VGame -> VGame
update seconds game = updateEntities $ handleCollisionsEntities $ movePlayer seconds $ moveentities seconds game


-- | Standard move function (player, bullets, some enemies)

standardMove :: Float -> Entity -> [Entity]
standardMove seconds entity = [newEntity]
    where
        newEntity = calcLoc seconds entity

calcLoc :: Float -> Entity -> Entity
calcLoc seconds entity = entity {pos = pos'} where
    (x, y) = pos entity
    (vx, vy) = vel entity
    x' = if x + vx * seconds <= height/2 && x + vx * seconds >= (-height/2) 
    then 
        x + vx * seconds 
    else 
        x

    y' = if y + vy * seconds <= width/2 && y + vy * seconds >= (-width/2)
    then
        y + vy * seconds
    else
        y

    pos' = (x', y')
