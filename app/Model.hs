module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game


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

-- | Gun definitions
data Gun = EmptyGun | DefaultGun -- Add more guns here and at the bottom of the file

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
    gun :: Gun
}

-- | Initial state
initialState :: VGame
initialState = Game {
    player = playerEntity,
    entities = level1,
    gamePaused = False,
    isShooting = False,
    gun = DefaultGun
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
        newList = if (health > 0 && (x >= (-width/2) - 5*radius && x <= width/2 + 5*radius && y >= (-height/2) - 5*radius && y <= height/2 + 5*radius))
            then
                h: removeEntities t
            else
                removeEntities t

removeEntities [] = []

-- | Shoot gun
shootGun :: VGame -> (VGame -> VGame)
shootGun game = shootingGun
    where
        currentGun = if (isShooting game) then gun game else EmptyGun
        shootingGun = getGun currentGun

getGun :: Gun -> (VGame -> VGame)
getGun EmptyGun = emptyGun
getGun DefaultGun = defaultGun

-- | Update game
update :: Float -> VGame -> VGame
update seconds game = updateEntities $ handleCollisionsEntities $ (shootGun game) $ movePlayer seconds $ moveentities seconds game


-- | Standard move function (player, bullets, some enemies)

standardMove :: Float -> Entity -> [Entity]
standardMove seconds entity = [newEntity]
    where
        newEntity = calcLoc seconds entity

calcLoc :: Float -> Entity -> Entity
calcLoc seconds entity = entity {pos = pos'} where
    (x, y) = pos entity
    (vx, vy) = vel entity
    x' = x + vx * seconds 
    y' = y + vy * seconds

    pos' = (x', y')

-- | Straight line enemy
lineMove :: Float -> Float -> Entity -> [Entity]
lineMove steps seconds entity = wait steps seconds entity

-- | Wait function
-- wait :: Float -> Float -> Entity -> [Entity] -> (Float -> Entity -> [Entity])
-- wait steps f2 = if steps == 0 then f2 else wait (steps - 1) f2

-- | wait part 2 electric boogaloo
wait :: Float -> Float -> Entity -> [Entity]
wait steps seconds entity = [entity {pos = pos', move = wait (steps - 1)}] where
    (x, y) = pos entity
    (vx, vy) = vel entity
    x' = if steps <= 0
    then 
        x + vx * seconds 
    else 
        x

    y' = if steps <= 0
    then
        y + vy * seconds
    else
        y

    pos' = (x', y')
    
        

-- | Empty gun (if not shooting)
emptyGun :: VGame -> VGame
emptyGun game = game

-- | Default gun (other guns could spawn 5 bullets at a time, bigger bullets, etc)
defaultGun :: VGame -> VGame
defaultGun game = game {entities = afterShooting, isShooting = False}
    where
        oldEntities = entities game
        newBullet = Entity (pos (player game)) (0, 250) red 10 standardMove 10 10
        afterShooting = newBullet:oldEntities

-- | first level of enemies
level1 :: [Entity]
level1 = [
    (Entity ((-width/2)-20, 0) (30, 0) green 10 standardMove 20 10),
    (Entity ((-width/2)-20, 100) (30, 0) green 10 (lineMove 200) 20 10),
    (Entity ((-width/2)-20, 200) (30, 0) green 10 (lineMove 400) 20 10),
    (Entity ((-width/2)-20, 300) (30, 0) green 10 (lineMove 600) 20 10),
    (Entity ((-width/2)-20, 400) (30, 0) green 10 (lineMove 800) 20 10),
    (Entity ((width/2)+20, 100) (-30, -20) yellow 10 standardMove 20 10),
    (Entity ((width/2)+20, 200) (-30, -20) yellow 10 (lineMove 200) 20 10),
    (Entity ((width/2)+20, 300) (-30, -20) yellow 10 (lineMove 400) 20 10),
    (Entity ((width/2)+20, 400) (-30, -20) yellow 10 (lineMove 600) 20 10),
    (Entity ((width/2)+20, 500) (-30, 20) yellow 10 (lineMove 800) 20 10),
    (Entity (0, height/2+50) (0, -20) violet 25 (lineMove 1200) 100 1000)
    ]
