module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- | Sprite definition
-- | x location, y location, x velocity, y velocity
data Sprite = Sprite Float Float Float Float

-- | Gun Definition
-- | bullet size, speed
data GunData = GunData Float Float Float

-- | Bullet Definition
-- | bullet position, speed
data Bullet = Bullet Float Float Float Float
data GunSelect = Norm | Shot | Big

-- | Enemy Definition
-- | enemy position, speed
data Enemy = Enemy {
    pos :: (Float, Float),
    shade :: Color,
    radius :: Float,
    move :: Enemy -> [Enemy],
    health :: Float
}

-- | Game parameters
width, height, offset, playerSpeed, resetpos :: Float
width = 700
height = 900
offset = 10
playerSpeed = 10
resetpos = -250

-- | Game state
data VGame = Game {
    player :: Sprite,
    gamePaused  :: Bool,
    isShooting :: Bool,
    normalGun :: GunData,
    shotGun :: GunData,
    bigGun :: GunData,
    selectedGun :: GunSelect,
    bullets :: [Bullet],
    enemies :: [Enemy]
}

-- | Initial state
initialState :: VGame
initialState = Game {
    player = Sprite resetpos 0 0 0,
    gamePaused = False,
    isShooting = False,
    normalGun = GunData 7 0 500,
    shotGun = GunData 7 100 200,
    bigGun = GunData 50 0 300,
    selectedGun = Norm,
    bullets = [],
    enemies = createEnemies 10
}
    where

createEnemies :: Float -> [Enemies]   
createEnemies numEnemies startx starty moveFunc = newEnemies
    where
        newEnemies = 
            if numEnemies == 1
                then
                    [Enemy {
                        pos = (startx, starty),
                        shade = green,
                        radius = 20,
                        move = moveFunc,
                        health = 1
                    }]
                else
                    Enemy {
                        pos = (startx, starty),
                        shade = green,
                        radius = 20,
                        move = moveFunc,
                        health = 1
                    } : createEnemies (numEnemies-1) (startx+25) starty moveFunc

-- | Update sprites
moveSprite :: Float -> VGame -> VGame
moveSprite seconds game = game { player = newSprite, bullets = newBullets } -- | figure out how to make it get anything out instead of just a player
    where
        -- Old locations and velocities.
        Sprite w x y z = player game
        
        oldbullets = bullets game
        shooting = isShooting game
        selectedgun = selectedGun game
        
        oldenemies = enemies game

        -- New locations.
        bullet = Bullet w x 500 0
        oldbullets2 = if shooting then oldbullets ++ [bullet] else oldbullets
        newSprite = calcLoc seconds (Sprite w x y z)
        
        func = calcBulletLoc seconds
        oldbullets3 = removeBullets oldbullets2
        newBullets = map func oldbullets3
        
        newEnemies = []

-- | Get new location of sprite
calcLoc :: Float -> Sprite -> Sprite
calcLoc seconds (Sprite x y vx vy) = Sprite x' y' vx vy where
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

calcBulletLoc :: Float -> Bullet -> Bullet
calcBulletLoc seconds (Bullet x y vx vy) = Bullet x' y' vx vy where
    x' = if x + vx * seconds <= height/2 + 20 && x + vx * seconds >= (-height/2) - 20 
        then 
            x + vx * seconds 
        else 
            x
    y' = if y + vy * seconds <= width/2 + 20 && y + vy * seconds >= (-width/2) - 20
        then
            y + vy * seconds
        else
            y

removeBullets :: [Bullet] -> [Bullet]
removeBullets (h:t) = newList
    where
        Bullet w x y z = h
        newList = if x >= (-width/2) - 20 && x <= width/2 + 20 && w >= (-height/2) - 20 && w <= height/2+20
            then
                h: removeBullets t
            else
                removeBullets t

removeBullets [h] = newList
    where
        Bullet w x y z = h
        newList = if x >= (-width/2) - 20 && x <= width/2 + 20 && w >= (-height/2) - 20 && w <= height/2 + 20
            then
                [h]
            else
                []

removeBullets [] = []