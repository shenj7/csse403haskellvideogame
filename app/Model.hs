module Model where

-- | Sprite definition
-- | x location, y location, x velocity, y velocity
data Sprite = Sprite Float Float Float Float -- Player Float Float Float Float | Enemy Float Float Float Float | Bullet Float Float Float Float

-- | Gun Definition
-- | bullet size, speed
data GunData = GunData Float Float

-- | Bullet Definition
-- | bullet position, speed
data Bullet = Bullet Float Float Float Float

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
    bullets :: [Bullet]
}

-- | Initial state
initialState :: VGame
initialState = Game {
    player = Sprite resetpos 0 0 0,
    gamePaused = False,
    isShooting = False,
    normalGun = GunData 7 500,
    bullets = []
}

-- | Update sprite
moveSprite :: Float -> VGame -> VGame
moveSprite seconds game = game { player = newSprite, bullets = newBullets } -- | figure out how to make it get anything out instead of just a player
    where
        -- Old locations and velocities.
        Sprite w x y z = player game
        oldbullets = bullets game
        shooting = isShooting game

        -- New locations.
        bullet = Bullet w x 500 0
        oldbullets2 = if shooting then oldbullets ++ [bullet] else oldbullets
        newSprite = calcLoc seconds (Sprite w x y z)
        func = calcBulletLoc seconds
        oldbullets3 = removeBullets oldbullets2
        newBullets = map func oldbullets3

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
    x' = if x + vx * seconds <= 250 && x + vx * seconds >= (-height/2) - 20 
        then 
            x + vx * seconds 
        else 
            x
    y' = if y + vy * seconds <= width/2 + 20 && y + vy * seconds >= (-width/2) - 20
        then
            y + vy * seconds
        else
            y

-- calcoll :: Float -> Bool

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