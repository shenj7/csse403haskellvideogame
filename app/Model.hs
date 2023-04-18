module Model where

-- | Sprite definition
-- | x location, y location, x velocity, y velocity
data Sprite = Sprite Float Float Float Float -- Player Float Float Float Float | Enemy Float Float Float Float | Bullet Float Float Float Float

-- | Gun Definition
-- | bullet size, speed
data GunData = GunData Float Float

-- | Game parameters
width, height, offset, playerSpeed :: Int
width = 1000
height = 800
offset = 10
playerSpeed = 10

-- | Game state
data VGame = Game {
    player :: Sprite,
    gamePaused  :: Bool,
    isShooting :: Bool,
    normalGun :: GunData
}

-- | Initial state
initialState :: VGame
initialState = Game {
    player = Sprite 0 0 0 0,
    gamePaused = False,
    isShooting = False,
    normalGun = GunData 7 500
}

-- | Update sprite
moveSprite :: Float -> VGame -> VGame
moveSprite seconds game = game { player = newSprite } -- | figure out how to make it get anything out instead of just a player
    where
        -- Old locations and velocities.
        oldSprite = player game

        -- New locations.
        newSprite = calcLoc seconds oldSprite

-- | Get new location of sprite
calcLoc :: Float -> Sprite -> Sprite
calcLoc seconds (Sprite x y vx vy) = Sprite x' y' vx vy where
    x' = if x + vx * seconds <= 400 && x + vx * seconds >= (-400) 
        then 
            x + vx * seconds 
        else 
            x
    y' = if y + vy * seconds <= 500 && y + vy * seconds >= (-500)
        then
            y + vy * seconds
        else
            y

-- |
