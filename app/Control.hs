module Control where

import Model
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- | Respond to key events.
handleKeys :: Event -> VGame -> VGame

-- For an 'r' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'n') _ _ _) game =
    game { player = playerEntity }

-- For an 'p' keypress, pause.
handleKeys (EventKey (Char 'p') Down _ _) game =
    game { gamePaused = not (gamePaused game)  }

-- For an 'w' keypress, move player up
handleKeys (EventKey (Char 'w') Down _ _) game =
    game { player = newEntity  }
    where
        oldplayer = player game
        (vx, vy) = vel oldplayer
        newEntity = oldplayer {vel = (vx, vy+playerSpeed)}
handleKeys (EventKey (Char 'w') Up _ _) game =
    game { player = newEntity  }
    where
        oldplayer = player game
        (vx, vy) = vel oldplayer
        newEntity = oldplayer {vel = (vx, vy-playerSpeed)}

-- For an 's' keypress, move player down
handleKeys (EventKey (Char 's') Down _ _) game =
    game { player = newEntity  }
    where
        oldplayer = player game
        (vx, vy) = vel oldplayer
        newEntity = oldplayer {vel = (vx, vy-playerSpeed)}
handleKeys (EventKey (Char 's') Up _ _) game =
    game { player = newEntity  }
    where
        oldplayer = player game
        (vx, vy) = vel oldplayer
        newEntity = oldplayer {vel = (vx, vy+playerSpeed)}

-- For an 'a' keypress, move player left
handleKeys (EventKey (Char 'a') Down _ _) game =
    game { player = newEntity  }
    where
        oldplayer = player game
        (vx, vy) = vel oldplayer
        newEntity = oldplayer {vel = (vx-playerSpeed, vy)}
handleKeys (EventKey (Char 'a') Up _ _) game =
    game { player = newEntity  }
    where
        oldplayer = player game
        (vx, vy) = vel oldplayer
        newEntity = oldplayer {vel = (vx+playerSpeed, vy)}

-- For an 'd' keypress, move player right
handleKeys (EventKey (Char 'd') Down _ _) game =
    game { player = newEntity  }
    where
        oldplayer = player game
        (vx, vy) = vel oldplayer
        newEntity = oldplayer {vel = (vx+playerSpeed, vy)}
handleKeys (EventKey (Char 'd') Up _ _) game =
    game { player = newEntity  }
    where
        oldplayer = player game
        (vx, vy) = vel oldplayer
        newEntity = oldplayer {vel = (vx-playerSpeed, vy)}

-- For a 'j' keypress, shoot current equipped weapon
handleKeys (EventKey (Char 'j') Down _ _) game =
    game { isShooting = True, gun = DefaultGun  }
-- For a 'k' keypress, shoot current equipped weapon
handleKeys (EventKey (Char 'k') Down _ _) game =
    game { isShooting = True , gun = TripleGun }
-- For a 'l' keypress, shoot current equipped weapon
handleKeys (EventKey (Char 'l') Down _ _) game =
    game { isShooting = True , gun = HugeGun }

-- Do nothing for all other events.
handleKeys _ game = game
