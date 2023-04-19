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
        Entity a b c d e = player game
        newEntity = Entity a b c \x -> 

handleKeys (EventKey (Char 'w') Up _ _) game =
    game { player = Sprite w x (y-150) z  }
    where
        Sprite w x y z = player game

-- For an 's' keypress, move player down
handleKeys (EventKey (Char 's') Down _ _) game =
    game { player = Sprite w x (y-150) z  }
    where
        Sprite w x y z = player game
handleKeys (EventKey (Char 's') Up _ _) game =
    game { player = Sprite w x (y+150) z  }
    where
        Sprite w x y z = player game

-- For an 'a' keypress, move player left
handleKeys (EventKey (Char 'a') Down _ _) game =
    game { player = Sprite w x y (z-150)  }
    where
        Sprite w x y z = player game
handleKeys (EventKey (Char 'a') Up _ _) game =
    game { player = Sprite w x y (z+150)  }
    where
        Sprite w x y z = player game

-- For an 'd' keypress, move player right
handleKeys (EventKey (Char 'd') Down _ _) game =
    game { player = Sprite w x y (z+150)  }
    where
        Sprite w x y z = player game
handleKeys (EventKey (Char 'd') Up _ _) game =
    game { player = Sprite w x y (z-150)  }
    where
        Sprite w x y z = player game

-- For a 'j' keypress, shoot current equipped weapon
handleKeys (EventKey (Char 'j') Down _ _) game =
    game { isShooting = True  }
handleKeys (EventKey (Char 'j') Up _ _) game =
    game { isShooting = False }

-- Do nothing for all other events.
handleKeys _ game = game

-- | Update game
update :: Float -> VGame -> VGame
update seconds = movePlayer seconds $ moveentity seconds
