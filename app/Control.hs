module Control where

import Model
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 'r' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'r') _ _ _) game =
    game { ballLoc = (0, 0)  }

-- For an 'p' keypress, pause.
handleKeys (EventKey (Char 'p') Down _ _) game =
    game { gamePaused = not (gamePaused game)  }

-- For an 'w' keypress, move player up
handleKeys (EventKey (Char 'w') Down _ _) game =
    game { pspeed = pspeed game + 50  }
handleKeys (EventKey (Char 'w') Up _ _) game =
    game { pspeed = pspeed game - 50  }

-- For an 's' keypress, move player down
handleKeys (EventKey (Char 's') Down _ _) game =
    game { pspeed = pspeed game - 50  }
handleKeys (EventKey (Char 's') Up _ _) game =
    game { pspeed = pspeed game + 50  }

-- For an 'a' keypress, move player left
handleKeys (EventKey (Char 'a') Down _ _) game =
    game { pspeed = pspeed game + 50  }
handleKeys (EventKey (Char 'a') Up _ _) game =
    game { pspeed = pspeed game - 50  }

-- For an 'd' keypress, move player right
handleKeys (EventKey (Char 'd') Down _ _) game =
    game { pspeed = pspeed game - 50  }
handleKeys (EventKey (Char 'd') Up _ _) game =
    game { pspeed = pspeed game + 50  }

-- | Update game
update :: Float -> VGame -> VGame
update seconds = moveSprite seconds
