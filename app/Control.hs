module Control where

import Model

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 'r' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'r') _ _ _) game =
    game { ballLoc = (0, 0)  }

-- For an 'p' keypress, pause.
handleKeys (EventKey (Char 'p') Down _ _) game =
    game { gamePaused = not (gamePaused game)  }

-- For an 'w' keypress, move left player up
handleKeys (EventKey (Char 'w') Down _ _) game =
    game { p2speed = p2speed game + 50  }
handleKeys (EventKey (Char 'w') Up _ _) game =
    game { p2speed = p2speed game - 50  }

-- For an 'a' keypress, move left player down
handleKeys (EventKey (Char 's') Down _ _) game =
    game { p2speed = p2speed game - 50  }
handleKeys (EventKey (Char 's') Up _ _) game =
    game { p2speed = p2speed game + 50  }

-- For an 's' keypress, move right player up
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game =
    game { p1speed = p1speed game + 50  }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game =
    game { p1speed = p1speed game - 50  }

-- For an 'd' keypress, move right player down
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game =
    game { p1speed = p1speed game - 50  }
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game =
    game { p1speed = p1speed game + 50  }

-- | Update game
update :: Float -> VGame -> VGame
update seconds = moveSprite seconds
