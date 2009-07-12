module Input (Input(..), updateInput, emptyInput) where

import Graphics.UI.SDL 

data Input = Input { inRArrow :: Bool
                   , inUArrow :: Bool
                   , inLArrow :: Bool
                   , inDArrow :: Bool 
                   , inQuit :: Bool
                   , inCtrl :: Bool
                   , inSpace :: Bool
                   }

emptyInput :: Input
emptyInput = Input { inRArrow = False
                   , inUArrow = False
                   , inLArrow = False
                   , inDArrow = False
                   , inQuit = False
                   , inCtrl = False
                   , inSpace = False
                   }

updateInput :: Input -> [Event] -> Input
updateInput = foldl f
    where
        f inp (KeyDown Keysym { symKey = key }) = keyDown inp key
        f inp (KeyUp Keysym { symKey = key }) = keyUp inp key
        f inp Quit = inp { inQuit = True }
        f inp _ = inp

        keyDown inp SDLK_LEFT = inp { inLArrow = True }
        keyDown inp SDLK_RIGHT = inp { inRArrow = True }
        keyDown inp SDLK_UP = inp { inUArrow = True }
        keyDown inp SDLK_DOWN = inp { inDArrow = True }
        keyDown inp SDLK_SPACE = inp { inSpace = True }
        keyDown inp _ = inp

        keyUp inp SDLK_LEFT = inp { inLArrow = False }
        keyUp inp SDLK_RIGHT = inp { inRArrow = False }
        keyUp inp SDLK_UP = inp { inUArrow = False }
        keyUp inp SDLK_DOWN = inp { inDArrow = False }
        keyUp inp SDLK_SPACE = inp { inSpace = False }
        keyUp inp _ = inp
