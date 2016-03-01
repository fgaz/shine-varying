module Graphics.Shine.FRP.Varying (
  ShineInput(..),
  playVarying,
  playVaryingIO,
  timeNumeric,
  timeEvent,
  isDownButton,
  isDownKey,
  mouseMove,
  mouseButtonsDown,
  keysDown
) where

import Graphics.Shine.Input
import Graphics.Shine.Picture
import Graphics.Shine
import Control.Varying.Core
import Control.Varying.Event
import Web.KeyCode
import Data.Functor.Identity
import Data.List (delete)
import GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import GHCJS.DOM.Types (IsDocument)
import GHCJS.DOM.EventTarget (IsEventTarget)


-- | Datatype representing all possible inputs coming from shine's main loop
data ShineInput =
    -- | An input event (keypress, mousemove...)
    Input Input
    -- | An advancement in time
    | Time Float


-- | Feed the input to the Var and draw the result
playVarying :: (IsEventTarget eventElement, IsDocument eventElement) => CanvasRenderingContext2D -> eventElement -> Float -> Var ShineInput Picture -> IO ()
playVarying ctx doc fps v =
    play ctx doc fps (Empty, v) fst (\a b -> runIdentity $ handleInput a b) (\a b -> runIdentity $ step a b)

-- | Feed the input to the VarT IO and draw the result
playVaryingIO :: (IsEventTarget eventElement, IsDocument eventElement) => CanvasRenderingContext2D -> eventElement -> Float -> VarT IO ShineInput Picture -> IO ()
playVaryingIO ctx doc fps v =
    playIO ctx doc fps (Empty, v) (return . fst) handleInput step

handleInput :: Monad m => Input -> (Picture, VarT m ShineInput Picture) -> m (Picture, VarT m ShineInput Picture)
handleInput i (_,v) = do
  v' <- execVar v $ Input i
  return (Empty, v')

step :: Monad m => Float -> (Picture, VarT m ShineInput Picture) -> m (Picture, VarT m ShineInput Picture)
step t (_,v) = runVarT v $ Time t

-- ## Useful Vars

--MAYBE wrap in Event

-- | Time since beginning. On non-time inputs the value is 0.
timeNumeric :: Monad m => VarT m ShineInput Float
timeNumeric = var f
  where
    f (Input _) = 0
    f (Time t) = t

-- | Time since beginning. On non-time inputs the value is NoEvent.
timeEvent :: Monad m => VarT m ShineInput (Event Float)
timeEvent = var f
  where
    f (Input _) = NoEvent
    f (Time t) = Event t


-- | Whether a mouse button is pressed.
isDownButton :: Monad m => MouseBtn -> VarT m ShineInput Bool
isDownButton b = accumulate f False
  where
    f _ (Input (MouseBtn b' Down _)) | b == b' = True
    f _ (Input (MouseBtn b' Up _)) | b == b' = False
    f s _ = s

-- | A list of mouse buttons that are currently being pressed.
mouseButtonsDown :: Monad m => VarT m ShineInput [MouseBtn]
mouseButtonsDown = accumulate f []
  where
    f bs (Input (MouseBtn b Down _)) = if b `elem` bs then bs else b:bs
    f bs (Input (MouseBtn b Up _)) = delete b bs
    f bs _ = bs

-- | The pointer's position, relative to the canvas.
-- The top-left corner is the origin.
mouseMove :: Monad m => VarT m ShineInput (Int,Int)
mouseMove = accumulate f (0,0)
  where
    f _ (Input (MouseMove coords)) = coords
    f s _ = s


-- | Whether a key is pressed.
isDownKey :: Monad m => Key -> VarT m ShineInput Bool
isDownKey k = accumulate f False
  where
    f _ (Input (Keyboard k' Down _)) | k == k' = True
    f _ (Input (Keyboard k' Up _)) | k == k' = False
    f s _ = s

-- | A list of keys that are currently being pressed.
keysDown :: Monad m => VarT m ShineInput [Key]
keysDown = accumulate f []
  where
    f ks (Input (Keyboard k Down _)) = if k `elem` ks then ks else k:ks
    f ks (Input (Keyboard k Up _)) = delete k ks
    f ks _ = ks
