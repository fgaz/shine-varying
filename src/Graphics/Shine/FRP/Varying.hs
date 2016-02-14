module Graphics.Shine.FRP.Varying (
  ShineInput(..),
  playVaryingIO,
  timeNumeric,
  timeEvent,
  isDownButton,
  isDownKey,
  mouseMove
) where

import Graphics.Shine.Input
import Graphics.Shine.Picture
import Graphics.Shine
import Control.Varying.Core
import Control.Varying.Event
import Web.KeyCode


data ShineInput = Input Input | Time Float


playVaryingIO :: Float -> (Int, Int) -> Var IO ShineInput Picture -> IO ()
playVaryingIO fps dims v =
    playIO fps dims (Empty, v) (\_ x-> return $ fst x) handleInput step

handleInput :: a -> Input -> (Picture, Var IO ShineInput Picture) -> IO (Picture, Var IO ShineInput Picture)
handleInput _ i (_,v) = do
  v' <- execVar v $ Input i
  return (Empty, v')

step :: a -> Float -> (Picture, Var IO ShineInput Picture) -> IO (Picture, Var IO ShineInput Picture)
step _ t (_,v) = runVar v $ Time t

-- ## Useful Vars

--MAYBE wrap in Event

timeNumeric :: Var IO ShineInput Float
timeNumeric = var f
  where
    f (Input _) = 0
    f (Time t) = t

timeEvent :: Var IO ShineInput (Event Float)
timeEvent = var f
  where
    f (Input _) = NoEvent
    f (Time t) = Event t


isDownButton :: MouseBtn -> Var IO ShineInput Bool
isDownButton b = accumulate f False
  where
    f _ (Input (MouseBtn b' Down _)) | b == b' = True
    f _ (Input (MouseBtn b' Up _)) | b == b' = False
    f s _ = s

--mouseButtonsDown :: Var IO ShineInput [MouseButton]

mouseMove :: Var IO ShineInput (Int,Int)
mouseMove = accumulate f (0,0)
  where
    f _ (Input (MouseMove coords)) = coords
    f s _ = s


isDownKey :: Key -> Var IO ShineInput Bool
isDownKey k = accumulate f False
  where
    f _ (Input (Keyboard k' Down _)) | k == k' = True
    f _ (Input (Keyboard k' Up _)) | k == k' = False
    f s _ = s

--keysDown :: Var IO ShineInput [Key]
