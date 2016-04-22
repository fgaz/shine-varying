{-|
Module      : Graphics.Shine.FRP.Varying
Description : FRP interface for shine
Copyright   : (c) Francesco Gazzetta, 2016
License     : MIT
Maintainer  : francygazz@gmail.com
Stability   : experimental

This package lets you interact with the screen Elm-style.
This is especially useful for small games or visualizations.
You can get something on the screen quickly using the Vars provided below.

Try to run this:

> import Graphics.Shine.FRP.Varying
> import Graphics.Shine
> import Graphics.Shine.Picture
> import Graphics.Shine.Image
> import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
>
> resizeImage img (x',y') = Translate (x/2) (y/2) -- Pictures are centered on (0,0), so we need to move it
>                         $ Image (Stretched x y) img -- Scale de picture to the given position
>   where
>     x = fromIntegral x' -- mousePosition is Integral
>     y = fromIntegral y'
>
> main :: IO ()
> main = runWebGUI $ \ webView -> do
>     ctx <- fixedSizeCanvas webView 1024 768
>     Just doc <- webViewGetDomDocument webView
>     narwhal <- makeImage "https://wiki.haskell.org/wikiupload/8/85/NarleyYeeaaahh.jpg"
>     let resizedNarwhal = resizeImage narwhal <$> mousePosition
>     playVarying ctx doc 30 resizedNarwhal


-}
module Graphics.Shine.FRP.Varying (
  ShineInput(..),
  -- * Running the Var
  playVarying,
  playVaryingIO,
  -- * Useful Vars
  timeDeltaNumeric,
  timeDeltaEvent,
  time,
  isDownButton,
  isDownKey,
  mousePosition,
  mouseButtonsDown,
  keysDown
) where

import Graphics.Shine.Input
import Graphics.Shine.Picture
import Graphics.Shine
import Control.Varying.Core
import Control.Varying.Event
import Control.Category ((.))
import Prelude hiding ((.))
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
playVarying :: (IsEventTarget eventElement, IsDocument eventElement)
            => CanvasRenderingContext2D -- ^ The context to draw on
            -> eventElement -- ^ the element used to catch events
            -> Float -- ^ FPS
            -> Var ShineInput Picture -- ^ A 'Var' that maps time and input events to a 'Picture'
            -> IO ()
playVarying ctx doc fps v =
    play ctx doc fps (Empty, v) fst (\a b -> runIdentity $ handleInput a b) (\a b -> runIdentity $ step a b)

-- | Feed the input to the VarT IO and draw the result
playVaryingIO :: (IsEventTarget eventElement, IsDocument eventElement)
              => CanvasRenderingContext2D -- ^ The context to draw on
              -> eventElement -- ^ the element used to catch events
              -> Float -- ^ FPS
              -> VarT IO ShineInput Picture -- ^ An effectful 'VarT' that maps time and input events to a 'Picture'
              -> IO ()
playVaryingIO ctx doc fps v =
    playIO ctx doc fps (Empty, v) (return . fst) handleInput step

handleInput :: Monad m => Input -> (Picture, VarT m ShineInput Picture) -> m (Picture, VarT m ShineInput Picture)
handleInput i (_,v) = do
  v' <- snd <$> runVarT v (Input i)
  return (Empty, v')

step :: Monad m => Float -> (Picture, VarT m ShineInput Picture) -> m (Picture, VarT m ShineInput Picture)
step t (_,v) = runVarT v $ Time t


-- | Time delta. On non-time inputs the value is 0.
timeDeltaNumeric :: Monad m => VarT m ShineInput Float
timeDeltaNumeric = var f
  where
    f (Input _) = 0
    f (Time t) = t

-- | Time delta. On non-time inputs the value is NoEvent.
timeDeltaEvent :: Monad m => VarT m ShineInput (Event Float)
timeDeltaEvent = var f
  where
    f (Input _) = NoEvent
    f (Time t) = Event t

-- | Time since beginning.
time :: Monad m => VarT m ShineInput Float
time = accumulate (+) 0 . timeDeltaNumeric


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
mousePosition :: Monad m => VarT m ShineInput (Int,Int)
mousePosition = accumulate f (0,0)
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
