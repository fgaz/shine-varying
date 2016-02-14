import Prelude hiding ((.))
import Control.Category ((.))
import Graphics.Shine.FRP.Varying
import Graphics.Shine.Input
import Graphics.Shine.Picture
import Control.Varying.Core
import Web.KeyCode

main :: IO ()
main = playVaryingIO 30 (800,600) ( expandingRectangle
                                 <> (Colored (Color 255 0 0 1) <$> movingCircle)
                                 <> (Colored (Color 255 0 0 1) <$> trail redTransparency 20 movingCircle)
                                 <> (Colored (Color 0 200 100 1) <$> trail lightGreenTransparency 5 mouseCircle) )

time :: Var IO ShineInput Float
time = accumulate
           (\t (btnDown,td) -> if btnDown then 0 else t+td) --reset if clicked
           0
       . ((,) <$> isDownButton BtnLeft <*> timeNumeric) --(click,time delta)

expandingRectangle :: Var IO ShineInput Picture
expandingRectangle = fmap (\x -> RectF (x*500) (x*100)) time

vX :: Var IO ShineInput Float
vX = accumulate addV 0 . ((,) <$> isDownKey ArrowLeft <*> isDownKey ArrowRight)
vY :: Var IO ShineInput Float
vY = accumulate addV 0 . ((,) <$> isDownKey ArrowUp <*> isDownKey ArrowDown)

addV :: Num a => a -> (Bool, Bool) -> a
addV v (False, True) = v+5
addV v (True, False) = v-5
addV v _ = v

posX :: Var IO ShineInput Float
posX = accumulate (+) 400 . (vX * timeNumeric)
posY :: Var IO ShineInput Float
posY = accumulate (+) 300 . (vY * timeNumeric)

arrowsCircle :: Var IO ShineInput Picture
arrowsCircle = arrowsCircle' <$> posX <*> posY
  where arrowsCircle' x y = Translate x y $ CircleF 10

mouseCircle :: Var IO ShineInput Picture
mouseCircle = translateMouse <*> pure (CircleF 20)

translateMouse :: Var IO ShineInput (Picture -> Picture)
translateMouse = uncurry Translate <$> (bimap fromIntegral <$> mouseMove)
  where bimap f (a, b) = (f a, f b)

redTransparency :: Float -> Color
redTransparency = Color 255 0 0
lightGreenTransparency :: Float -> Color
lightGreenTransparency = Color 0 200 100

--TODO bubbling to prevent it to depend on the number of events
trail :: Monad m => (Float -> Color) -> Int -> Var m a Picture -> Var m a Picture
trail transparency l' v = foldMap (\(t,p) -> Colored (transparency t) <$> p)
                        $ zip (map (/l) [l,(l-1)..(l/10)])
                        $ history Empty v
  where l = fromIntegral l'

history :: Monad m => b -> Var m a b -> [Var m a b]
history def v = v' : history def v'
  where v' = delay def v
