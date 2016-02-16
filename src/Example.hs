import Prelude hiding ((.))
import Control.Category ((.))
import Graphics.Shine.FRP.Varying
import Graphics.Shine.Input
import Graphics.Shine.Picture
import Control.Varying.Core
import Web.KeyCode

main :: IO ()
main = playVarying 30 (800,600) ( expandingRectangle
                               <> (Colored (Color 255 0 255 1) <$> Translate 200 200 <$> keys)
                               <> (Colored (Color 255 0 0 1) <$> arrowsCircle)
                               <> (Colored (Color 255 0 0 1) <$> trail redTransparency 20 arrowsCircle)
                               <> (Colored (Color 0 200 100 1) <$> trail lightGreenTransparency 5 mouseCircle) )

time :: Var ShineInput Float
time = accumulate
           (\t (btnDown,td) -> if btnDown then 0 else t+td) --reset if clicked
           0
       . ((,) <$> isDownButton BtnLeft <*> timeNumeric) --(click,time delta)

expandingRectangle :: Var ShineInput Picture
expandingRectangle = fmap (\x -> RectF (x*500) (x*100)) time

vX :: Var ShineInput Float
vX = accumulate addV 0 . ((,) <$> isDownKey ArrowLeft <*> isDownKey ArrowRight)
vY :: Var ShineInput Float
vY = accumulate addV 0 . ((,) <$> isDownKey ArrowUp <*> isDownKey ArrowDown)

addV :: Num a => a -> (Bool, Bool) -> a
addV v (False, True) = v+5
addV v (True, False) = v-5
addV v _ = v

posX :: Var ShineInput Float
posX = accumulate (+) 400 . (vX * timeNumeric)
posY :: Var ShineInput Float
posY = accumulate (+) 300 . (vY * timeNumeric)

arrowsCircle :: Var ShineInput Picture
arrowsCircle = arrowsCircle' <$> posX <*> posY
  where arrowsCircle' x y = Translate x y $ CircleF 10

mouseCircle :: Var ShineInput Picture
mouseCircle = translateMouse <*> pure (CircleF 20)

translateMouse :: Var ShineInput (Picture -> Picture)
translateMouse = uncurry Translate <$> (bimap fromIntegral <$> mouseMove)
  where bimap f (a, b) = (f a, f b)

keys :: Var ShineInput Picture
keys = (Text "12px sans" CenterAlign 400 . show) <$> keysDown

redTransparency :: Float -> Color
redTransparency = Color 255 0 0
lightGreenTransparency :: Float -> Color
lightGreenTransparency = Color 0 200 100

--TODO bubbling to prevent it to depend on the number of events
trail :: Monad m => (Float -> Color) -> Int -> VarT m a Picture -> VarT m a Picture
trail transparency l' v = foldMap (\(t,p) -> Colored (transparency t) <$> p)
                        $ zip (map (/l) [l,(l-1)..(l/10)])
                        $ history Empty v
  where l = fromIntegral l'

history :: Monad m => b -> VarT m a b -> [VarT m a b]
history def v = v' : history def v'
  where v' = delay def v
