import Graphics.Shine.FRP.Varying
import Graphics.Shine
import Graphics.Shine.Picture
import Graphics.Shine.Image
import GHCJS.DOM (webViewGetDomDocument, runWebGUI)

resizeImage img (x',y') = Translate (x/2) (y/2) -- Pictures are centered on (0,0), so we need to move it
                        $ Image (Stretched x y) img -- Scale de picture to the given position
  where
    x = fromIntegral x' -- mousePosition is Integral
    y = fromIntegral y'

main :: IO ()
main = runWebGUI $ \ webView -> do
    ctx <- fixedSizeCanvas webView 1024 768
    Just doc <- webViewGetDomDocument webView
    narwhal <- makeImage "https://wiki.haskell.org/wikiupload/8/85/NarleyYeeaaahh.jpg"
    let resizedNarwhal = resizeImage narwhal <$> mousePosition
    playVarying ctx doc 30 resizedNarwhal
