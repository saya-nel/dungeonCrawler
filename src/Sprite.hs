module Sprite where

import Control.Monad.IO.Class (MonadIO)

import Foreign.C.Types (CInt)

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import SDL.Vect (V2 (..), Point (..))

import SDL.Video.Renderer (Renderer, Texture, Rectangle (..))
import qualified SDL.Video.Renderer as R

import TextureMap (TextureMap, TextureId)
import qualified TextureMap as TM

import qualified Debug.Trace as T


type Area = Rectangle CInt

data Image =
  Image { textureId :: TextureId
        , sourceArea :: Area }

createImage :: TextureId -> Area -> Image
createImage txt rct = Image txt rct

data Sprite =
  Sprite { images :: Seq Image
         , current :: Int
         , destArea :: Area }

createEmptySprite :: Sprite
createEmptySprite = Sprite Seq.empty 0 (mkArea 0 0 0 0) 

addImage :: Sprite -> Image -> Sprite
addImage sp@(Sprite { images=is }) img = sp { images = is :|> img }

changeImage :: Sprite -> Int -> Sprite
changeImage sp@(Sprite { images = imgs }) new
  | Seq.null imgs = error $ "Cannot change sprite image, no image in sprite"
  | (new < 0) || (new > Seq.length imgs) = error $ "Cannot change sprite image, bad index: " <> (show new)
  | otherwise = sp { current= new }

cycleImage :: Sprite -> Sprite
cycleImage sp@(Sprite { images = imgs, current = cur }) =
  let new = if cur == Seq.length imgs - 1 then 0 else cur + 1
  in changeImage sp new

mkArea :: CInt -> CInt -> CInt -> CInt -> Area
mkArea x y w h = Rectangle (P (V2 x y)) (V2 w h)

moveArea :: Area -> CInt -> CInt -> Area
moveArea rect@(Rectangle _ wh) x y = Rectangle (P (V2 x y)) wh

resizeArea :: Area -> CInt -> CInt -> Area
resizeArea rect@(Rectangle p _) w h = Rectangle p (V2 w h)

moveTo :: Sprite -> CInt -> CInt -> Sprite
moveTo sp@(Sprite { destArea = dest }) x y = sp { destArea = moveArea dest x y }

scale :: Sprite -> CInt -> CInt -> Sprite
scale sp@(Sprite { destArea = dest}) w h = sp { destArea = resizeArea dest w h }

currentImage :: Sprite -> Image
currentImage (Sprite imgs cur _) = Seq.index imgs cur

defaultScale :: Sprite -> Sprite
defaultScale sp = case currentImage sp of
                    (Image _ (Rectangle _ (V2 w h))) -> scale sp w h

displaySprite :: Renderer -> TextureMap -> Sprite -> IO ()
displaySprite rdr tmap sp@(Sprite imgs cur dest) =
  case currentImage sp of
    (Image tid src) -> do
      let txt = TM.fetchTexture tid tmap
      R.copy rdr txt Nothing (Just dest)