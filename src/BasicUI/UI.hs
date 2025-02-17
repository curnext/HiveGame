module BasicUI.UI where

import Graphics.Vty


class ShiftAble s where
  (<-<<) :: s -> (Int, Int) -> s
  (>>->) :: s -> (Int, Int) -> s
  s >>-> (ix, iy) = s <-<< (-ix, -iy)

newtype UI = UI {
  getUI :: [(String, Attr, (Int, Int))]
}

instance Semigroup UI where
  UI a <> UI b = UI (a <> b)

instance Monoid UI where
  mempty = UI mempty

class MoveAble ma where
  maPos     :: ma -> (Int, Int)
  maSetPos  :: (Int, Int) -> ma -> ma


renderPicture :: UI -> Picture
renderPicture (UI xs) =
  let
    toImage :: (String, Attr, (Int, Int)) -> Image
    toImage (s,a,(x,y)) = translate x y $ string a s
    images = map toImage xs
  in
    Picture NoCursor images ClearBackground


shiftUI :: (Int, Int) -> UI -> UI
shiftUI dp (UI ui) = UI $ map (\((s,a,(x,y)),(dx,dy)) -> (s,a,(x+dx,y+dy))) $ zip ui (repeat dp)

charVLine :: (Int,Int) -> Int -> Attr -> Char -> UI
charVLine (sx,sy) l a c = UI $ map (\i -> ([c],a,(sx,sy+i))) [0..l]

charHLine :: (Int,Int) -> Int -> Attr -> Char -> UI
charHLine (sx,sy) l a c = UI [(replicate l c,a,(sx,sy))]

charFillRect :: (Int,Int) -> (Int,Int) -> Attr -> Char -> UI
charFillRect (px,py) (sx,sy) a c =
  let
    line = replicate sx c
    lines = replicate sy line
    ui = map (\(ln, i) -> (ln, a, (px,py+i))) $ zip lines [0..]
  in
    UI ui

mix  :: Color -> Color -> Attr
mix b f = defAttr `withBackColor` b `withForeColor` f

classicWindows :: String -> (Int,Int) -> Color -> UI
classicWindows str (sx,sy) tc =
  let
    title = UI [(str, (mix tc white), (1,0))]
    tb = charHLine (0,0) sx (mix tc white) ' '
    lb = charVLine (0,1) (sy-2) (mix white tc) '|'
    rb = charVLine (sx-1,1) (sy-2) (mix white tc) '|'
    bb = charHLine (0,sy-1) sx (mix white tc) '='
  in
    mconcat [title,tb,lb,rb,bb]