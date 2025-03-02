module Hive.Grid where

import Hive.Hid

import Data.Word(Word8)
import qualified Data.ByteString as BS

data HiveGridMapper = HiveGridMapper {
  cxyToHid :: (Int, Int) -> HID
, cidToCxy :: CID -> (Int, Int)
, lidToCxy :: LID -> [(Int, Int)]
, nidToCxy :: NID -> (Int, Int)
}

class HiveGrid hg where
  hgSize          :: hg -> (Int, Int)
  hgSetSize       :: hg -> (Int, Int) -> hg
  hgOrigin        :: hg -> (Int, Int)
  hgSetOrigin     :: hg -> (Int, Int) -> hg
  hgMappers       :: hg -> ([HiveGridMapper], HiveGridMapper, [HiveGridMapper])
  hgSetMappers    :: hg -> ([HiveGridMapper], HiveGridMapper, [HiveGridMapper]) -> hg

  hgMapper        :: hg -> HiveGridMapper
  hgMapper hg = case (hgMappers hg) of (_,m,_) -> m

  hgZoomIn        :: hg -> hg
  hgZoomIn hg = case (hgMappers hg) of
    (_,_,[]) -> hg
    (a,c,m:ms) -> hgSetMappers hg (c:a,m,ms)

  hgZoomOut       :: hg -> hg
  hgZoomOut hg = case (hgMappers hg) of
    ([],_,_) -> hg
    (m:ms,c,a) -> hgSetMappers hg (ms,m,c:a)


-- given a size and tag mapper (in byte string)
-- returns a function that maps a screen coordinates into hive coordinates
-- this function is used to locate the hive elements user clicked on screen
cxy2hid :: Int -> BS.ByteString -> (Int, Int) -> HID
cxy2hid s tbl (cx, cy) =
  let
    (hx, rx) = cx `divMod` (3 * s)
    (hy, ry) = (cy - s * hx) `divMod` (2 * s)
    tag = BS.index tbl (3*s*ry + rx)
  in
    case tag of
      0 -> HCell $ Cell hx hy
      1 -> HCell $ Cell (hx+1) (hy-1)
      2 -> HCell $ Cell hx (hy-1)
      3 -> HLine $ LineT hx (hy-1)
      4 -> HLine $ LineL hx hy
      5 -> HLine $ LineR hx (hy-1)
      6 -> HLine $ LineT (hx+1) (hy-1)
      7 -> HLine $ LineT (hx+1) (hy-2)
      8 -> HNode $ NodeR hx (hy-1)
      9 -> HNode $ NodeL (hx+1) (hy-1)
      otherwise -> error ("undefined tag: " ++ show tag)

-- A serious treats all elements according to its ui
-- B serious have larger 'bounding box' for line elements

-- S1:
--      + +
--   + +   + +
--  +   + +   +
--   + +   + +
--  +   + +   +
--   + +   + +
--      + +
s1MapperA :: HiveGridMapper
s1MapperA = HiveGridMapper {
  cxyToHid = cxy2hid 1 $ BS.pack
    [0,0,9
    ,3,8,1]
, cidToCxy = \(Cell x y) -> (3*x, x+2*y)
, lidToCxy = const []
, nidToCxy = \nid -> case nid of
    NodeL x y -> (3*x-1, x+2*y-1)
    NodeR x y -> (3*x+1, x+2*y-1)
}

-- S2:
--          + + +
--         +     +
--    + + +       + + +
--   +     \     /     +
--  +       +---+       +
--   +     /     \     +
--    +---+       +---+
--   +     \     /     +
--  +       +---+       +
--   +     /     \     +
--    + + +       + + +
--         +     +
--          + + +
s2MapperA :: HiveGridMapper
s2MapperA = HiveGridMapper {
  cxyToHid = cxy2hid 2 $ BS.pack
    [0,0,0,0,9,6
    ,0,0,0,4,1,1
    ,3,3,8,1,1,1
    ,2,2,2,5,1,1]
, cidToCxy = \(Cell x y) -> (6*x, 2*x+4*y)
, lidToCxy = \lid -> case lid of
    LineT x y -> map (\i -> (6*x+i, 2*x+4*y-2)) [-1..1]
    LineL x y -> [(6*x-3, 2*x+4*y-1)]
    LineR x y -> [(6*x+3, 2*x+4*y+1)]
, nidToCxy = \nid -> case nid of
    NodeL x y -> (6*x-2, 2*x+4*y-2)
    NodeR x y -> (6*x+2, 2*x+4*y-2)
}

s2MapperB :: HiveGridMapper
s2MapperB = s2MapperA {
  cxyToHid = cxy2hid 2 $ BS.pack
    [0,0,0,9,9,6
    ,3,3,4,4,4,6
    ,3,3,8,8,1,1
    ,3,3,5,5,5,7]
}

-- S3:
--              + + + +
--             +       +
--            +         +
--     + + + +           + + + +
--    +       \         /       +
--   +         \       /         +
--  +           +-----+           +
--   +         /       \         +
--    +       /         \       +
--     +-----+           +-----+
--    +       \         /       +
--   +         \       /         +
--  +           +-----+           +
--   +         /       \         +
--    +       /         \       +
--     + + + +           + + + +
--            +         +
--             +       +
--              + + + +
s3MapperA :: HiveGridMapper
s3MapperA = HiveGridMapper {
  cxyToHid = cxy2hid 3 $ BS.pack
    [0,0,0,0,0,0,9,6,6
    ,0,0,0,0,0,4,1,1,1
    ,0,0,0,0,4,1,1,1,1
    ,3,3,3,8,1,1,1,1,1
    ,2,2,2,2,5,1,1,1,1
    ,2,2,2,2,2,5,1,1,1]
, cidToCxy = \(Cell x y) -> (9*x, 3*x+6*y)
, lidToCxy = \lid -> case lid of
    LineT x y -> map (\i -> (9*x+i, 3*x+6*y-3)) [-2..2]
    LineL x y -> [ (9*x-5, 3*x+6*y-1)
                 , (9*x-4, 3*x+6*y-2)]
    LineR x y -> [ (9*x+5, 3*x+6*y-1)
                 , (9*x+4, 3*x+6*y-2)]
, nidToCxy = \nid -> case nid of
    NodeL x y -> (9*x-3, 3*x+6*y-3)
    NodeR x y -> (9*x+3, 3*x+6*y-3)
}

s3MapperB :: HiveGridMapper
s3MapperB = s3MapperA {
  cxyToHid = cxy2hid 3 $ BS.pack
    [0,0,0,0,0,9,9,6,6
    ,0,0,0,0,4,4,4,6,6
    ,3,3,3,4,4,4,1,1,1
    ,3,3,3,8,8,1,1,1,1
    ,3,3,3,5,5,5,1,1,1
    ,2,2,2,2,5,5,5,7,7]
}

-- S4:
--                  + + + + +
--                 +         +
--                +           +
--               +             +
--      + + + + +               + + + + +
--     +         \             /         +
--    +           \           /           +
--   +             \         /             +
--  +               +-------+               +
--   +             /         \             +
--    +           /           \           +
--     +         /             \         +
--      +-------+               +-------+
--     +         \             /         +
--    +           \           /           +
--   +             \         /             +
--  +               +-------+               +
--   +             /         \             +
--    +           /           \           +
--     +         /             \         +
--      + + + + +               + + + + +
--               +             +
--                +           +
--                 +         +
--                  + + + + +
s4MapperA :: HiveGridMapper
s4MapperA = HiveGridMapper {
  cxyToHid = cxy2hid 4 $ BS.pack
    [0,0,0,0,0,0,0,0,9,6,6,6
    ,0,0,0,0,0,0,0,4,1,1,1,1
    ,0,0,0,0,0,0,4,1,1,1,1,1
    ,0,0,0,0,0,4,1,1,1,1,1,1
    ,3,3,3,3,8,1,1,1,1,1,1,1
    ,2,2,2,2,2,5,1,1,1,1,1,1
    ,2,2,2,2,2,2,5,1,1,1,1,1
    ,2,2,2,2,2,2,2,5,1,1,1,1]
, cidToCxy = \(Cell x y) -> (12*x, 4*x+8*y)
, lidToCxy = \lid -> case lid of
    LineT x y -> map (\i -> (12*x+i, 4*x+8*y-4)) [-3..3]
    LineL x y -> [ (12*x-7, 4*x+8*y-1)
                 , (12*x-6, 4*x+8*y-2)
                 , (12*x-5, 4*x+8*y-3)]
    LineR x y -> [ (12*x+7, 4*x+8*y-1)
                 , (12*x+6, 4*x+8*y-2)
                 , (12*x+5, 4*x+8*y-3)]
, nidToCxy = \nid -> case nid of
    NodeL x y -> (12*x-4, 4*x+8*y-4)
    NodeR x y -> (12*x+4, 4*x+8*y-4)
}

s4MapperB :: HiveGridMapper
s4MapperB = s4MapperA {
  cxyToHid = cxy2hid 4 $ BS.pack
    [0,0,0,0,0,0,0,9,9,6,6,6
    ,0,0,0,0,0,0,4,4,4,6,6,6
    ,0,0,0,0,0,4,4,4,1,1,1,1
    ,3,3,3,3,4,4,4,1,1,1,1,1
    ,3,3,3,3,8,8,1,1,1,1,1,1
    ,3,3,3,3,5,5,5,1,1,1,1,1
    ,2,2,2,2,2,5,5,5,1,1,1,1
    ,2,2,2,2,2,2,5,5,5,7,7,7]
}

testCxyToHid :: IO ()
testCxyToHid = do
  let mappers = [s1MapperA, s2MapperA, s3MapperA, s4MapperA, s2MapperB, s3MapperB, s4MapperB]
  let hid2char hid = case hid of HCell _ -> '-' ; HLine _ -> '*'; HNode _ -> '#'
  let grid m w h = unlines [ [ hid2char (m (x, y)) | x <- [0..w-1] ] | y <- [0..h-1] ]
  let grids w h = map (\m -> grid (cxyToHid m) w h) mappers
  mapM_ putStrLn (grids 40 20)

