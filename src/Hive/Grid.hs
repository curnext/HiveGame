module Hive.Grid where

import Hive.Hid

import Data.Word(Word8)
import qualified Data.ByteString as BS

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
s1TableA :: BS.ByteString
s1TableA = BS.pack
  [0,0,9
  ,3,8,1]

s1MapperA :: (Int, Int) -> HID
s1MapperA = cxy2hid 1 s1TableA

s2TableA :: BS.ByteString
s2TableA = BS.pack
  [0,0,0,0,9,6
  ,0,0,0,4,1,1
  ,3,3,8,1,1,1
  ,2,2,2,5,1,1]

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
s2MapperA :: (Int, Int) -> HID
s2MapperA = cxy2hid 2 s2TableA

s2TableB :: BS.ByteString
s2TableB = BS.pack
  [0,0,0,9,9,6
  ,3,3,4,4,4,6
  ,3,3,8,8,1,1
  ,3,3,5,5,5,7]

s2MapperB :: (Int, Int) -> HID
s2MapperB = cxy2hid 2 s2TableB

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
s3TableA :: BS.ByteString
s3TableA = BS.pack
  [0,0,0,0,0,0,9,6,6
  ,0,0,0,0,0,4,1,1,1
  ,0,0,0,0,4,1,1,1,1
  ,3,3,3,8,1,1,1,1,1
  ,2,2,2,2,5,1,1,1,1
  ,2,2,2,2,2,5,1,1,1]

s3MapperA :: (Int, Int) -> HID
s3MapperA = cxy2hid 3 s3TableA

s3TableB :: BS.ByteString
s3TableB = BS.pack
  [0,0,0,0,0,9,9,6,6
  ,0,0,0,0,4,4,4,6,6
  ,3,3,3,4,4,4,1,1,1
  ,3,3,3,8,8,1,1,1,1
  ,3,3,3,5,5,5,1,1,1
  ,2,2,2,2,5,5,5,7,7]

s3MapperB :: (Int, Int) -> HID
s3MapperB = cxy2hid 3 s3TableB

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
s4TableA :: BS.ByteString
s4TableA = BS.pack
  [0,0,0,0,0,0,0,0,9,6,6,6
  ,0,0,0,0,0,0,0,4,1,1,1,1
  ,0,0,0,0,0,0,4,1,1,1,1,1
  ,0,0,0,0,0,4,1,1,1,1,1,1
  ,3,3,3,3,8,1,1,1,1,1,1,1
  ,2,2,2,2,2,5,1,1,1,1,1,1
  ,2,2,2,2,2,2,5,1,1,1,1,1
  ,2,2,2,2,2,2,2,5,1,1,1,1]

s4MapperA :: (Int, Int) -> HID
s4MapperA = cxy2hid 4 s4TableA

s4TableB :: BS.ByteString
s4TableB = BS.pack
  [0,0,0,0,0,0,0,9,9,6,6,6
  ,0,0,0,0,0,0,4,4,4,6,6,6
  ,0,0,0,0,0,4,4,4,1,1,1,1
  ,3,3,3,3,4,4,4,1,1,1,1,1
  ,3,3,3,3,8,8,1,1,1,1,1,1
  ,3,3,3,3,5,5,5,1,1,1,1,1
  ,2,2,2,2,2,5,5,5,1,1,1,1
  ,2,2,2,2,2,2,5,5,5,7,7,7]

s4MapperB :: (Int, Int) -> HID
s4MapperB = cxy2hid 4 s4TableB

testMapper :: IO ()
testMapper = do
  let mappers = [s1MapperA, s2MapperA, s3MapperA, s4MapperA, s2MapperB, s3MapperB, s4MapperB]
  let hid2char hid = case hid of HCell _ -> '-' ; HLine _ -> '*'; HNode _ -> '#'
  let grid m w h = unlines [ [ hid2char (m (x, y)) | x <- [0..w-1] ] | y <- [0..h-1] ]
  let grids w h = map (\m -> grid m w h) mappers
  mapM_ putStrLn (grids 40 20)

