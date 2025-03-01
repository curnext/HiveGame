module Hive.Hid where

-- Define data type for Cells, Lines, Nodes, and their neighbor-lookup functions
-- (x,y) :: cell coordinates
-- T B L R H :: relative position, H for horizontal

data CID = Cell Int Int
  deriving (Eq, Ord, Show) 
--            + + + +
--           +       +
--    + + + +  (0,1)  + + + +
--   +       \   T   /       +
--  + (-1, 1) +-----+  (1,0)  +
--   +   TL  / TL TR \  TR   +
--    +-----+ L(0,0)R +-----+
--   +   BL  \ BL BR /  BR   +
--  + (-1, 0) +-----+ (1, -1) +
--   +       /   B   \       +
--    + + + + (0, -1) + + + +
--           +       +
--            + + + +

data LID
  = LineT Int Int
  | LineL Int Int
  | LineR Int Int
  deriving (Eq, Ord, Show) 
--            + + + +
--           +       +
--    + + + +         + + + +
--   +       \   T   /       +
--  +      L  +LineT+  R      +
--   +     LineL B LineR     +
--    +-----+  (x,y)  +-----+
--   +       \       /       +
--  +         +-----+         +
--   +       /       \       +
--    + + + +         + + + +
--           +       +
--            + + + +

data NID
  = NodeL Int Int
  | NodeR Int Int
  deriving (Eq, Ord, Show) 
--            + + + +
--           +       +
--    + + + +         + + + +
--   +       \   T   /       +
--  +     H NodeL-NodeR H     +
--   +       /   B   \       +
--    +-----+  (x,y)  +-----+
--   +       \       /       +
--  +         +-----+         +
--   +       /       \       +
--    + + + +         + + + +
--           +       +
--            + + + +

data HID = HCell CID | HLine LID | HNode NID
  deriving (Eq, Ord, Show) 

-- Cell-to-Cell transformations
c2cTR, c2cT, c2cTL, c2cBL, c2cB, c2cBR :: CID -> CID
c2cTR (Cell x y) = Cell (x + 1) y
c2cT  (Cell x y) = Cell x (y + 1)
c2cTL (Cell x y) = Cell (x - 1) (y + 1)
c2cBL (Cell x y) = Cell (x - 1) y
c2cB  (Cell x y) = Cell x (y - 1)
c2cBR (Cell x y) = Cell (x + 1) (y - 1)

c2c :: [CID -> CID]
c2c = [c2cTR, c2cT, c2cTL, c2cBL, c2cB, c2cBR]

-- Cell-to-Line transformations
c2lTR, c2lT, c2lTL, c2lBL, c2lB, c2lBR :: CID -> LID
c2lTR (Cell x y) = LineR x y
c2lT  (Cell x y) = LineT x y
c2lTL (Cell x y) = LineL x y
c2lBL (Cell x y) = LineR (x - 1) y
c2lB  (Cell x y) = LineT x (y - 1)
c2lBR (Cell x y) = LineL (x + 1) (y - 1)

c2l :: [CID -> LID]
c2l = [c2lTR, c2lT, c2lTL, c2lBL, c2lB, c2lBR]

-- Cell-to-Node transformations
c2nL, c2nTL, c2nTR, c2nR, c2nBL, c2nBR :: CID -> NID
c2nR  (Cell x y) = NodeL (x + 1) (y - 1)
c2nTR (Cell x y) = NodeR x y
c2nTL (Cell x y) = NodeL x y
c2nL  (Cell x y) = NodeR (x - 1) y
c2nBL (Cell x y) = NodeL x (y - 1)
c2nBR (Cell x y) = NodeR x (y - 1)

c2n :: [CID -> NID]
c2n = [c2nL, c2nTL, c2nTR, c2nR, c2nBL, c2nBR]

-- Line-to-Cell transformations
l2cT, l2cB, l2cR, l2cL :: LID -> CID
l2cT (LineR x y) = Cell (x + 1) y
l2cT (LineT x y) = Cell x (y + 1)
l2cT (LineL x y) = Cell (x - 1) (y + 1)

l2cB (LineR x y) = Cell x y
l2cB (LineT x y) = Cell x y
l2cB (LineL x y) = Cell x y

l2cR (LineR x y) = Cell (x + 1) (y - 1)
l2cR (LineT x y) = Cell (x + 1) y
l2cR (LineL x y) = Cell x (y + 1)

l2cL (LineR x y) = Cell x (y + 1)
l2cL (LineT x y) = Cell (x - 1) (y + 1)
l2cL (LineL x y) = Cell (x - 1) y

-- Line-to-Line transformations
l2lTR, l2lTL, l2lBL, l2lBR :: LID -> LID
l2lTR (LineR x y) = LineT (x + 1) (y - 1)
l2lTR (LineT x y) = LineL (x + 1) y
l2lTR (LineL x y) = LineR (x - 1) (y + 1)

l2lTL (LineR x y) = LineL (x + 1) y
l2lTL (LineT x y) = LineR x (y + 1)
l2lTL (LineL x y) = LineT (x - 1) y

l2lBL (LineR x y) = LineT x y
l2lBL (LineT x y) = LineL x y
l2lBL (LineL x y) = LineR (x - 1) y

l2lBR (LineR x y) = LineL (x + 1) (y - 1)
l2lBR (LineT x y) = LineR x y
l2lBR (LineL x y) = LineT x y

-- Line-to-Node transformations
l2nR, l2nL :: LID -> NID
l2nR (LineR x y) = NodeL (x + 1) (y - 1)
l2nR (LineT x y) = NodeR x y
l2nR (LineL x y) = NodeL x y

l2nL (LineR x y) = NodeR x y
l2nL (LineT x y) = NodeL x y
l2nL (LineL x y) = NodeR (x - 1) y

-- Node-to-Cell transformations
n2cH, n2cT, n2cB :: NID -> CID
n2cH (NodeR x y) = Cell (x + 1) y
n2cH (NodeL x y) = Cell (x - 1) (y + 1)
n2cT (NodeR x y) = Cell x (y + 1)
n2cT (NodeL x y) = Cell x (y + 1)
n2cB (NodeR x y) = Cell x y
n2cB (NodeL x y) = Cell x y

-- Node-to-Line transformations
n2lH, n2lT, n2lB :: NID -> LID
n2lH (NodeR x y) = LineT x y
n2lH (NodeL x y) = LineT x y
n2lT (NodeR x y) = LineL (x + 1) y
n2lT (NodeL x y) = LineR (x - 1) (y + 1)
n2lB (NodeR x y) = LineR x y
n2lB (NodeL x y) = LineL x y

-- Node-to-Node transformations
n2nH, n2nT, n2nB :: NID -> NID
n2nH (NodeR x y) = NodeL x y
n2nH (NodeL x y) = NodeR x y
n2nT (NodeR x y) = NodeL (x + 1) y
n2nT (NodeL x y) = NodeR (x - 1) (y + 1)
n2nB (NodeR x y) = NodeL (x + 1) (y - 1)
n2nB (NodeL x y) = NodeR (x - 1) y
