{- eoleahy Eoin Leahy-}
module Ex02 where

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype
data Tree k d
  = Br (Tree k d) (Tree k d) k d
  | Leaf k d
  | Nil
  deriving (Eq, Show)

type IntFun = Tree Int Int -- binary tree with integer keys and data

data Expr
  = Val Double
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Abs Expr
  | Sign Expr
   deriving (Eq, Show)



-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> Tree k d -> Tree k d
--ins _ _ _ = error "ins NYI"
ins k d Nil = Leaf k d

ins key value (Leaf k d)
    | k < key = Br (Nil) (Leaf key value) k d
    | k > key = Br (Leaf key value) (Nil) k d
    | key == k = Leaf key value

ins key value (Br l r k d)
    | key < k = Br (ins key value l) r k d
    | key > k = Br l (ins key value r) k d
    | key == k = Br l r key value


-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => Tree k d -> k -> m d

lkp Nil _ = fail("Not found")

lkp (Leaf k d) key
    | key == k = return d
    | otherwise = fail("Not found")

lkp (Br l r k d) key
    | key < k = lkp l key
    | key > k = lkp r key
    | key == k = return d


-- Part 3 : Instance of Num for Expr

{-
  Fix the following instance for Num of Expr so all tests pass

  Note that the tests expect simplification to be done
  only when *all* Expr arguments are of the form Val v

  Hint 1 :  implementing fromInteger *first* is recommended!
  Hint 2 :  remember that Double is already an instance of Num
-}



instance Num Expr where

  e1 + e2 = Add e1 e2
  e1 - e2 =  Sub e1 e2
  e1 * e2 =  Mul e1 e2
  negate e =  Sub 0 e
  abs e =  Abs e
  signum e =  Sign e
  fromInteger i =  Val (fromIntegral i)
