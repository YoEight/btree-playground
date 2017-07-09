module Data.BTree where

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Mutable
-- import Text.PrettyPrint

type Keys k = Vector.Vector k
type Children k v = Vector.Vector (BTree k v)
type Values k v = Vector.Vector (k, v)

data BTree k v
  = Node !(Keys k) (Children k v)
  | Leaf (Values k v)
  deriving Show

btreeOrder :: Num a => a
btreeOrder = 3

emptyBTree :: BTree k v
emptyBTree = Leaf $ Vector.empty

insert :: Ord k => k -> v -> BTree k v -> BTree k v
insert k v node =
  case node of
    Node keys children ->
      let minN = Vector.head keys
          maxN = Vector.last keys in
      if k < minN
      then
        let newChildren =
              Vector.modify (\m -> Mutable.modify m (insert k v) 0) children in
        Node keys newChildren
      else if k > maxN
        then
          let lastIdx = Vector.length children - 1
              newChildren =
                Vector.modify (\m -> Mutable.modify m (insert k v) lastIdx)
                  children in
          Node keys newChildren
        else undefined
    Leaf vs
      | Vector.length vs < btreeOrder - 1 -> Leaf (Vector.snoc vs (k, v))
      | otherwise ->
        let split = truncate $ ((btreeOrder :: Float) - 1) / 2
            (sk, sv) = Vector.unsafeIndex vs split
            lower  = Leaf $ Vector.take split vs
            middle = Leaf $ Vector.fromList [(sk, sv)]
            rest =
              if Vector.length vs == split + 1
              then Vector.empty
              else Vector.slice (split + 1) (Vector.length vs - split + 1) vs
            higher = Leaf $ Vector.snoc rest (k, v)
            keys   = Vector.singleton sk
            parent = Node keys $ Vector.fromList [ lower, middle, higher ] in
        parent

-- ppBTree :: (Show k, Show v) => BTree k v -> Doc
-- ppBTree tree = ppBTreeRoot tree

-- ppBTreeRoot :: (Show k, Show v) => BTree k v -> Doc
-- ppBTreeRoot BTreeEmpty =
--   text "*Empty*"
-- ppBTreeRoot (BTreeNode l r) =
--   text "*Root*" <> ppBTreeChild l <> foldMap ppBTreeChild r
-- ppBTreeRoot (BTreeLeaf l) =
--   text "*Root*" <> ppBTreeLeaf l

-- ppBTreeChild :: (Show k, Show v) => BTree k v -> Doc
-- ppBTreeChild = undefined

-- ppBTreeLeaf :: (Show k, Show v) => Leaf k v -> Doc
-- ppBTreeLeaf (Leaf k v) =
--   lbrack <> text (show k)
--          <> colon
--          <> colon
--          <> text (show v)
--          <> rbrack