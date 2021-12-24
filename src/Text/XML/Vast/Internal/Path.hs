{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.XML.Vast.Internal.Path
  ( HasPath
  , Path
  , Search
  , PathTo
  , PathClue
  , PathClues
  , AllPaths
  , ToNonEmpty
  , NonEmptyInit
  , KnownElement
  ) where

import           Data.List.NonEmpty
import           Data.Tree
import           Data.Type.Bool
import           Data.Type.Equality
import           Fcf                         hiding (Any, type (&&), type (||))
import           Fcf.Class.Foldable
import           Fcf.Data.List
import           Fcf.Data.Symbol
import           GHC.TypeLits

import           Text.XML.Vast.Internal.Tree

-- ** Search for a path to an element

type Path (p :: [Symbol]) = Eval (PathImpl p VastTree)

data PathImpl :: [a] -> Tree a -> Exp (NonEmpty a)
type instance Eval (PathImpl p t) =
  If (HasPath p t)
     (ToNonEmpty p)
     ( If (HasSubPath p t)
          (ToNonEmpty p)
          (TypeError (UnknownPathOrSubPathError p))
     )

--------------------------------------------------------
--------------------------------------------------------

type HasPath (p :: [a]) (t :: Tree a) = Eval (HasPathImpl p t)

-- | 'HasPath' implementation
data HasPathImpl :: [a] -> Tree a -> Exp Bool
type instance Eval (HasPathImpl '[] _) =
  TypeError ('Text "Empty paths are not allowed")
type instance Eval (HasPathImpl (a ': as) (t :> ts)) =
  If (a == t)
    (Eval (HasPathImplHelper as ts))
    'False

data HasPathImplHelper :: [a] -> [Tree a] -> Exp Bool
type instance Eval (HasPathImplHelper '[] _) = 'True
type instance Eval (HasPathImplHelper (a ': as) trees) =
  Eval (UnMaybe (Pure 'False) (HasPathImplEliminator as) =<< Find (HasPathImplPredicate a) trees)

data HasPathImplEliminator :: [a] -> Tree a -> Exp Bool
type instance Eval (HasPathImplEliminator as (_ :> ts)) =
  Eval (HasPathImplHelper as ts)

data HasPathImplPredicate :: pathChunk -> node -> Exp Bool
type instance Eval (HasPathImplPredicate pathChunk (t :> _)) = pathChunk == t

--------------------------------------------------------
--------------------------------------------------------

-- | Find relative path in a tree
type HasSubPath (p :: [a]) (t :: Tree a) = Eval (HasSubPathImpl p t)

-- 'SubPath' implementation
data HasSubPathImpl :: [a] -> Tree a -> Exp Bool
type instance Eval (HasSubPathImpl '[] _) = 'False
type instance Eval (HasSubPathImpl (p ': '[]) (a :> as)) =
  p == a || Eval (Any (HasSubPathImpl '[p]) as)
type instance Eval (HasSubPathImpl (p1 ': (p2 ': ps)) (a :> as)) =
  If (p1 == a && as == '[])
    'False
    ( If (p1 == a)
        (Eval (Or =<< Map (HasPathImpl (p2 ': ps)) as))
        (Eval (Any (HasSubPathImpl (p1 ': (p2 ': ps))) as))
    )

--------------------------------------------------------
--------------------------------------------------------

-- | Find all paths to an element in 'VastTree'
type Search (s :: Symbol) = Eval (SearchWithError s)

data SearchWithError :: Symbol -> Exp (NonEmpty [Symbol])
type instance Eval (SearchWithError a) = Eval
  ( SearchWithErrorHelper a (Eval (SearchImpl a VastTree))
  )

-- | Raises Type-Level error if the list is empty
data SearchWithErrorHelper :: a -> [[a]] -> Exp (NonEmpty [a])
type instance Eval (SearchWithErrorHelper name list) = ToNonEmpty
  ( If (list == '[])
      (TypeError (UnknownElementError name))
      list
  )

-- | 'Search' implementation
data SearchImpl :: a -> Tree a -> Exp [[a]]
type instance Eval (SearchImpl s t) =
  Eval (Map (Cons (Root VastTree)) =<< SearchImplWithoutRoot s t)

data SearchImplWithoutRoot :: a -> Tree a -> Exp [[a]]
type instance Eval (SearchImplWithoutRoot val (dat :> subs)) =
  If (Eval (TyEq val dat))
    '[ '[] ]
    (Eval (ConcatMap (SearchImplWithoutRootHelper val) subs))

data SearchImplWithoutRootHelper :: a -> Tree a -> Exp [[a]]
type instance Eval (SearchImplWithoutRootHelper a (t :> ts)) =
  Eval (Map (Cons t) (Eval (SearchImplWithoutRoot a (t :> ts))))

--------------------------------------------------------
--------------------------------------------------------

-- | Find path to an element, fails if there is several paths
-- to the given target.
type PathTo (target :: Symbol) = PathClues '[] target

-- | Handy synonim of 'PathClues' for 1 clue cases.
-- to the given target.
type PathClue (clue :: Symbol) (target :: Symbol) = PathClues '[clue] target

-- | Find path to an element, use clues to disambiguate
-- paths.
type PathClues (clues :: [Symbol]) (target :: Symbol) =
  Eval (PathCluesWithError clues target (Eval (PathCluesImpl clues target VastTree)))

data PathCluesWithError
  :: [a]    -- Clues
  -> a      -- Target
  -> [[a]]  -- Resulting paths after applying the clues
  -> Exp (NonEmpty a)
type instance Eval (PathCluesWithError _     target '[]             ) = TypeError (UnknownElementError target)
type instance Eval (PathCluesWithError _     _      (path ': '[]   )) = ToNonEmpty path
type instance Eval (PathCluesWithError clues target (p1 ': p2 ': ps)) = TypeError (NotEnoughCluesError clues target (p1 ': p2 ': ps))

data PathCluesImpl :: [a] -> a -> Tree a -> Exp [[a]]
type instance Eval (PathCluesImpl clues target tree) =
  Eval (Filter (IsSubsequenceOf clues) =<< SearchImpl target tree)

--------------------------------------------------------
--------------------------------------------------------

-- | List all paths in a Tree
type AllPaths t = Eval (AllPathsImpl t)

-- | 'AllPaths' implementation
data AllPathsImpl :: Tree a -> Exp [[a]]
type instance Eval (AllPathsImpl t) = Eval (Map Reverse =<< AllPathsImplHelper '[] t)

data AllPathsImplHelper :: [a] -> Tree a -> Exp [[a]]
type instance Eval (AllPathsImplHelper p (s :> '[]))       = '[s ': p]
type instance Eval (AllPathsImplHelper p (s :> (t ': ts))) = Eval (ConcatMap (AllPathsImplHelper (s ': p)) (t ': ts))

--------------------------------------------------------
--------------------------------------------------------

-- | Check if 'Tree' contains the given element
type KnownElement (e :: Symbol) = Eval (KnownElementImpl e VastTree)

data KnownElementImpl :: a -> Tree a -> Exp Bool
type instance Eval (KnownElementImpl e t) = Eval (Elem e (Flatten t))

--------------------------------------------------------
--------------------------------------------------------

-- ** Error messages

type UnknownPathOrSubPathError (path :: [a]) =
        'Text "There is no absolute or relative path "
  ':$$: 'ShowType path
  ':$$: 'Text " in VastTree."
  ':$$: 'Text "Use 'Data.Tree.drawTree vastTree' to see VAST syntax tree"

type UnknownElementError e =
        'Text "There is no element "
  ':<>: 'Text e
  ':<>: 'Text " in VastTree specification"
  ':$$: 'Text "Use 'Data.Tree.drawTree vastTree' to see VAST syntax tree"

type NotEnoughCluesError (clues :: [a]) (target :: a) (paths :: [[a]]) =
        'Text "There are several possible paths to an element "
  ':<>: 'ShowType target ':<>: 'Text ": "
  ':$$: 'ShowType paths
  ':$$: 'Text "Couldn't disambiguate them with these clues: "
  ':$$: 'ShowType clues
  ':$$: 'Text "Consider using more clues or specify absolute path with HasPath"

-- ** Utils

-- | Takes two lists and returns 'True' if all
-- the elements of the first list occur, in order, in the second. The
-- elements do not have to occur consecutively.
data IsSubsequenceOf :: [a] -> [a] -> Exp Bool
type instance Eval (IsSubsequenceOf '[]      (_ ': _)) = 'True
type instance Eval (IsSubsequenceOf (_ ': _) '[]     ) = 'False
type instance Eval (IsSubsequenceOf '[]      '[]     ) = 'True
type instance Eval (IsSubsequenceOf (x ': a) (y ': b)) =
  If (x == y)
    (Eval (IsSubsequenceOf a b))
    (Eval (IsSubsequenceOf (x ': a) b))

type family ToNonEmpty (list :: [a]) :: NonEmpty a where
  ToNonEmpty (a ': as) = a ':| as

type family NonEmptyInit (ne :: NonEmpty a) :: NonEmpty a where
  NonEmptyInit (a ':| as) = a ':| Eval (FromMaybe '[] =<< Init as)
