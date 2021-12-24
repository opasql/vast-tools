{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Text.XML.Vast.Internal.Term where

import           Data.List.NonEmpty
import           Data.Proxy
import           Data.Tree
import           GHC.TypeLits
import           Text.XML

type KindOf (a :: k) = k

type family Demote k where
  Demote Symbol = String
  Demote Nat = Integer
  Demote Bool = Bool
  Demote [k] = [Demote k]
  Demote (Maybe k) = Maybe (Demote k)
  Demote (Either k1 k2) = Either (Demote k1) (Demote k2)
  Demote (Tree k) = Tree (Demote k)
  Demote (NonEmpty k) = NonEmpty (Demote k)
  Demote Document = Document
  Demote Element = Element

class Term a where
  term :: Demote (KindOf a)

instance (KnownSymbol s) => Term (s :: Symbol) where
  term = symbolVal (Proxy @s)

instance (KnownNat n) => Term (n :: Nat) where
  term = natVal (Proxy @n)

instance Term 'True where
  term = True

instance Term 'False where
  term = False

instance Term ('[] :: [k]) where
  term = []

instance (Term a, Term as) => Term ((a ': as) :: [k]) where
  term = term @a : term @as

instance Term ('Nothing :: Maybe k) where
  term = Nothing

instance (Term a) => Term (('Just a) :: Maybe k) where
  term = Just (term @a)

instance (Term a) => Term (('Left a) :: Either k1 k2) where
  term = Left (term @a)

instance (Term a) => Term (('Right a) :: Either k1 k2) where
  term = Right (term @a)

instance (Term a, Term as) => Term ('Node a as) where
  term = Node (term @a) (term @as)

instance (Term a, Term as) => Term (a ':| as) where
  term = term @a :| term @as
