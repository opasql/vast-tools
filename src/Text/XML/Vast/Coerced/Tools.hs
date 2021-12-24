{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Text.XML.Vast.Coerced.Tools
  ( module M
  , dModify
  , dCreate
  , dAdd
  , dAddOrd
  , dTakeAll
  , dTakeFirst
  , dTakeAllIf
  , dTakeFirstIf
  , dTakeUrls
  , dTakeUrl
  , dTakeUrlsIf
  , dTakeUrlIf
  , dDrop
  , dDropIf
  , dMerge
  , dMergeFold
  , eModify
  , eCreate
  , eAdd
  , eAddOrd
  , eTakeAll
  , eTakeFirst
  , eTakeAllIf
  , eTakeFirstIf
  , eTakeUrls
  , eTakeUrl
  , eTakeUrlsIf
  , eTakeUrlIf
  , eDrop
  , eDropIf
  , eMerge
  , eMergeFold
  ) where

import           Control.Exception
import           Data.Coerce
import           Data.List.NonEmpty           (NonEmpty (..))
import           GHC.TypeLits
import           Text.URI
import           Text.XML

import           Data.Kind
import           Text.XML.Vast.Internal.Path  as M
import           Text.XML.Vast.Internal.Term  as M
import qualified Text.XML.Vast.Internal.Tools as Internal

-- ** Document tools

dModify
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => (e -> e)
  -> d
  -> d
dModify (coerce -> f) (coerce -> d) = coerce (Internal.dModify @path f d)

dCreate
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => [e]
  -> d
  -> d
dCreate (coerce -> es) (coerce -> d) = coerce (Internal.dCreate @path es d)

dAdd
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => [e]
  -> d
  -> d
dAdd (coerce -> es) (coerce -> d) = coerce (Internal.dAdd @path es d)

dAddOrd
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => [e]
  -> d
  -> d
dAddOrd (coerce -> es) (coerce -> d) = coerce (Internal.dAddOrd @path es d)

dTakeAll
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => d
  -> [e]
dTakeAll (coerce -> d) = coerce (Internal.dTakeAll @path d)

dTakeFirst
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => d
  -> Maybe e
dTakeFirst (coerce -> d) = coerce (Internal.dTakeFirst @path d)

dTakeAllIf
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => (e -> Bool)
  -> d
  -> [e]
dTakeAllIf (coerce -> p) (coerce -> d) = coerce (Internal.dTakeAllIf @path p d)

dTakeFirstIf
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => (e -> Bool)
  -> d
  -> Maybe e
dTakeFirstIf (coerce -> p) (coerce -> d) = coerce (Internal.dTakeFirstIf @path p d)

dTakeUrls
  :: forall (path :: NonEmpty Symbol) (d :: Type)
   . (Term path, Coercible d Document)
  => d
  -> [Either SomeException URI]
dTakeUrls (coerce -> d) = coerce (Internal.dTakeUrls @path d)

dTakeUrl
  :: forall (path :: NonEmpty Symbol) (d :: Type)
   . (Term path, Coercible d Document)
  => d
  -> Maybe (Either SomeException URI)
dTakeUrl (coerce -> d) = coerce (Internal.dTakeUrl @path d)

dTakeUrlsIf
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => (e -> Bool)
  -> d
  -> [Either SomeException URI]
dTakeUrlsIf (coerce -> p) (coerce -> d) = coerce (Internal.dTakeUrlsIf @path p d)

dTakeUrlIf
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => (e -> Bool)
  -> d
  -> Maybe (Either SomeException URI)
dTakeUrlIf (coerce -> p) (coerce -> d) = coerce (Internal.dTakeUrlIf @path p d)

dDrop
  :: forall (path :: NonEmpty Symbol) (d :: Type)
   . (Term path, Term (NonEmptyInit path), Coercible d Document)
  => d
  -> d
dDrop (coerce -> d) = coerce (Internal.dDrop @path d)

dDropIf
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => (e -> Bool)
  -> d
  -> d
dDropIf (coerce -> p) (coerce -> d) = coerce (Internal.dDropIf @path p d)

dMerge
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => e
  -> d
  -> d
dMerge (coerce -> e) (coerce -> d) = coerce (Internal.dMerge @path e d)

dMergeFold
  :: forall (path :: NonEmpty Symbol) (e :: Type) (d :: Type)
   . (Term path, Coercible e Element, Coercible d Document)
  => [e]
  -> d
  -> d
dMergeFold (coerce -> es) (coerce -> d) = coerce (Internal.dMergeFold @path es d)

-- ** Element tools

eModify
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type) (c :: Type)
   . (Term path, Coercible a Element, Coercible b Element, Coercible c Element)
  => (a -> b)
  -> c
  -> c
eModify (coerce -> f) (coerce -> e) = coerce (Internal.eModify @path f e)

eCreate
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => [a]
  -> b
  -> b
eCreate (coerce -> es) (coerce -> e) = coerce (Internal.eCreate @path es e)

eAdd
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => [a]
  -> b
  -> b
eAdd (coerce -> es) (coerce -> e) = coerce (Internal.eAdd @path es e)

eAddOrd
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => [a]
  -> b
  -> b
eAddOrd (coerce -> es) (coerce -> e) = coerce (Internal.eAddOrd @path es e)

eTakeAll
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => a
  -> [b]
eTakeAll (coerce -> e) = coerce (Internal.eTakeAll @path e)

eTakeFirst
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => a
  -> Maybe b
eTakeFirst (coerce -> e) = coerce (Internal.eTakeFirst @path e)

eTakeAllIf
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => (a -> Bool)
  -> b
  -> [a]
eTakeAllIf (coerce -> p) (coerce -> e) = coerce (Internal.eTakeAllIf @path p e)

eTakeFirstIf
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => (Element -> Bool)
  -> Element
  -> Maybe Element
eTakeFirstIf = Internal.takeFirstIf @path id

eTakeUrls
  :: forall (path :: NonEmpty Symbol) (e :: Type)
   . (Term path, Coercible e Element)
  => e
  -> [Either SomeException URI]
eTakeUrls (coerce -> e) = coerce (Internal.eTakeUrls @path e)

eTakeUrl
  :: forall (path :: NonEmpty Symbol) (e :: Type)
   . (Term path, Coercible e Element)
  => e
  -> Maybe (Either SomeException URI)
eTakeUrl (coerce -> e) = coerce (Internal.eTakeUrl @path e)

eTakeUrlsIf
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => (a -> Bool)
  -> b
  -> [Either SomeException URI]
eTakeUrlsIf (coerce -> p) (coerce -> e) = coerce (Internal.eTakeUrlsIf @path p e)

eTakeUrlIf
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => (a -> Bool)
  -> b
  -> Maybe (Either SomeException URI)
eTakeUrlIf (coerce -> p) (coerce -> e) = coerce (Internal.eTakeUrlIf @path p e)

eDrop
  :: forall (path :: NonEmpty Symbol) (e :: Type)
   . (Term path, Term (NonEmptyInit path), Coercible e Element)
  => e
  -> e
eDrop (coerce -> e) = coerce (Internal.eDrop @path e)

eDropIf
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => (a -> Bool)
  -> b
  -> b
eDropIf (coerce -> p) (coerce -> e) = coerce (Internal.eDropIf @path p e)

eMerge
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => a
  -> b
  -> b
eMerge (coerce -> container) (coerce -> receiver) = coerce (Internal.eMerge @path container receiver)

eMergeFold
  :: forall (path :: NonEmpty Symbol) (a :: Type) (b :: Type)
   . (Term path, Coercible a Element, Coercible b Element)
  => [a]
  -> b
  -> b
eMergeFold (coerce -> containers) (coerce -> receiver) = coerce (Internal.eMergeFold @path containers receiver)
