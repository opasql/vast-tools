{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Text.XML.Vast.Internal.Tools where

import           Control.Lens                hiding ((:>))
import           Data.Coerce
import           Data.List
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Map.Strict             (Map)
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text, pack)
import           Data.Tree
import           GHC.TypeLits
import           Text.URI
import           Text.XML.Lens

import           Data.Function
import           Data.Kind

import           Control.Exception
import           Data.CaseInsensitive
import           Data.Maybe
import           Text.XML.Vast.Internal.Path
import           Text.XML.Vast.Internal.Term
import           Text.XML.Vast.Internal.Tree

-- | Term-Level 'VastTree'
vastTree :: Tree String
vastTree = term @VastTree

-- ** Generic tools

modify
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> (Element -> Element)
  -> s
  -> s
modify l = over (l . foldPath' (term @path))

create
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> [Element]
  -> s
  -> s
create l es d
  | view (l . name) d /= rootName = d & l .~ reconstruct (rootName:path)
  | otherwise                     = d & l .~ findHole path (view l d)
  where
    rootName : path = fmap fromString $ NonEmpty.toList $ term @path

    findHole []     e = e & nodes %~ (<> fmap (_Element #) es)
    findHole (p:ps) e = case e ^. nodes of
      [] -> e & nodes .~ [_Element # reconstruct (p:ps)]
      ns -> if any (nameIs p) ns
        then e & nodes %~ fmap aim
        else e & nodes %~ insertList compare [_Element # reconstruct (p:ps)]
      where
        aim :: Node -> Node
        aim (NodeElement e')
          | e' ^. name == p = NodeElement $ findHole ps e'
          | otherwise       = NodeElement e'
        aim someNode = someNode

    nameIs p = (Just p ==) . preview (_Element . name)

    -- First parameter can't be empty
    reconstruct names
      | length names == 1 = makeElement (head names) mempty ((_Element #) <$> es)
      | otherwise         = makeElement (head names) mempty [_Element # reconstruct (tail names)]

add
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> [Element]
  -> s
  -> s
add l es = modify @path l (over nodes (<> fmap (_Element #) es))

addOrd
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> [Element]
  -> s
  -> s
addOrd l (fmap (_Element #) -> ns) = modify @path l (over nodes (insertList compareByName ns))

takeAll
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> s
  -> [Element]
takeAll l = toListOf (l . foldPath' (term @path))

takeFirst
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> s
  -> Maybe Element
takeFirst l = listToMaybe . takeAll @path l

takeAllIf
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> (Element -> Bool)
  -> s
  -> [Element]
takeAllIf l p = toListOf (l . foldPath' (term @path) . filtered p)

takeFirstIf
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> (Element -> Bool)
  -> s
  -> Maybe Element
takeFirstIf l p = listToMaybe . takeAllIf @path l p

takeUrls
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> s
  -> [Either SomeException URI]
takeUrls l = fmap mkURI . toListOf (l . foldPath' (term @path) . text)

takeUrl
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> s
  -> Maybe (Either SomeException URI)
takeUrl l = listToMaybe . takeUrls @path l

takeUrlsIf
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> (Element -> Bool)
  -> s
  -> [Either SomeException URI]
takeUrlsIf l p = fmap mkURI . toListOf (l . foldPath' (term @path) . filtered p . text)

takeUrlIf
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> (Element -> Bool)
  -> s
  -> Maybe (Either SomeException URI)
takeUrlIf l p = listToMaybe . takeUrlsIf @path l p

drop'
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . (Term path, Term (NonEmptyInit path))
  => Lens' s Element
  -> s
  -> s
drop' l = dropIf @(NonEmptyInit path) l p
  where
    elemName = fromString . NonEmpty.last $ term @path
    p e = elemName == view name e

dropIf
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> (Element -> Bool)
  -> s
  -> s
dropIf l p = modify @path l (over nodes (filter deleteNodes))
  where
    deleteNodes (NodeElement e) = not $ p e
    deleteNodes _               = True

merge
  :: forall (path :: NonEmpty Symbol) (s :: Type)
   . Term path
  => Lens' s Element
  -> Element
  -> s
  -> s
merge l e = modify @path l (over nodes (<> view nodes e))

mergeFold
  :: forall (path :: NonEmpty Symbol) (s :: Type) (f :: Type -> Type)
   . (Term path, Foldable f)
  => Lens' s Element
  -> f Element
  -> s
  -> s
mergeFold l = foldFs (merge @path l)

-- ** Document tools

-- | Find all 'Element's in the
-- given @path@ an apply function to them.
--
-- > vast :: Lazy.Text
-- > vast = "
-- > <VAST>
-- >   <Ad>
-- >     <InLine>
-- >       <AdSystem>foo></AdSystem>
-- >     </InLine>
-- >   </Ad>
-- >   <Ad>
-- >     <InLine>
-- >       <AdSystem>bar></AdSystem>
-- >     </InLine>
-- >   </Ad>
-- > </VAST>
-- > "
-- > dMofidy (term)
dModify
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Element)
  -> Document
  -> Document
dModify = modify @path root

-- |
dCreate
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => [Element]
  -> Document
  -> Document
dCreate = create @path root

dAdd
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => [Element]
  -> Document
  -> Document
dAdd = add @path root

dAddOrd
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => [Element]
  -> Document
  -> Document
dAddOrd = addOrd @path root

dTakeAll
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Document
  -> [Element]
dTakeAll = takeAll @path root

dTakeFirst
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Document
  -> Maybe Element
dTakeFirst = takeFirst @path root

dTakeAllIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Document
  -> [Element]
dTakeAllIf = takeAllIf @path root

dTakeFirstIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Document
  -> Maybe Element
dTakeFirstIf = takeFirstIf @path root

dTakeUrls
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Document
  -> [Either SomeException URI]
dTakeUrls = takeUrls @path root

dTakeUrl
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Document
  -> Maybe (Either SomeException URI)
dTakeUrl = takeUrl @path root

dTakeUrlsIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Document
  -> [Either SomeException URI]
dTakeUrlsIf = takeUrlsIf @path root

dTakeUrlIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Document
  -> Maybe (Either SomeException URI)
dTakeUrlIf = takeUrlIf @path root

dDrop
  :: forall (path :: NonEmpty Symbol)
   . (Term path, Term (NonEmptyInit path))
  => Document
  -> Document
dDrop = drop' @path root

dDropIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Document
  -> Document
dDropIf = dropIf @path root

dMerge
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Element
  -> Document
  -> Document
dMerge = merge @path root

dMergeFold
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => [Element]
  -> Document
  -> Document
dMergeFold = mergeFold @path root

-- ** Element tools

eModify
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Element)
  -> Element
  -> Element
eModify = modify @path id

eCreate
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => [Element]
  -> Element
  -> Element
eCreate = create @path id

eAdd
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => [Element]
  -> Element
  -> Element
eAdd = add @path id

eAddOrd
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => [Element]
  -> Element
  -> Element
eAddOrd = addOrd @path id

eTakeAll
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Element
  -> [Element]
eTakeAll = takeAll @path id

eTakeFirst
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Element
  -> Maybe Element
eTakeFirst = takeFirst @path id

eTakeAllIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Element
  -> [Element]
eTakeAllIf = takeAllIf @path id

eTakeFirstIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Element
  -> Maybe Element
eTakeFirstIf = takeFirstIf @path id

eTakeUrls
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Element
  -> [Either SomeException URI]
eTakeUrls = takeUrls @path id

eTakeUrl
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Element
  -> Maybe (Either SomeException URI)
eTakeUrl = takeUrl @path id

eTakeUrlsIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Element
  -> [Either SomeException URI]
eTakeUrlsIf = takeUrlsIf @path id

eTakeUrlIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Element
  -> Maybe (Either SomeException URI)
eTakeUrlIf = takeUrlIf @path id

eDrop
  :: forall (path :: NonEmpty Symbol)
   . (Term path, Term (NonEmptyInit path))
  => Element
  -> Element
eDrop = drop' @path id

eDropIf
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => (Element -> Bool)
  -> Element
  -> Element
eDropIf = dropIf @path id

eMerge
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => Element
  -> Element
  -> Element
eMerge = merge @path id

eMergeFold
  :: forall (path :: NonEmpty Symbol)
   . Term path
  => [Element]
  -> Element
  -> Element
eMergeFold = mergeFold @path id

-- ** Utils

-- | Turn a list of path segments into
-- a 'Traversal' of 'Element's in 'Document'.
foldPath :: NonEmpty String -> Traversal' Document Element
foldPath p = root . foldPath' p

-- | Turn a list of path segments into
-- a 'Traversal' of 'Element's in 'Element'.
foldPath' :: NonEmpty String -> Traversal' Element Element
foldPath' = foldr1 (...) . fmap (named . mk . pack)

makeElement
  :: Name
  -> Map Name Text
  -> [Node]
  -> Element
makeElement n as ns = Element
  { elementName = n
  , elementAttributes = as
  , elementNodes = ns
  }

compareByName
  :: Node
  -> Node
  -> Ordering
compareByName n1 n2 = coerce @_ @Node' n1 `compare` coerce @_ @Node' n2

newtype Node' = Node'
  { unNode' :: Node }
  deriving (Eq, Show)

instance Ord Node' where
  compare (Node' (NodeElement e1)) (Node' (NodeElement e2)) = (compare `on` view localName) e1 e2
  compare (Node' n1)               (Node' n2)               = compare n1 n2

insertList
  :: Ord a
  => (a -> a -> Ordering)
  -> [a] -- ^ New elements
  -> [a] -- ^ Initial list
  -> [a] -- ^ Both of them merged preserving ordering
insertList cmp = foldFs (insertBy cmp)

foldFs
  :: Foldable t
  => (a -> b -> b)
  -> t a
  -> b
  -> b
foldFs f = appEndo . foldMap (Endo . f)
