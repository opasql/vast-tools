{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.XML.Vast.Internal.Tree where

import           Data.Tree
import           Fcf                hiding (Any, type (&&), type (||))
import           Fcf.Class.Foldable
import           Fcf.Class.Monoid

-- * Type-Level Tree definition and tools

-- ** VAST 'Tree'

type VastTree =
  "VAST" :>
    [ Leaf "Error"
    , "Ad" :>
      [ "InLine" :>
        [ Leaf "AdSystem"
        , Leaf "AdTitle"
        , Leaf "Description"
        , Leaf "Advertiser"
        , Leaf "Pricing"
        , Leaf "Survey"
        , Leaf "Error"
        , Leaf "Impression"
        , Singleton "Extensions" "Extension"
        , "Creatives" :>
          '[ "Creative" :>
            [ Singleton "CreativeExtensions" "CreativeExtension"
            , "Linear" :>
              [ Leaf "AdParameters"
              , Leaf "Duration"
              , Singleton "MediaFiles" "MediaFile"
              , Singleton "TrackingEvents" "Tracking"
              , "VideoClicks" :>
                [ Leaf "ClickThrough"
                , Leaf "ClickTracking"
                , Leaf "CustomClick"
                ]
              , "Icons" :>
                [ "IconClicks" :>
                  [ Leaf "IconClickThrough"
                  , Leaf "IconClickTracking"
                  ]
                , Leaf "IconViewTracking"
                ]
              ]
            , Leaf "NonLinear"
            , Leaf "CompanionAds"
            ]
          ]
        ]
      , "Wrapper" :>
        [ Leaf "AdSystem"
        , Leaf "VASTAdTagURI"
        , Leaf "Error"
        , Leaf "Impression"
        , Singleton "Extensions" "Extension"
        , "Creatives" :>
          '[ "Creative" :>
            [ Singleton "CreativeExtensions" "CreativeExtension"
            , "Linear" :>
              [ Singleton "TrackingEvents" "Tracking"
              , "VideoClicks" :>
                [ Leaf "ClickTracking"
                , Leaf "CustomClick"
                ]
              , "Icons" :>
                [ "IconClicks" :>
                  [ Leaf "IconClickThrough"
                  , Leaf "IconClickTracking"
                  ]
                , Leaf "IconViewTracking"
                ]
              ]
            ]
          ]
        ]
      ]
    ]

-- ** Construct Tree

type Leaf a = a :> '[]
type Singleton a b = a :> '[Leaf b]
type a :> as = 'Node a as

-- ** Deconstruct Tree

type family Root (t :: Tree a) :: a where
  Root (a :> as) = a

type family Subtrees (t :: Tree a) :: [Tree a] where
  Subtrees (a :> as) = as

-- ** Traverse Tree

-- | Map implementation for Tree
type instance Eval (Map f ('Node a ts))
  = 'Node (Eval (f a)) (Eval (Map (Map f) ts))

-- | FoldMap implementation for Tree
type instance Eval (FoldMap f (a :> ts))
  = Eval (f a) <> Eval (FoldMap (FoldMap f) ts)

-- | Flatten a tree
type Flatten t = Eval (FlattenImpl t)

-- | 'Flatten' implementation
data FlattenImpl :: Tree a -> Exp [a]
type instance Eval (FlattenImpl t) =
  Eval (FlattenHelper t '[])

data FlattenHelper :: Tree a -> [a] -> Exp [a]
type instance Eval (FlattenHelper (x :> ts) xs) =
  x ': Eval (Foldr FlattenHelper xs ts)

