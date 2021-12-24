{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Text.XML.Vast.Coerced.Lens where

import           Control.Lens
import qualified Data.CaseInsensitive        as CI
import           Data.Coerce
import           Data.Map                    (Map)
import           Data.Maybe                  (isNothing)
import           Data.Text                   (Text)
import           Text.XML
import qualified Text.XML.Lens               as XML

import           Text.XML.Vast.Coerced.Types

-- ** Basic Lenses

type struct >> field = Traversal' struct field

-- | Representational equality
type a ~~ b = Coercible a b

class a ~~ Element => HasURL a where
  url :: a >> Text
  url = text
  {-# INLINE url #-}

instance HasURL Error
instance HasURL Impression
instance HasURL MediaFile
instance HasURL Tracking
instance HasURL ClickTracking
instance HasURL VastAdTagURI

vast :: VastDocument >> Vast
vast = root . named "VAST"
{-# INLINE vast #-}

nobanner :: VastDocument >> Nobanner
nobanner = root . named "nobanner"

ad :: Vast >> Ad
ad = named "Ad"
{-# INLINE ad #-}

inline :: Ad >> InLine
inline = named "InLine"
{-# INLINE inline #-}

wrapper :: Ad >> Wrapper
wrapper = named "Wrapper"
{-# INLINE wrapper #-}

class a ~~ Element => HasAdSystem a where
  adSystem :: a >> AdSystem
  adSystem = named "AdSystem"
  {-# INLINE adSystem #-}

instance HasAdSystem InLine
instance HasAdSystem Wrapper

adTitle :: InLine >> AdTitle
adTitle = named "AdTitle"
{-# INLINE adTitle #-}

description :: InLine >> Description
description = named "Description"
{-# INLINE description #-}

advertiser :: InLine >> Advertiser
advertiser = named "Advertiser"
{-# INLINE advertiser #-}

pricing :: InLine >> Pricing
pricing = named "Pricing"
{-# INLINE pricing #-}

survey :: InLine >> Survey
survey = named "Survey"
{-# INLINE survey #-}

class a ~~ Element => HasError a where
  error' :: a >> Error
  error' = named "Error"
  {-# INLINE error' #-}

instance HasError Vast
instance HasError InLine
instance HasError Wrapper

class a ~~ Element => HasImpression a where
  impression :: a >> Impression
  impression = named "Impression"
  {-# INLINE impression #-}

instance HasImpression InLine
instance HasImpression Wrapper

class a ~~ Element => HasCreatives a where
  creatives :: a >> Creatives
  creatives = named "Creatives"
  {-# INLINE creatives #-}

instance HasCreatives InLine
instance HasCreatives Wrapper

creative :: Creatives >> Creative
creative = named "Creative"
{-# INLINE creative #-}

creativeExtensions :: Creative >> CreativeExtensions
creativeExtensions = named "CreativeExtensions"
{-# INLINE creativeExtensions #-}

creativeExtension :: CreativeExtensions >> CreativeExtension
creativeExtension = named "CreativeExtension"
{-# INLINE creativeExtension #-}

linear :: Creative >> Linear
linear = named "Linear"
{-# INLINE linear #-}

adParameters :: Linear >> AdParameters
adParameters = named "AdParameters"
{-# INLINE adParameters #-}

duration :: Linear >> Duration
duration = named "Duration"
{-# INLINE duration #-}

mediaFiles :: Linear >> MediaFiles
mediaFiles = named "MediaFiles"
{-# INLINE mediaFiles #-}

mediaFile :: MediaFiles >> MediaFile
mediaFile = named "MediaFile"
{-# INLINE mediaFile #-}

trackingEvents :: Linear >> TrackingEvents
trackingEvents = named "TrackingEvents"
{-# INLINE trackingEvents #-}

tracking :: TrackingEvents >> Tracking
tracking = named "Tracking"
{-# INLINE tracking #-}

videoClicks :: Linear >> VideoClicks
videoClicks = named "VideoClicks"
{-# INLINE videoClicks #-}

clickThrough :: VideoClicks >> ClickThrough
clickThrough = named "ClickThrough"
{-# INLINE clickThrough #-}

clickTracking :: VideoClicks >> ClickTracking
clickTracking = named "ClickTracking"
{-# INLINE clickTracking #-}

customClick :: VideoClicks >> CustomClick
customClick = named "CustomClick"
{-# INLINE customClick #-}

icons :: Linear >> Icons
icons = named "Icons"
{-# INLINE icons #-}

icon :: Icons >> Icon
icon = named "Icon"
{-# INLINE icon #-}

iconClicks :: Icon >> IconClicks
iconClicks = named "IconClicks"
{-# INLINE iconClicks #-}

iconClickThrough :: IconClicks >> IconClickThrough
iconClickThrough = named "IconClickThrough"
{-# INLINE iconClickThrough #-}

iconClickTracking :: IconClicks  >> IconClickTracking
iconClickTracking = named "IconClickTracking"
{-# INLINE iconClickTracking #-}

iconViewTracking :: Icon >> IconViewTracking
iconViewTracking = named "IconViewTracking"
{-# INLINE iconViewTracking #-}

nonLinear :: Creative >> NonLinear
nonLinear = named "NonLinear"
{-# INLINE nonLinear #-}

companionAds :: Creative >> CompanionAds
companionAds = named "CompanionAds"
{-# INLINE companionAds #-}

class a ~~ Element => HasExtensions a where
  extensions :: a >> Extensions
  extensions = named "Extensions"
  {-# INLINE extensions #-}

instance HasExtensions InLine
instance HasExtensions Wrapper

extension :: Extensions >> Extension
extension = named "Extension"
{-# INLINE extension #-}

vastAdTagURI :: Wrapper >> VastAdTagURI
vastAdTagURI = named "VASTAdTagURI"
{-# INLINE vastAdTagURI #-}

-- ** Coercible xml-conduit lenses
-- These lenses are just coerced versions of those defined at 'Text.XML.Lens'.

-- | The root element of the document.
root :: (a ~~ Document) => Lens' a Vast
root = coerced . XML.root . coerced
{-# INLINE root #-}

prologue :: a ~~ Document => Lens' a Prologue
prologue = coerced . XML.prologue
{-# INLINE prologue #-}

epilogue :: a ~~ Document => Lens' a [Miscellaneous]
epilogue = coerced . XML.epilogue
{-# INLINE epilogue #-}

doctype :: Lens' Prologue (Maybe Doctype)
doctype = XML.doctype
{-# INLINE doctype #-}

class AsInstruction t where
    _Instruction :: Prism' t Instruction

_instructionTarget :: Lens' Instruction Text
_instructionTarget f (Instruction t d) = f t <&> \t' -> Instruction t' d
{-# INLINE _instructionTarget #-}

_instructionData :: Lens' Instruction Text
_instructionData f (Instruction t d) = f d <&> \d' -> Instruction t d'
{-# INLINE _instructionData #-}

instance AsInstruction Node where
    _Instruction = prism' NodeInstruction $ \case
        NodeInstruction e -> Just e
        _                 -> Nothing
    {-# INLINE _Instruction #-}

instance AsInstruction Miscellaneous where
    _Instruction = prism' MiscInstruction $ \case
        MiscInstruction e -> Just e
        _                 -> Nothing
    {-# INLINE _Instruction #-}

class AsComment t where
    _Comment :: Prism' t Text

instance AsComment Node where
    _Comment = prism' NodeComment $ \case
        NodeComment e -> Just e
        _             -> Nothing
    {-# INLINE _Comment #-}

instance AsComment Miscellaneous where
    _Comment = prism' MiscComment $ \case
        MiscComment e -> Just e
        _             -> Nothing
    {-# INLINE _Comment #-}

_nameLocalName :: Lens' Name Text
_nameLocalName f n = f (nameLocalName n) <&> \x -> n { nameLocalName = x }
{-# INLINE _nameLocalName #-}

_nameNamespace :: Lens' Name (Maybe Text)
_nameNamespace f n = f (nameNamespace n) <&> \x -> n { nameNamespace = x }
{-# INLINE _nameNamespace #-}

_namePrefix :: Lens' Name (Maybe Text)
_namePrefix f n = f (namePrefix n) <&> \x -> n { namePrefix = x }
{-# INLINE _namePrefix #-}

_Element :: a ~~ Element => Prism' Node a
_Element = XML._Element . coerced
{-# INLINE _Element #-}

_Content :: Prism' Node Text
_Content = prism' NodeContent $ \case
    NodeContent e -> Just e
    _             -> Nothing
{-# INLINE _Content #-}

name :: a ~~ Element => Lens' a Name
name = coerced . XML.name
{-# INLINE name #-}

localName :: a ~~ Element => Lens' a Text
localName = coerced . XML.localName
{-# INLINE localName #-}

attrs :: a ~~ Element => Lens' a (Map Name Text)
attrs = coerced . XML.attrs
{-# INLINE attrs #-}

nodes :: a ~~ Element => Lens' a [Node]
nodes = coerced . XML.nodes
{-# INLINE nodes #-}

attr :: a ~~ Element => Name -> Traversal' a Text
attr n = attrs . ix n
{-# INLINE attr #-}

attribute :: a ~~ Element => Name -> Lens' a (Maybe Text)
attribute n = attrs . at n
{-# INLINE attribute #-}

-- | Traverse elements which has the specified *local* name (case-insensitive).
named :: (a ~~ Element, b ~~ Element) => CI.CI Text -> Traversal' a b
named n = coerced . XML.named n . coerced
{-# INLINE named #-}

attributeSatisfies
  :: (a ~~ Element)
  => Name
  -> (Text -> Bool)
  -> Traversal' a a
attributeSatisfies n p = attributeSatisfies' n (maybe False p)
{-# INLINE attributeSatisfies #-}

attributeSatisfies'
  :: (a ~~ Element)
  => Name
  -> (Maybe Text -> Bool)
  -> Traversal' a a
attributeSatisfies' n p = coerced . XML.attributeSatisfies' n p . coerced
{-# INLINE attributeSatisfies' #-}

withoutAttribute
  :: (a ~~ Element)
  => Name
  -> Traversal' a a
withoutAttribute n = attributeSatisfies' n isNothing
{-# INLINE withoutAttribute #-}

attributeIs
  :: (a ~~ Element)
  => Name
  -> Text
  -> Traversal' a a
attributeIs n v = attributeSatisfies n (==v)
{-# INLINE attributeIs #-}

-- | Traverse all contents of the element.
text :: a ~~ Element => Traversal' a Text
text = coerced . XML.text
{-# INLINE text #-}

-- | Traverse all comments of the element.
comment :: a ~~ Element => Traversal' a Text
comment = nodes . traverse . _Comment
{-# INLINE comment #-}

-- | 'plate' traverses over its sub-elements.
instance a ~~ Element => Plated a where
  plate = nodes . traverse . _Element
  {-# INLINE plate #-}
