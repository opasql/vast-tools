{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Vast.Coerced.Prism
  (
    ID
  , URL
  , Type
  , Event
  , _Impression
  , _Error
  , _Extension
  , _VastLoadExtension
  , _Tracking
  , (#)
  , review
  , preview
  ) where

import           Control.Lens                hiding (element)
import           Data.Map.Strict             as M
import           Data.Text                   (Text)
import           Text.XML
import           Text.XML.Vast.Coerced.Lens
import           Text.XML.Vast.Coerced.Types

type ID = Text
type URL = Text
type Type = Text
type Event = Text

_Impression :: Prism' Impression (ID, URL)
_Impression = prism' embed match
  where
    embed :: (ID, URL) -> Impression
    embed (id', url') = Impression $ Element
      { elementName = "Impression"
      , elementAttributes = M.singleton "id" id'
      , elementNodes = pure $ _Content # url'
      }

    match :: Impression -> Maybe (ID, URL)
    match (Impression e) = do
      id' <- e ^? attr "id"
      url' <- e ^? text
      pure (id', url')

_Error :: Prism' Error URL
_Error = prism' embed match
  where
    embed :: URL -> Error
    embed url' = Error $ Element
      { elementName = "Error"
      , elementAttributes = M.empty
      , elementNodes = pure $ _Content # url'
      }

    match :: Error -> Maybe Text
    match (Error e) = e ^? text


_Extension :: Prism' Extension (Type, Text)
_Extension = prism' embed match
  where
    embed :: (Type, Text) -> Extension
    embed (type', text') = Extension $ Element
      { elementName = "Extension"
      , elementAttributes = M.singleton "type" type'
      , elementNodes = pure $ _Content # text'
      }

    match :: Extension -> Maybe (Type, Text)
    match (Extension e) = do
      type' <- e ^? attr "type"
      text' <- e ^? text
      pure (type', text')

_VastLoadExtension :: Prism' Extension (Type, Tracking)
_VastLoadExtension = prism' embed match
  where
    embed :: (Text, Tracking) -> Extension
    embed (type', Tracking tracking') = Extension $ Element
      { elementName = "Extension"
      , elementAttributes = M.singleton "type" type'
      , elementNodes = pure $ _Element # tracking'
      }

    match :: Extension -> Maybe (Type, Tracking)
    match (Extension e) = do
      type' <- e ^? attr "type"
      tracking' <- e ^? named "tracking"
      pure (type', tracking')

_Tracking :: Prism' Tracking (Event, URL)
_Tracking = prism' embed match
  where
    embed :: (Event, URL) -> Tracking
    embed (event', url') = Tracking $ Element
      { elementName = "Tracking"
      , elementAttributes = M.singleton "event" event'
      , elementNodes = pure $ _Content # url'
      }

    match :: Tracking -> Maybe (Event, URL)
    match (Tracking e) = do
      event' <- e ^? attr "event"
      url' <- e ^? text
      pure (event', url')
