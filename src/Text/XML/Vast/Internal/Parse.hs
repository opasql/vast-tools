{-# LANGUAGE OverloadedStrings #-}

module Text.XML.Vast.Internal.Parse where

import           Control.Exception
import           Control.Lens          (over)
import qualified Data.ByteString.Lazy  as LBS
import           Data.Char
import           Data.Maybe
import           Data.Text             as Text
import qualified Data.Text.Lazy        as TextLazy
import           Text.XML              hiding (parseLBS, renderLBS)
import qualified Text.XML              as XML (parseLBS, parseText)
import           Text.XML.Lens
import           Text.XML.Stream.Parse

-- | Parse lazy 'ByteString' to 'Document'.
parseLbs :: LBS.ByteString -> Either SomeException Document
parseLbs = XML.parseLBS defaultParseSettings

-- | Parse lazy 'ByteString' to 'Document' and perform 'flatten'.
--
-- Note: This version is much more expensive than 'parseLbs'
-- since it traverses entire 'Document' in case of successful
-- parsing to get rid of empty 'NodeContent's which appear when
-- parsing prettyfied XML.
parseFlattenLbs :: LBS.ByteString -> Either SomeException Document
parseFlattenLbs = fmap flatten . XML.parseLBS defaultParseSettings

-- | Parse lazy 'Text' to 'Document'.
parseText :: TextLazy.Text -> Either SomeException Document
parseText = XML.parseText defaultParseSettings

-- | Parse lazy 'Text' to 'Document' and perform 'flatten'.
--
-- Note: This version is much more expensive than 'parseText'
-- since it traverses entire 'Document' in case of successful
-- parsing to get rid of empty 'NodeContent's which appear when
-- parsing prettyfied XML.
parseFlattenText :: TextLazy.Text -> Either SomeException Document
parseFlattenText = fmap flatten . XML.parseText defaultParseSettings

-- | Parse lazy 'Text' to 'Document', replace all @&@ with @&amp;@
-- and peroform 'flatten'.
--
-- Note: This version is much more expensive than 'Text.XML.parseText'
-- since it replaces all @&@ with @&amp;@ and
-- traverses entire 'Document' in case of successful
-- parsing to get rid of empty 'NodeContent's which appear when
-- parsing prettyfied XML.
parseTextReplacingAmps :: TextLazy.Text -> Either SomeException Document
parseTextReplacingAmps
  = fmap flatten
  . XML.parseText defaultParseSettings
  . TextLazy.replace "&" "&amp;"

defaultParseSettings :: ParseSettings
defaultParseSettings = def
    { psDecodeEntities = decodeXmlEntities
    , psRetainNamespaces = False
    , psDecodeIllegalCharacters = const Nothing
    , psEntityExpansionSizeLimit = 8192
    }

-- | Delete empty 'NodeContent's and trim nonempty ones.
-- This function essentially will make pretty XML flat and ugly.
flatten :: Document -> Document
flatten = over root to
  where
    to :: Element -> Element
    to = over nodes (mapMaybe fro)

    fro :: Node -> Maybe Node
    fro (NodeContent c) = NodeContent <$> isEmptyContent c
    fro (NodeElement e) = Just . NodeElement $ to e
    fro other           = Just other

    isEmptyContent :: Text -> Maybe Text
    isEmptyContent t =
      let trimmed = trim t
      in if Text.null trimmed
            then Nothing
            else Just trimmed

    trim :: Text -> Text
    trim = Text.dropWhile isSpace . Text.dropWhileEnd isSpace
