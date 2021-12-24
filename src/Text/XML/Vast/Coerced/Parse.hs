{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Vast.Coerced.Parse where

import           Control.Exception
import qualified Data.ByteString.Lazy         as LBS
import           Data.Coerce
import qualified Data.Text.Lazy               as LT
import           Text.XML
import           Text.XML.Vast.Internal.Parse as Internal

-- ** Parsing

parseVastLbs :: Coercible d Document => LBS.ByteString -> Either SomeException d
parseVastLbs = coerce . Internal.parseLbs

parseFlattenVastLbs :: Coercible d Document => LBS.ByteString -> Either SomeException d
parseFlattenVastLbs = coerce . Internal.parseFlattenLbs

parseVastText :: Coercible d Document => LT.Text -> Either SomeException d
parseVastText = coerce . Internal.parseText

parseFlattenVastText :: Coercible d Document => LT.Text -> Either SomeException d
parseFlattenVastText = coerce . Internal.parseFlattenText

parseVastTextReplacingAmps :: Coercible d Document => LT.Text -> Either SomeException d
parseVastTextReplacingAmps = coerce . Internal.parseFlattenText
