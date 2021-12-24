{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Vast.Coerced.Render where

import qualified Data.ByteString.Lazy          as LBS
import           Data.Coerce
import qualified Data.Text.Lazy                as LT
import           Text.XML
import qualified Text.XML.Vast.Internal.Render as Internal

-- ** Rendering

renderVastLbs :: Coercible d Document => d -> LBS.ByteString
renderVastLbs = Internal.renderLbs . coerce

renderVastText :: Coercible d Document => d -> LT.Text
renderVastText = Internal.renderText . coerce

prettyRenderVastLbs :: Coercible d Document => d -> LBS.ByteString
prettyRenderVastLbs = Internal.prettyRenderLbs . coerce

prettyRenderVastText :: Coercible d Document => d -> LT.Text
prettyRenderVastText = Internal.prettyRenderText . coerce
