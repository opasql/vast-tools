module Text.XML.Vast.Internal.Render where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map
import qualified Data.Text.Lazy       as TextLazy
import           Text.XML             hiding (parseLBS, renderLBS)
import qualified Text.XML             as XML (renderLBS, renderText)

renderLbs :: Document -> LBS.ByteString
renderLbs = XML.renderLBS defaultRenderSettings

renderText :: Document -> TextLazy.Text
renderText = XML.renderText defaultRenderSettings

prettyRenderLbs :: Document -> LBS.ByteString
prettyRenderLbs = XML.renderLBS
  defaultRenderSettings
    { rsPretty = True
    }

prettyRenderText :: Document -> TextLazy.Text
prettyRenderText = XML.renderText
  defaultRenderSettings
    { rsPretty = True
    }

defaultRenderSettings :: RenderSettings
defaultRenderSettings = def
  { rsPretty = False
  , rsNamespaces = []
  , rsAttrOrder = const Map.toList
  , rsUseCDATA = const True
  , rsXMLDeclaration = False
  }
