module Text.XML.Vast
  ( module M
  ) where

import           Text.XML                      as M hiding (parseLBS, parseText,
                                                     renderLBS, renderText)
import           Text.XML.Lens                 as M
import           Text.XML.Vast.Internal.Parse  as M
import           Text.XML.Vast.Internal.Path   as M
import           Text.XML.Vast.Internal.Render as M
import           Text.XML.Vast.Internal.Term   as M
import           Text.XML.Vast.Internal.Tools  as M
import           Text.XML.Vast.Internal.Tree   as M
