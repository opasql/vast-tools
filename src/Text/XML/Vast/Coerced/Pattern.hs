{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Text.XML.Vast.Coerced.Pattern where

import           Control.Lens
import           Data.Coerce
import           Text.XML.Lens
import           Text.XML.Vast.Coerced.Types
import           Text.XML.Vast.Internal.Path
import           Text.XML.Vast.Internal.Term
import           Text.XML.Vast.Internal.Tools (foldPath)

-- ** Patterns

-- *** Wrapper

isWrapper :: Coercible d Document => d -> Bool
isWrapper (coerce -> d) =  has (foldPath (term @(PathTo "Wrapper"))) d

pattern IsWrapper :: VastDocument
pattern IsWrapper <- (isWrapper -> True)

-- *** InLine

isInLine :: Coercible d Document => d -> Bool
isInLine (coerce -> d) = has (foldPath (term @(PathTo "InLine"))) d

pattern IsInLine :: VastDocument
pattern IsInLine <- (isInLine -> True)

-- *** nobanner

isNobanner :: Coercible d Document => d -> Bool
isNobanner (coerce -> d) = has (root . named "nobanner") d

pattern IsNobanner :: VastDocument
pattern IsNobanner <- (isNobanner -> True)

-- Disable "pattern matching isn't exhausted" warnings.
{-# COMPLETE IsWrapper, IsInLine, IsNobanner #-}

-- *** VPAID

isVpaid :: Coercible d Document => d -> Bool
isVpaid (coerce -> d) = has ( foldPath (term @(PathTo "MediaFile"))
                            . attributeIs "apiFramework" "VPAID"
                            ) d

pattern IsVpaid :: VastDocument
pattern IsVpaid <- (isVpaid -> True)
