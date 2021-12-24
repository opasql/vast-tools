{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Text.XML.Vast.Coerced.Types where

import           Data.OpenApi
import qualified Network.HTTP.Media           as M
import           Servant.API                  (Accept (contentType),
                                               MimeRender (mimeRender),
                                               MimeUnrender (mimeUnrender))
import           Text.XML                     (Document, Element)
import           Text.XML.Vast.Coerced.Parse
import           Text.XML.Vast.Coerced.Render

newtype VastDocument = VastDocument
  { unVastDocument :: Document
  } deriving (Show, Eq)

data XML

instance Accept XML where
  contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

instance MimeRender XML VastDocument where
  mimeRender _ = renderVastLbs

instance MimeUnrender XML VastDocument where
  mimeUnrender _ bs = case parseVastLbs bs of
    Right doc -> Right doc
    Left err  -> Left $ show err

instance ToSchema VastDocument where
  declareNamedSchema _ =
    pure (NamedSchema (Just "VAST") binarySchema)

-- ** Types for VAST elements

newtype Vast = Vast
  { unVast :: Element
  } deriving (Show, Eq)

newtype Nobanner = Nobanner
  { unNobanner :: Element
  } deriving (Show, Eq)

newtype Ad = Ad
  { unAd :: Element
  } deriving (Show, Eq)

newtype InLine = InLine
  { unInLine :: Element
  } deriving (Show, Eq)

newtype Wrapper = Wrapper
  { unWrapper :: Element
  } deriving (Show, Eq)

data WrapperTrackers = WrapperTrackers
  { wtVideoClicks   :: [VideoClicks]
  , wtErrors        :: [Error]
  , wtImpressions   :: [Impression]
  , wtExtensions    :: [Extension]
  , wtTrackings     :: [Tracking]
  , wtClickTracking :: [ClickTracking]
  } deriving (Show, Eq)

newtype Error = Error
  { unError :: Element
  } deriving (Show, Eq)

newtype Impression = Impression
  { unImpression :: Element
  } deriving (Show, Eq)

newtype Creatives = Creatives
  { unCreatives :: Element
  } deriving (Show, Eq)

newtype Creative = Creative
  { unCreative :: Element
  } deriving (Show, Eq)

newtype Linear = Linear
  { unLinear :: Element
  } deriving (Show, Eq)

newtype Duration = Duration
  { unDuration :: Element
  } deriving (Show, Eq)

newtype TrackingEvents = TrackingEvents
  { unTrackingEvents :: Element
  } deriving (Show, Eq)

newtype Tracking = Tracking
  { unTracking :: Element
  } deriving (Show, Eq)

newtype VideoClicks = VideoClicks
  { unVideoClicks :: Element
  } deriving (Show, Eq)

newtype ClickTracking = ClickTracking
  { unClickTracking :: Element
  } deriving (Show, Eq)

newtype Extensions = Extensions
  { unExtensions :: Element
  } deriving (Show, Eq)

newtype Extension = Extension
  { unExtension :: Element
  } deriving (Show, Eq)

newtype AdSystem = AdSystem
  { unAdSystem :: Element
  } deriving (Show, Eq)

newtype AdTitle = AdTitle
  { unAdTitle :: Element
  } deriving (Show, Eq)

newtype Description = Description
  { unDescription :: Element
  } deriving (Show, Eq)

newtype Advertiser = Advertiser
  { unAdvertiser :: Element
  } deriving (Show, Eq)

newtype Pricing = Pricing
  { unPricing :: Element
  } deriving (Show, Eq)

newtype Survey = Survey
  { unSurvey :: Element
  } deriving (Show, Eq)

newtype CreativeExtensions = CreativeExtensions
  { unCreativeExtensions :: Element
  } deriving (Show, Eq)

newtype CreativeExtension = CreativeExtension
  { unCreativeExtension :: Element
  } deriving (Show, Eq)

newtype AdParameters = AdParameters
  { unAdParameters :: Element
  } deriving (Show, Eq)

newtype MediaFiles = MediaFiles
  { unMediaFiles :: Element
  } deriving (Show, Eq)

newtype MediaFile = MediaFile
  { unMediaFile :: Element
  } deriving (Show, Eq)

newtype ClickThrough = ClickThrough
  { unClickThrough :: Element
  } deriving (Show, Eq)

newtype CustomClick = CustomClick
  { unCustomClick :: Element
  } deriving (Show, Eq)

newtype NonLinear = NonLinear
  { unNonLinear :: Element
  } deriving (Show, Eq)

newtype CompanionAds = CompanionAds
  { unCompanionAds :: Element
  } deriving (Show, Eq)

newtype Icons = Icons
  { unIcons :: Element
  } deriving (Show, Eq)

newtype Icon = Icon
  { unIcon :: Element
  } deriving (Show, Eq)

newtype IconClicks = IconClicks
  { unIconClicks :: Element
  } deriving (Show, Eq)

newtype IconClickThrough = IconClickThrough
  { unIconClickThrough :: Element
  } deriving (Show, Eq)

newtype IconClickTracking = IconClickTracking
  { unIconClickTracking :: Element
  } deriving (Show, Eq)

newtype IconViewTracking = IconViewTracking
  { unIconViewTracking :: Element
  } deriving (Show, Eq)

newtype VastAdTagURI = VastAdTagURI
  { unVastAdTagURI :: Element
  } deriving (Show, Eq)

