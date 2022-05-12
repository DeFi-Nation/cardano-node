{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Cardano.Logging.ConfigurationParser
  (
    readConfiguration
  , defaultConfig
  ) where

import           Control.Exception (throwIO)
import qualified Data.Aeson as AE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import           Data.List(foldl')
import           Data.Text (Text, split)
import           Data.Yaml

import           Cardano.Logging.Types

defaultConfig :: TraceConfig
defaultConfig = emptyTraceConfig {
  tcOptions = Map.fromList
    [([] :: Namespace,
         [ ConfSeverity (SeverityF (Just Info))
         , ConfDetail DNormal
         , ConfBackend [Stdout HumanFormatColoured]
         ])
    ]
  }

-- -----------------------------------------------------------------------------
-- Configuration file

readConfiguration :: FilePath -> IO TraceConfig
readConfiguration fp =
    either throwIO pure . parseRepresentation =<< BS.readFile fp

parseRepresentation :: ByteString -> Either ParseException TraceConfig
parseRepresentation bs = transform (decodeEither' bs)
  where
    transform ::
         Either ParseException ConfigRepresentation
      -> Either ParseException TraceConfig
    transform (Left e)   = Left e
    transform (Right rl) = Right $ transform' emptyTraceConfig rl
    transform' :: TraceConfig -> ConfigRepresentation -> TraceConfig
    transform' TraceConfig {tcOptions=to} cr =
      let to'  = foldl' (\ tci (ns, opts) ->
                              let ns' = split (=='.') ns
                                  ns'' = if ns' == [""] then [] else ns'
                              in Map.insertWith (++) ns'' opts tci)
                           to (traceOptions cr)
      in TraceConfig
          to'
          (traceOptionForwarder cr)
          (traceOptionNodeName cr)
          (traceOptionPeerFreqency cr)
          (traceOptionResourceFreqency cr)

data ConfigRepresentation = ConfigRepresentation {
    traceOptions                :: [(Text,[ConfigOption])]
  , traceOptionForwarder        :: TraceOptionForwarder
  , traceOptionNodeName         :: Maybe Text
  , traceOptionPeerFreqency     :: Maybe Int
  , traceOptionResourceFreqency :: Maybe Int
  }
  deriving (Eq, Ord, Show)

instance AE.FromJSON ConfigRepresentation where
    parseJSON (Object obj) = ConfigRepresentation
                           <$> obj .: "TraceOptions"
                           <*> obj .: "TraceOptionForwarder"
                           <*> obj .:? "TraceOptionNodeName"
                           <*> obj .:? "TraceOptionPeerFreqency"
                           <*> obj .:? "TraceOptionResourceFreqency"
