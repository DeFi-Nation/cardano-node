module Cardano.Logging.Test.Config (
    standardConfig
  ) where

import           Data.Map (fromList)
import           Test.QuickCheck

import           Cardano.Logging

-- | different configurations for testing
config1 :: TraceConfig
config1 = emptyTraceConfig {
  tcOptions = fromList
    [([] :: Namespace,
         [ CoSeverity DebugF
         , CoDetail DNormal
         , CoBackend [Stdout HumanFormatColoured, Forwarder, EKGBackend]
         ])
    ]
  }

config2 :: TraceConfig
config2 = emptyTraceConfig {
  tcOptions = fromList
    [ ([] :: Namespace,
         [ CoSeverity DebugF
         , CoDetail DNormal
         , CoBackend [Stdout HumanFormatColoured, Forwarder, EKGBackend]
         ])
    , (["Node", "Message1"],
         [ CoSeverity InfoF
         , CoDetail DNormal
         , CoBackend [Stdout HumanFormatColoured, EKGBackend]
         ])
    , (["Node", "Message2"],
         [ CoSeverity ErrorF
         , CoDetail DMinimal
         , CoBackend [Forwarder, EKGBackend]
         ])
    ]
  }

instance Arbitrary TraceConfig where
  arbitrary = oneof [config1, config2]
