module Org.Types.Aeson where
import qualified Data.Aeson as Aeson

aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '-'
    }
