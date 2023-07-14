{-# LANGUAGE RecordWildCards #-}

module Org.Exporters.LaTeX where

import Org.Exporters.Common

defBackend :: Monad m => ExportBackend m
defBackend =
  let affiliatedMap _ = pure ()
      customElement _ _ _ = Nothing
      customObject _ _ _ = Nothing
      srcPretty _ _ _ = namespace pass
      babelCall _ = namespace pass
      macro _ _ = namespace pass
   in ExportBackend {..}
