{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Org.Exporters.Pandoc where

import Org.Exporters.Common

defBackend :: Monad m => ExportBackend m
defBackend =
  let affiliatedMap _ = pure ()
      customExp _ _ _ = Nothing
      srcPretty _ _ _ = namespace pass
      babelCall _ = namespace pass
      macro _ _ = namespace pass
   in ExportBackend {..}

