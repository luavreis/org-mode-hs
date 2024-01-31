{-# LANGUAGE RankNTypes #-}

-- | This module deals with internal link resolution.
module Org.Exporters.Processing.ResolveLinks (resolveLinks) where

import Control.Category.Natural (type (~>) (..))
import Control.Category.RecursionSchemes qualified as R
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Ix.RecursionSchemes (Fix (..))
import Data.Ix.Traversable (isequenceA)
import Data.Map qualified as Map
import Data.Text qualified as T
import Network.URI.Encode (encodeText)
import Org.Exporters.Processing.OrgData
import Org.Types.Variants.Annotated

resolveObjects ::
  OrgF (T Org) ObjIx ->
  M (OrgF Org ObjIx)
resolveObjects (OrgObject p a d) = do
  case d of
    FootnoteRef (FootnoteRefLabel label) -> do
      targets <- gets (.footnotes)
      let anchor = targets Map.!? label <&> fst
          a' = a & maybe id (Aeson.insert "target" . Aeson.String) anchor
      return $ OrgObject p a' (FootnoteRef (FootnoteRefLabel label))
    Link target objs -> do
      objs' <- getCompose objs
      formatters <- gets (.exporterSettings.orgLinkAbbrevAlist)
      targets <- gets (.internalTargets)
      (target', objs'') <-
        target & fix \f -> \case
          URILink prot uri
            | Just repl <- formatters Map.!? T.toLower prot -> do
                let replaced =
                      if "%s" `T.isInfixOf` repl || "%h" `T.isInfixOf` repl
                        then T.replace "%s" uri $ T.replace "%h" (encodeText uri) repl
                        else repl <> uri
                 in f (UnresolvedLink replaced)
          UnresolvedLink link
            | Just (anchor, alias) <- targets Map.!? unresolvedToInternalLink link ->
                return (AnchorLink anchor, if mempty == objs' then alias else objs')
          x -> return (x, objs')
      return $ OrgObject p a $ Link target' objs''
    x -> OrgObject p a <$> coerce (isequenceA x)

resolveLinks :: Org ix -> M (Org ix)
resolveLinks = getCompose . (R.fold (NT go) #)
  where
    go :: ComposeIx [] OrgF (T Org) ix -> T Org ix
    go (ComposeIx x) =
      Compose $ Fix <$> coerce @(M [_]) do
        forM x \case
          o@OrgObject' {} -> resolveObjects o
          y -> getCompose $ isequenceA y
