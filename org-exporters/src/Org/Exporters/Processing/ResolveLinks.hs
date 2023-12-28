{-# LANGUAGE RankNTypes #-}

-- | This module deals with internal link resolution.
module Org.Exporters.Processing.ResolveLinks where

import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Char (isAlpha)
import Data.Ix.Traversable (isequenceA)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Network.URI.Encode (encodeText)
import Optics.Core ((%~), (.~), (?~), (^.), _1, _2, _3)
import Org.Exporters.Processing.OrgData
import Org.Types.Variants.Annotated

resolveObjects ::
  OrgF (T Org) ObjIx ->
  T (OrgF Org) ObjIx
resolveObjects (OrgObject p a d) = Compose do
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
            | Just (anchor, alias) <- targets Map.!? link ->
                return (AnchorLink anchor, alias)
          x -> return (x, objs')
      return $ OrgObject p a $ Link target' objs''
    x -> OrgObject p a <$> coerce (isequenceA x)

-- resolveElements ::
--   OrgElementData (T k) ix ->
--   T (OrgElementData k) ix
-- resolveElements r f = \case
--   fd@(FootnoteDef label stuff) ->
--     Compose $ do
--       stuff' <- getCompose $ traverse r stuff
--       registerFootnote label (Right <$> stuff') $> pure fd
--   (PlainList t i) -> PlainList t <$> resolveListItems i
--   kw@(Keyword k _) -> do
--     Compose $ do
--       kw' <- getCompose $ f kw
--       let val = keywordValue <$> kw'
--       registerKeyword k val
--       return kw'
--   obj -> f obj

-- resolveLinks :: WalkM (Compose M F)
-- resolveLinks = buildMultiW \f l ->
--   l
--     .> resolveSection resolveLinks
--     .> resolveObjects resolveLinks f
--     .> resolveElements resolveLinks f
