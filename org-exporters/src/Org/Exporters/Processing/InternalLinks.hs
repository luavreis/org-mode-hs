{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module deals with internal link resolution.
module Org.Exporters.Processing.InternalLinks where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Network.URI.Encode (encodeText)
import Org.Exporters.Processing.OrgData
import Org.Types
import Org.Walk
import System.FilePath (normalise)
import Text.Slugify (slugify)

popUniqueId :: M Text
popUniqueId = do
  ids <- gets1 idStack
  case ids of
    (x : xs) -> x <$ modify1 (\s -> s {idStack = xs})
    [] -> error "something's wrong. out of unique ids"

makeAnchorUnique :: Text -> M Text
makeAnchorUnique a = do
  anchors <- gets1 knownAnchors
  pure
    if a `Set.member` anchors
      then
        fromMaybe a $
          find (`Set.notMember` anchors) $
            map (\n -> a <> "-" <> show (n :: Int)) [1 ..]
      else a

registerFootnote :: Text -> F [OrgElement] -> M ()
registerFootnote name def =
  modify2 \s -> do
    def' <- def
    pure $ s {footnotes = Map.insert name def' (footnotes s)}

registerTarget :: Text -> F [OrgObject] -> M Text
registerTarget name alias = do
  anchors <- gets1 knownAnchors
  uid <- makeAnchorUnique =<< popUniqueId
  modify1 \s -> s {knownAnchors = Set.insert uid anchors}
  modify2 \s -> do
    alias' <- alias
    pure $ s {internalTargets = Map.insert name (uid, alias') (internalTargets s)}
  pure uid

registerAnchorTarget :: Text -> Text -> F [OrgObject] -> M ()
registerAnchorTarget name anchor alias = do
  anchors <- gets1 knownAnchors
  modify1 \s -> s {knownAnchors = Set.insert anchor anchors}
  modify2 \s -> do
    alias' <- alias
    pure $ s {internalTargets = Map.insert name (anchor, alias') (internalTargets s)}

withTargetDescription :: [OrgObject] -> M a -> M a
withTargetDescription descr x = do
  oldCtx <- gets1 targetDescriptionCtx
  modify1 \s -> s {targetDescriptionCtx = Just descr}
  x <* modify1 \s -> s {targetDescriptionCtx = oldCtx}

-- | Create anchors for sections
resolveSection :: WalkM (Compose M F) -> OrgSection -> Compose M F OrgSection
resolveSection r s@OrgSection {..} = Compose $ do
  title <- getCompose $ traverse r sectionTitle
  child <- getCompose $ traverse r sectionChildren
  subse <- getCompose $ traverse r sectionSubsections
  anchor <- case Map.lookup "custom_id" sectionProperties of
    Just a -> do
      registerAnchorTarget ("#" <> a) a title
      registerAnchorTarget ("*" <> sectionRawTitle) a title
      pure a
    Nothing -> do
      a <- makeAnchorUnique $ slugify sectionRawTitle
      registerAnchorTarget ("*" <> sectionRawTitle) a title
      pure a
  return do
    title' <- title
    child' <- child
    subse' <- subse
    return
      s
        { sectionTitle = title',
          sectionChildren = child',
          sectionAnchor = anchor,
          sectionSubsections = subse'
        }

-- | Adds context for list items
resolveListItems :: WalkM (Compose M F) -> [ListItem] -> Compose M F [ListItem]
resolveListItems r items =
  Compose $
    flip evalStateT 1 $
      sequence <$> forM items \(ListItem b i c t e) -> do
        case i of
          Just n0 -> put n0
          Nothing -> modify (+ 1)
        n <- one . Plain . show <$> get
        lift $ withTargetDescription n do
          tag <- getCompose $ traverse r t
          els <- getCompose $ traverse r e
          return $ ListItem b i c <$> tag <*> els

-- | Resolve footnote references
resolveFootnoteRefs ::
  WalkM (Compose M F) ->
  FootnoteRefData ->
  Compose M F FootnoteRefData
resolveFootnoteRefs r = \case
  FootnoteRefDef label objs -> Compose $ do
    objs' <- getCompose $ traverse r objs
    label' <- maybe popUniqueId pure label
    registerFootnote label' (one . Paragraph mempty <$> objs')
    return $ pure $ FootnoteRefLabel label'
  x -> pure x

linkToTarget :: Text -> F (LinkTarget, [OrgObject])
linkToTarget link
  | any (`T.isPrefixOf` link) ["/", "./", "../"],
    let link' = toText (normalise (toString link)) =
      return (URILink "file" link', [Plain link])
  | (prot, rest) <- T.break (== ':') link,
    Just (_, uri) <- T.uncons rest = do
      formatters <- asks (orgLinkAbbrevAlist . exporterSettings)
      case Map.lookup (T.toLower prot) formatters of
        Just repl ->
          let f x =
                if "%s" `T.isInfixOf` repl || "%h" `T.isInfixOf` repl
                  then T.replace "%s" x $ T.replace "%h" (encodeText x) repl
                  else repl <> x
           in linkToTarget (f uri)
        Nothing -> pure (URILink prot uri, [Plain link])
  | otherwise = do
      targets <- asks internalTargets
      case Map.lookup link targets of
        Just (newLink, alias) -> pure (InternalLink newLink, alias)
        Nothing -> pure (UnresolvedLink link, [Plain link])

-- | FIXME This is not exactly how org figures out if a link is an image.
isImgTarget :: LinkTarget -> Bool
isImgTarget (URILink protocol rest) = hasImgExtension && (protocol `elem` imgProtocols)
  where
    hasImgExtension = any (`T.isSuffixOf` T.toLower rest) imgExtensions
    imgExtensions = [".jpeg", ".jpg", ".png", ".gif", ".svg"]
    imgProtocols = ["file", "http", "https", "attachment"]
isImgTarget _ = False

resolveObjects ::
  WalkM (Compose M F) ->
  WalkM (Compose M F) ->
  OrgObject ->
  Compose M F OrgObject
resolveObjects r f = \case
  FootnoteRef fdata -> FootnoteRef <$> resolveFootnoteRefs r fdata
  Link (UnresolvedLink link) descr ->
    Compose $ do
      descr' <- getCompose $ traverse r descr
      return $ do
        (target, descr'') <- linkToTarget link
        descr''' <- descr'
        return $
          if isImgTarget target
            then Image target
            else Link target (if null descr then descr'' else descr''')
  Link target descr -> Link target <$> traverse r descr
  obj -> f obj

resolveElements ::
  WalkM (Compose M F) ->
  WalkM (Compose M F) ->
  OrgElement ->
  Compose M F OrgElement
resolveElements r f = \case
  fd@(FootnoteDef label stuff) ->
    Compose $ do
      stuff' <- getCompose $ traverse r stuff
      registerFootnote label stuff' $> pure fd
  obj -> f obj

resolveLinks :: WalkM (Compose M F)
resolveLinks = buildMultiW \f l ->
  l
    .> resolveSection resolveLinks
    .> resolveObjects resolveLinks f
    .> resolveElements resolveLinks f
    .> resolveListItems resolveLinks
