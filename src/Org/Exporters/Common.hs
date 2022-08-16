{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Org.Exporters.Common where
import Org.Types
import Org.Walk
import Data.Time (TimeLocale, defaultTimeLocale)
import Data.Text qualified as T
import Ondim
import Data.Map.Syntax
import System.FilePath (isRelative, takeExtension, (-<.>))
import Ondim.Extra (Attribute, ignore, HasAttrChild)
import Relude.Extra (toPairs, insert)

data ExporterState = ExporterState
  { footnoteCounter :: (Int, Map Text Int)
  , exporterSettings :: ExporterSettings
  }

defaultExporterState :: ExporterState
defaultExporterState = ExporterState
  { footnoteCounter = (1, mempty)
  , exporterSettings = defaultExporterSettings
  }

getSetting :: MonadExporter m => (ExporterSettings -> b) -> m b
getSetting f = f <$> gets exporterSettings

getFootnoteRef :: MonadExporter n => Text -> n Text
getFootnoteRef label = do
  (i, m) <- gets footnoteCounter
  modify \s -> s { footnoteCounter = (i + 1, insert label i m) }
  pure (show i)

type MonadExporter n = MonadState ExporterState n

class ( OndimTag tag
      , MonadExporter (OndimMonad tag)
      , OndimNode tag (OutputFor tag)
      ) => HasOutput tag where
  type OutputFor tag

-- type Expanded t = Ondim t [OutputFor t]

-- class HasOutput t => CanExpand t a where
--   expand :: a -> Expanded t

-- instance CanExpand t a => CanExpand t [a] where
--   expand = fmap join . mapM expand

-- instance (CanExpand t a, CanExpand t b) => CanExpand t (Either a b) where
--   expand = either expand expand

-- instance (CanExpand t a, CanExpand t b) => CanExpand t (a, b) where
--   expand content =
--     liftA2 (<>) (expand (fst content)) (expand (snd content))

data ExporterSettings = ExporterSettings
  { orgExportHeadlineLevels :: Int
  , orgExportWithSpecialStrings :: Bool
  , headlineLevelShift :: Int
  , timeLocale :: TimeLocale
  } deriving (Eq, Show)

defaultExporterSettings :: ExporterSettings
defaultExporterSettings = ExporterSettings
  { orgExportHeadlineLevels = 3
  , orgExportWithSpecialStrings = True
  , headlineLevelShift = 0
  , timeLocale = defaultTimeLocale
  }

doSpecialStrings :: Text -> Text
doSpecialStrings txt =
  txt & T.replace "---" "—"
      & T.replace "--" "–"
      & T.replace "..." "…"
      & T.replace "\\-" "\173"

processSpecialStrings :: Walkable OrgObject a => a -> a
processSpecialStrings = walk process
  where
    process :: OrgObject -> OrgObject
    process (Plain txt) = Plain $ doSpecialStrings txt
    process x = x

justOrIgnore :: OndimTag tag => Maybe a -> (a -> Expansion tag b) -> Expansion tag b
justOrIgnore = flip (maybe ignore)

tags :: HasAttrChild tag t => Tag -> Expansion tag t
tags tag x = children x `bindingText` ("tag" ## pure tag)

parsedKwExpansions :: ([OrgObject] -> Ondim tag [t]) -> Affiliated -> Text -> Expansions' tag t
parsedKwExpansions f kws prefix =
  forM_ parsedKws \(name, t) ->
    (prefix <> name) ## const $ f t
  where
    parsedKws = mapMaybe
      (\case (n, ParsedKeyword _ t) -> Just (n, t); _ -> Nothing) (toPairs kws)

textKwExpansions :: OndimTag t => Affiliated -> Text -> MapSyntax Text (Ondim t Text)
textKwExpansions kws prefix =
  forM_ textKws \(name, t) ->
    (prefix <> name) ## pure t
  where
    textKws = mapMaybe
      (\case (n, ValueKeyword _ t) -> Just (n, t); _ -> Nothing) (toPairs kws)

-- | Attribute expansion for affiliated keywords.
affiliatedAttrExpansions :: OndimTag t => Text -> Affiliated -> Expansions' t Attribute
affiliatedAttrExpansions lang aff =
  "affiliated" ## const $ pure affAttrs
  where
    affAttrs :: [(Text, Text)]
    affAttrs = join $ mapMaybe getHtmlAttrs (toPairs aff)
      where
        getHtmlAttrs (k, BackendKeyword x)
          | ("attr_" <> lang) `T.isPrefixOf` k = Just x
        getHtmlAttrs _ = Nothing

-- | Text expansion for link target.
linkTarget :: OndimTag t => LinkTarget -> MapSyntax Text (Ondim t Text)
linkTarget tgt = do
  "target" ## pure case tgt of
    URILink "file" (changeExtension -> file)
      | isRelative file -> toText file
      | otherwise -> "file:///" <> T.dropWhile (== '/') (toText file)
    URILink protocol uri -> protocol <> ":" <> uri
    InternalLink anchor -> "#" <> anchor
    UnresolvedLink tgt' -> tgt'
  case tgt of
    URILink protocol _ -> "protocol" ## pure protocol
    InternalLink {} -> "protocol" ## pure "internal"
    _ -> pure ()
  where
    changeExtension (toString -> file) =
      if takeExtension file == ".org"
      then file -<.> ".html"
      else file
