{-# LANGUAGE ApplicativeDo #-}

-- | Options datatype and parsing for the CLI.
module Options where

import Options.Applicative
import Org.Exporters.Common
import Org.Parser (OrgOptions (..), defaultOrgOptions)

data AppOptions = AppOptions
  { backend :: BackendOptions,
    input :: Input,
    options :: OrgOptions,
    output :: Output
  }

data BackendOptions
  = AST {pretty :: Bool}
  | HTML {ondimOptions :: OndimOptions}
  | Pandoc {formatSpec :: Text, template :: Maybe FilePath, ondimOptions :: OndimOptions}

data OndimOptions = OndimOptions
  { settings :: ExporterSettings,
    templateDir :: Maybe FilePath
  }

appOptions :: ParserInfo AppOptions
appOptions =
  info
    (AppOptions <$> backend' <*> input' <*> options' <*> output' <**> helper)
    ( fullDesc
        <> progDesc "org-hs is a parser for Org Mode documents with customizable exporters."
        <> header "org-hs - parse and export your Org documents."
    )

options' :: Parser OrgOptions
options' =
  OrgOptions
    <$> option
      falsity
      ( long "org-src-preserve-indentation"
          <> value a
          <> metavar "BOOL"
          <> falsityShow
      )
    <*> option
      auto
      ( long "tab-width"
          <> value b
          <> showDefault
          <> metavar "WIDTH"
      )
    <*> option
      auto
      ( long "org-element-parsed-keywords"
          <> value c
          <> showDefault
          <> metavar "LIST"
      )
    <*> option
      auto
      ( long "org-element-dual-keywords"
          <> value d
          <> showDefault
          <> metavar "LIST"
      )
    <*> option
      auto
      ( long "org-element-affiliated-keywords"
          <> value e
          <> showDefault
          <> metavar "LIST"
      )
  where
    OrgOptions a b c d e = defaultOrgOptions

settings' :: Parser ExporterSettings
settings' =
  ExporterSettings
    <$> option auto (long "org-export-headline-levels" <> value a <> showDefault)
    <*> option falsity (long "org-export-with-special-strings" <> value b <> metavar "BOOL" <> falsityShow)
    <*> option falsity (long "org-export-with-entities" <> value c <> metavar "BOOL" <> falsityShow)
    <*> option auto (long "healine-level-shift" <> value d <> showDefault)
    ?? e
  where
    ExporterSettings a b c d e = defaultExporterSettings

falsity :: ReadM Bool
falsity = maybeReader $ \case
  s
    | s `elem` ["t", "yes", "y"] -> Just True
    | s `elem` ["nil", "no", "n"] -> Just True
    | otherwise -> Nothing

falsityShow :: Mod f Bool
falsityShow = showDefaultWith $ \case
  True -> "t"
  False -> "nil"

ondimOptions' :: Parser OndimOptions
ondimOptions' =
  OndimOptions
    <$> settings'
    <*> optional (strOption (long "template-dir" <> metavar "DIR"))

backend' :: Parser BackendOptions
backend' =
  hsubparser
    ( command "ast" (info ast (progDesc "Export the parsed AST"))
        <> command "html" (info html (progDesc "Export to HTML using Ondim."))
        <> command
          "pandoc"
          ( info
              pandoc
              ( progDesc
                  "Export to Pandoc using Ondim \
                  \(and then to the Pandoc writer of your choice, see FORMAT-SPEC).\n\
                  \For more advanced Pandoc options, please pipe the generated JSON into the pandoc command."
              )
          )
    )
  where
    ast = AST . not <$> switch (long "no-pretty" <> help "Disable pretty-printing of the parsed AST")
    html = HTML <$> ondimOptions'
    pandoc =
      Pandoc
        <$> strArgument
          ( metavar "FORMAT-SPEC"
              <> help "Format spec used by the Pandoc writer. See the Pandoc documentation for a list of available writers and extensions."
              <> value "json"
          )
        <*> optional
          ( strOption
              ( long "template"
                  <> metavar "FILEPATH"
                  <> help "Template used by the Pandoc writer. See the Pandoc documentation for help with such templates."
              )
          )
        <*> ondimOptions'

data Input
  = FileInput FilePath
  | StdInput

data Output
  = FileOutput FilePath
  | StdOutput

input' :: Parser Input
input' = file <|> std
  where
    file =
      FileInput
        <$> strOption
          ( long "in"
              <> short 'i'
              <> metavar "FILEPATH"
              <> help "Input file"
          )
    std =
      flag'
        StdInput
        ( long "stdin"
            <> help "Read from stdin"
        )

output' :: Parser Output
output' = file <|> std
  where
    file =
      FileOutput
        <$> strOption
          ( long "out"
              <> short 'o'
              <> metavar "FILEPATH"
              <> help "Output file"
          )
    std =
      flag'
        StdOutput
        ( long "stdout"
            <> help "Write to stdout"
        )
