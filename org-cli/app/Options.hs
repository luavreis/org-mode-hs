{-# LANGUAGE ApplicativeDo #-}

-- | Options datatype and parsing for the CLI.
module Options where

import Options.Applicative

data AppCmd
  = CmdExport AppOptions
  | CmdInitTemplates

data AppOptions = AppOptions
  { backend :: BackendOptions
  , input :: Input
  , output :: Output
  }

data BackendOptions
  = AST {pretty :: Bool}
  | Ondim {ondimOptions :: OndimOptions}

data OndimFormat
  = HTML
  | Pandoc
  | LaTeX

data OndimOptions = OndimOptions
  { format :: OndimFormat
  , templateDir :: Maybe FilePath
  , layout :: Text
  }

appCmd :: ParserInfo AppCmd
appCmd =
  info
    ( hsubparser
        ( command
            "export"
            ( info
                (CmdExport <$> appOptions)
                (progDesc "Parse and export an Org file")
            )
            <> command
              "init-templates"
              ( info
                  (pure CmdInitTemplates)
                  (progDesc "Creates a '.horg' directory with the default Ondim template files for all available formats")
              )
        )
        <**> helper
    )
    ( fullDesc
        <> progDesc
          "horg is a parser for Org Mode documents with customizable exporters.\
          \ Use --help for more info."
        <> header "horg - parse and export your Org documents."
    )

appOptions :: Parser AppOptions
appOptions =
  AppOptions <$> backend' <*> input' <*> output'

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

ondimOptions' :: OndimFormat -> Parser OndimOptions
ondimOptions' fmt =
  OndimOptions fmt
    <$> optional (strOption (long "template-dir" <> metavar "DIR"))
    <*> strOption (long "layout" <> metavar "NAME" <> value ("document." <> ext) <> showDefault)
  where
    ext = case fmt of
      HTML -> "html"
      Pandoc -> "md"
      LaTeX -> "tex"

backend' :: Parser BackendOptions
backend' =
  hsubparser
    ( command "ast" (info ast (progDesc "Export the parsed AST"))
        <> command "html" (info html (progDesc "Export to HTML using Ondim."))
        <> command "latex" (info latex (progDesc "Export to LaTeX using Ondim."))
        <> command "pandoc" (info pandoc (progDesc "Export to Pandoc JSON using Ondim."))
    )
  where
    ast = AST . not <$> switch (long "no-pretty" <> help "Disable pretty-printing of the parsed AST")
    html = Ondim <$> ondimOptions' HTML
    latex = Ondim <$> ondimOptions' LaTeX
    pandoc = Ondim <$> ondimOptions' Pandoc

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
