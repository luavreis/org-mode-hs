-- |

module Text.Org.Parser.ExperimentalContexts where
import Text.Org.Parser.Definitions
import Relude.Extra (notMember)
import qualified Text.Org.Builder as B
import qualified Data.Text as T

-- | This holds the parsers for ending the parent and the current context and
-- the last char
data MarkupContext m = MarkupContext
    { parentMEnd  :: Marked (WithMContext m) ()
    , contextMEnd :: Marked (WithMContext m) ()
    , lastChar    :: Maybe Char
    }

type WithMContext m = ReaderT (MarkupContext m) (OrgParser m)

contextEnd :: WithMContext m ()
contextEnd = getParser . contextMEnd =<< ask

parentEnd :: WithMContext m ()
parentEnd = getParser . parentMEnd =<< ask

runMContext ::
  MOrgParser m ()
  -> WithMContext m a
  -> OrgParser m a
runMContext p = flip runReaderT (MarkupContext mempty (mapParser lift p) Nothing)

withMContext ::
  -- | End parser for new context
  Marked (WithMContext m) () ->
  WithMContext m a ->
  WithMContext m a
withMContext childEnd ctx = do
  pEnd  <- contextMEnd <$> ask
  ppEnd <- parentMEnd  <$> ask
  lchar <- lastChar    <$> ask
  lift $ runReaderT ctx (MarkupContext (pEnd <> ppEnd) childEnd lchar)

withLastChar ::
  Maybe Char -- ^ End parser for new context
  -> WithMContext m a -- ^ New context
  -> WithMContext m a
withLastChar lchar ctx = do
  plchar <- lastChar <$> ask
  pEnd <- parentMEnd  <$> ask
  cEnd <- contextMEnd <$> ask
  lift $ runReaderT ctx (MarkupContext pEnd cEnd (lchar <|> plchar))

contextMarks :: WithMContext m (Set Char)
contextMarks = do
  ctx <- ask
  pure $ getMarks (contextMEnd ctx) <> getMarks (parentMEnd ctx)

markupContext' :: Monoid k
  => (Text -> k)
  -> Marked (WithMContext m) k
  -> WithMContext m k
markupContext' f elems = try $ do
  specials <- (getMarks elems <>) <$> contextMarks
  str <- optional (takeWhile1P
         (Just $ "chars not in " ++ show (toList specials))
         (`notMember` specials))
  let self = maybe mempty f str
  (self <>) <$> withLastChar (T.last <$> str)
    (anotherEl <|> finishSelf <|> unlessAbort)
  where
    anotherEl = do
      el <- getParser elems
      rest <- markupContext' f elems
      pure $ el <> rest
    finishSelf = do
      contextEnd
      pure mempty
    unlessAbort =
      optional (lookAhead parentEnd) >>= \case
        Nothing -> f . T.singleton <$> anySingle
        _ -> empty

markupContext ::
  Marked (WithMContext m) (F OrgInlines)
  -> WithMContext m (F OrgInlines)
markupContext = markupContext' (pure . B.plain)
