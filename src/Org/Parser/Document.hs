-- |

module Org.Parser.Document where

import Prelude hiding (many, some)
import Org.Parser.Definitions
import Org.Parser.Common
import Org.Parser.Elements
import Org.Parser.Objects
import Org.Parser.MarkupContexts
import qualified Data.Text as T
import qualified Data.Map as M

-- | Parse input as org document tree.
orgDocument :: OrgParser OrgDocument
orgDocument = do
  _ <- many commentLine
  properties <- option mempty propertyDrawer
  topLevel <- elements
  sections <- many (section 1)
  eof
  -- At this point, the whole document was parsed.
  -- This means an state with keywords is available.
  finalState <- getState
  return $ flip runReader finalState . getAp $ do
    keywords' <- sequence $ orgStateKeywords finalState
    footnotes' <- sequence $ orgStateFootnotes finalState
    topLevel' <- topLevel
    sections' <- sequence sections
    return OrgDocument
      { documentProperties = properties
      , documentKeywords = keywords'
      , topLevelContents = toList topLevel'
      , documentChildren = sections'
      , documentFootnotes = M.map toList footnotes'
      }

-- | Read an Org mode section and its contents. @lvl@
-- gives the minimum acceptable level of the heading.
section
  :: Int
  -> OrgParser (F OrgSection)
section lvl = try $ do
  level <- headingStart
  guard (lvl <= level)
  todoKw <- optional todoKeyword
  priority <- optional priorityCookie
  (title, tags) <- titleObjects
  planning   <- option emptyPlanning planningInfo
  properties <- option mempty propertyDrawer
  contents   <- elements
  children   <- many (section (level + 1))
  return $ do
    title'    <- title
    contents' <- contents
    children' <- sequence children
    return OrgSection
      { sectionLevel = level
      , sectionProperties = properties
      , sectionTodo = todoKw
      , sectionPriority = priority
      , sectionTitle = toList title'
      , sectionTags = tags
      , sectionPlanning = planning
      , sectionContents = toList contents'
      , sectionChildren = children'
      }
 where
   titleObjects :: OrgParser (F OrgInlines, Tags)
   titleObjects = runMContext_
                  (mark " \n" endOfTitle)
                  (plainMarkupContext standardSet)

   endOfTitle :: OrgParser Tags
   endOfTitle = try $ do
     skipSpaces
     tags <- option [] (headerTags <* skipSpaces)
     void newline <|> eof
     return tags

   headerTags :: OrgParser Tags
   headerTags = try $ do
     _ <- char ':'
     endBy1 orgTagWord (char ':')


-- * Heading and document "subelements"

-- | Parse a to-do keyword that is registered in the state.
todoKeyword :: OrgParser TodoKeyword
todoKeyword = try $ do
  taskStates <- activeTodoMarkers <$> getState
  choice (map kwParser taskStates)
  where
    kwParser :: TodoKeyword -> OrgParser TodoKeyword
    kwParser tdm =
      -- NOTE to self: space placement - "TO" is subset of "TODOKEY"
      try (string (todoName tdm) *> space $> tdm)

-- | Parse a priority cookie like @[#A]@.
priorityCookie :: OrgParser Priority
priorityCookie = try $
  string "[#"
  *> priorityFromChar
  <* char ']'
  where
    priorityFromChar :: OrgParser Priority
    priorityFromChar =
      NumericPriority <$> digitIntChar <|>
      LetterPriority <$> uppercaseAZ

orgTagWord :: OrgParser Text
orgTagWord = takeWhile1P (Just "tag characters (alphanumeric, @, %, # or _)")
             (\c -> isAlphaNum c || c `elem` ['@', '%', '#', '_'])

-- | TODO READ ABOUT PLANNING
emptyPlanning :: PlanningInfo
emptyPlanning = PlanningInfo Nothing Nothing Nothing

-- | Read a single planning-related and timestamped line. TODO
planningInfo :: OrgParser PlanningInfo
planningInfo = try $ do
  updaters <- some planningDatum <* skipSpaces <* newline
  return $ foldr ($) emptyPlanning updaters
 where
  planningDatum = skipSpaces *> choice
    [ updateWith (\s p -> p { planningScheduled = Just s}) "SCHEDULED"
    , updateWith (\d p -> p { planningDeadline = Just d}) "DEADLINE"
    , updateWith (\c p -> p { planningClosed = Just c}) "CLOSED"
    ]
  updateWith fn cs = fn <$> (string cs *> char ':' *> skipSpaces *> parseTimestamp)

-- | Read a :PROPERTIES: drawer and return the key/value pairs contained
-- within.
propertyDrawer :: OrgParser Properties
propertyDrawer = try $ do
  skipSpaces
  _ <- string' ":properties:"
  skipSpaces
  _ <- newline
  manyTill nodeProperty (try endOfDrawer)
  where
   endOfDrawer :: OrgParser Text
   endOfDrawer = try $
     hspace *> string' ":end:" <* space

   nodeProperty :: OrgParser (PropertyName, PropertyValue)
   nodeProperty = try $ liftA2 (,) name value

   name :: OrgParser PropertyName
   name =
     skipSpaces
     *> char ':'
     *> takeWhile1P (Just "node property name") (not . isSpace)
        <&> T.stripSuffix ":"
        >>= guardMaybe "expecting ':' at end of node property name"

   value :: OrgParser PropertyValue
   value =
     skipSpaces
     *> (takeWhileP (Just "node property value") (/= '\n')
         <&> T.stripEnd)
     <* newline
