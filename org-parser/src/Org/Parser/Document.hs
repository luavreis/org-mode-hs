-- | Parsers for Org documents.
module Org.Parser.Document
  ( -- Document and sections
    orgDocument
  , section

    -- * Components
  , propertyDrawer
  ) where

import Data.Text qualified as T
import Org.Parser.Common
import Org.Parser.Definitions hiding (section)
import Org.Parser.Definitions qualified as D
import Org.Parser.Elements
import Org.Parser.MarkupContexts
import Org.Parser.Objects
import Prelude hiding (many, some)

-- | Parse an Org document.
orgDocument :: OrgParser (OrgDocumentData OrgParsed i)
orgDocument = do
  skipMany commentLine
  properties <- option mempty propertyDrawer
  topLevel <- elements
  sections' <- sections 1
  eof
  return $
    OrgDocumentData
      { properties = properties
      , children = topLevel
      , sections = sections'
      }

sections :: Int -> OrgParser OrgSections
sections lvl =
  mconcat <$> many do
    begin <- getOffset
    datum <- section lvl
    end <- getOffset
    return $ D.section begin end datum

{- | Parse an Org section and its contents. @lvl@ gives the minimum acceptable
   level of the heading.
-}
section :: Int -> OrgParser OrgSectionD
section lvl = try $ do
  level <- headingStart
  guard (lvl <= level)
  todoKw <- optional todoKeyword
  isComment <- option False $ try $ string "COMMENT" *> hspace1 $> True
  priority <- optional priorityCookie
  (title, tags, titleTxt) <- titleObjects
  planning <- option emptyPlanning planningInfo
  properties <- option mempty propertyDrawer
  contents <- elements
  children <- sections (level + 1)
  return
    OrgSectionData
      { level = level
      , properties = properties
      , todo = todoKw
      , comment = isComment
      , priority = priority
      , title = title
      , rawTitle = titleTxt
      , tags = tags
      , planning = planning
      , children = contents
      , subsections = children
      }
  where
    titleObjects :: OrgParser (OrgObjects, [Tag], Text)
    titleObjects =
      option mempty $
        withContext__
          (anySingle *> takeWhileP Nothing (\c -> not (isSpace c || c == ':')))
          endOfTitle
          (plainMarkupContext standardSet)

    endOfTitle :: OrgParser [Tag]
    endOfTitle = try $ do
      hspace
      tags <- option [] (headerTags <* hspace)
      newline'
      return tags

    headerTags :: OrgParser [Tag]
    headerTags = try $ do
      _ <- char ':'
      endBy1 orgTagWord (char ':')

-- | Parse a to-do keyword that is registered in the state.
todoKeyword :: OrgParser TodoKeyword
todoKeyword = try $ do
  taskStates <- getsO (.orgTodoKeywords)
  choice (map kwParser taskStates)
  where
    kwParser :: TodoKeyword -> OrgParser TodoKeyword
    kwParser tdm =
      -- NOTE to self: space placement - "TO" is subset of "TODOKEY"
      try (string tdm.name *> hspace1 $> tdm)

-- | Parse a priority cookie like @[#A]@.
priorityCookie :: OrgParser Priority
priorityCookie =
  try $
    string "[#"
      *> priorityFromChar
      <* char ']'
  where
    priorityFromChar :: OrgParser Priority
    priorityFromChar =
      NumericPriority
        <$> digitIntChar
          <|> LetterPriority
        <$> upperAscii

orgTagWord :: OrgParser Text
orgTagWord =
  takeWhile1P
    (Just "tag characters (alphanumeric, @, %, # or _)")
    (\c -> isAlphaNum c || c `elem` ['@', '%', '#', '_'])

-- | TODO READ ABOUT PLANNING
emptyPlanning :: PlanningInfo
emptyPlanning = PlanningInfo Nothing Nothing Nothing

-- | Parse a single planning-related and timestamped line.
planningInfo :: OrgParser PlanningInfo
planningInfo = try $ do
  updaters <- some planningDatum <* skipSpaces <* newline
  return $ foldr ($) emptyPlanning updaters
  where
    planningDatum =
      skipSpaces
        *> choice
          [ updateWith (\s p -> p {scheduled = Just s}) "SCHEDULED"
          , updateWith (\d p -> p {deadline = Just d}) "DEADLINE"
          , updateWith (\c p -> p {closed = Just c}) "CLOSED"
          ]
    updateWith fn cs = fn <$> (string cs *> char ':' *> skipSpaces *> parseTimestamp)

-- | Parse a :PROPERTIES: drawer and return the key/value pairs contained within.
propertyDrawer :: OrgParser Properties
propertyDrawer = try $ do
  _ <- skipSpaces
  _ <- string' ":properties:"
  _ <- skipSpaces
  _ <- newline
  fromList <$> manyTill nodeProperty (try endOfDrawer)
  where
    endOfDrawer :: OrgParser Text
    endOfDrawer =
      try $
        hspace
          *> string' ":end:"
          <* blankline'

    nodeProperty :: OrgParser (Text, Text)
    nodeProperty = try $ liftA2 (,) name value

    name :: OrgParser Text
    name =
      skipSpaces
        *> char ':'
        *> takeWhile1P (Just "node property name") (not . isSpace)
          <&> T.stripSuffix ":"
        >>= guardMaybe "expecting ':' at end of node property name"
          <&> T.toLower

    value :: OrgParser Text
    value =
      skipSpaces
        *> ( takeWhileP (Just "node property value") (/= '\n')
              <&> T.stripEnd
           )
        <* newline
