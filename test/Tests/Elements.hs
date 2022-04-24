-- |

module Tests.Elements where
import Tests.Helpers
import Org.Parser.Elements
import NeatInterpolation
import qualified Org.Builder as B

testElements :: TestTree
testElements = testGroup "Elements"
  [
    "Comment line" ~: commentLine $
    [ "# this is a comment" =?> mempty
    , "  # this is also a comment" =?> mempty
    , "#this line is not a comment" =!> ()
    ]

  , "Paragraph" ~: para $
    [
      [text|
         foobar
         baz
      |]
        =?> B.para mempty ("foobar" <> B.softbreak <> "baz"),

      [text|
         with /wrapped
         markup/ and markup *at end*
         =at start= but not~here~ and
         not _here_right.
      |]
        =?> B.para mempty ("with " <> B.italic "wrapped\nmarkup" <> " and markup "
                          <> B.bold "at end" <> B.softbreak <> B.verbatim "at start"
                          <> " but not~here~ and\nnot _here" <> B.subscript (B.plain "right")
                          <> B.plain ".")
    ]

  -- Add case for affiliated keywords with:
  --  + aff keyword at end of paragraph acting on next element
  --  + showing non leaking of aff kw from block to outside
  -- Add case showing preservation of state of end parser
  -- Add case showing preservation of state from inside to outside markupContext
  ]
