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
    [
      "# this is a comment" =?> mempty,

      "  # this is also a comment" =?> mempty,

      "#this line is not a comment" =!> ()
    ],

    "Paragraph" ~: para $
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
                          <> " but not~here~ and\nnot _here_right.")
    ]
  ]
