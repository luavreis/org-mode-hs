-- |

module Tests.Elements where
import Org.Types
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

    , "Affiliated Keywords in Context" ~: elements $
    [
      [text|
         #+attr_html: :width 40px :foo bar:joined space :liz buuz
         Hi
      |]
        =?>
        let kw = BackendKeyword [ ("width", "40px")
                                , ("foo", "bar:joined space")
                                , ("liz", "buuz")]
        in B.para (fromList [("attr_html", kw)]) "Hi"

    , [text|
         Some para
         #+caption: hi /guys/
         Hi
      |]
        =?>
        let kw = ParsedKeyword [] (toList $ "hi " <> B.italic "guys")
        in B.para mempty "Some para"
        <> B.para (fromList [("caption", kw)]) "Hi"

    , [text|
         #+attr_org: :foo bar
         #+begin_center
         Some para
         #+caption: hi /guys/
         #+end_center
         I don't have a caption
      |]
        =?>
        B.greaterBlock (fromList [("attr_org", BackendKeyword [("foo", "bar")])]) Center
          (B.para mempty "Some para")
        <> B.para mempty "I don't have a caption"
    ]


  -- Add case for affiliated keywords with:
  -- Add case showing preservation of state of end parser
  -- Add case showing preservation of state from inside to outside markupContext
  ]
