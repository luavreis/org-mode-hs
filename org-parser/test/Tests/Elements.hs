module Tests.Elements where

import NeatInterpolation
import Org.Builder qualified as B
import Org.Parser.Elements
import Org.Types
import Tests.Helpers

testElements :: TestTree
testElements =
  testGroup
    "Elements"
    [ "Comment line" ~: commentLine $
        [ "# this is a comment" =?> mempty,
          "  # this is also a comment" =?> mempty,
          "#this line is not a comment" =!> ()
        ],
      "Paragraph" ~: para $
        [ --
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
            =?> B.para
              mempty
              ( "with " <> B.italic "wrapped\nmarkup" <> " and markup "
                  <> B.bold "at end"
                  <> B.softbreak
                  <> B.verbatim "at start"
                  <> " but not~here~ and\nnot _here"
                  <> B.subscript (B.plain "right")
                  <> B.plain "."
              )
        ],
      "Affiliated Keywords in Context" ~: elements $
        [ --
          [text|
            #+attr_html: :width 40px :foo bar:joined space :liz buuz
            Hi
          |]
            =?> let kw =
                      BackendKeyword
                        [ ("width", "40px"),
                          ("foo", "bar:joined space"),
                          ("liz", "buuz")
                        ]
                 in B.para (fromList [("attr_html", kw)]) "Hi",
          [text|
            Some para
            #+caption: hi /guys/

            Hi
          |]
            =?> B.para mempty "Some para"
              <> B.para mempty "Hi",
          [text|
            #+attr_html: :style color: red
              - foo
          |]
            =?> let kw = BackendKeyword [("style", "color: red")]
                 in B.list
                    (fromList [("attr_html", kw)])
                    (Unordered '-')
                    [ListItem (Bullet '-') Nothing Nothing [] [Paragraph mempty [Plain "foo"]]],
          [text|
            Some para
            #+caption: hi /guys/
            Hi
          |]
            =?> let kw = ParsedKeyword [] (toList $ "hi " <> B.italic "guys")
                 in B.para mempty "Some para"
                      <> B.para (fromList [("caption", kw)]) "Hi",
          [text|
            #+attr_org: :foo bar
            #+begin_center
            Some para
            #+caption: hi /guys/
            #+end_center
            I don't have a caption
          |]
            =?> B.greaterBlock
              (fromList [("attr_org", BackendKeyword [("foo", "bar")])])
              Center
              (B.para mempty "Some para")
            <> B.para mempty "I don't have a caption"
        ],
      -- Add case for affiliated keywords with:
      -- Add case showing preservation of state of end parser
      -- Add case showing preservation of state from inside to outside markupContext

      "Descriptive Lists" ~: plainList $
        [ "- foo :: bar"
            =?> B.list
              mempty
              Descriptive
              [ListItem (Bullet '-') Nothing Nothing (toList $ B.plain "foo") [Paragraph mempty (toList $ B.plain "bar")]],
          "- /foo/ :: bar"
            =?> B.list
              mempty
              Descriptive
              [ListItem (Bullet '-') Nothing Nothing (toList $ B.italic "foo") [Paragraph mempty (toList $ B.plain "bar")]],
          "- [[foo][bar]] :: bar"
            =?> B.list
              mempty
              Descriptive
              [ListItem (Bullet '-') Nothing Nothing (toList $ B.link (UnresolvedLink "foo") "bar") [Paragraph mempty (toList $ B.plain "bar")]]
        ],
      "Greater Blocks" ~: greaterBlock $
        [ --
          ( unlines
              [ "#+begin_fun",
                "    ",
                "#+end_fun"
              ]
          )
            =?> B.greaterBlock
              mempty
              (Special "fun")
              mempty
        ],
      "Tricky" ~: elements $
        [ "\n    " =?> mempty
        ]
    ]
