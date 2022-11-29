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
              ( "with "
                  <> B.italic "wrapped\nmarkup"
                  <> " and markup "
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
      "Ordered Lists in context" ~: elements $
        [ --
          unlines
            [ "",
              " 1. our",
              " 2. moment's",
              " 3. else's"
            ]
            =?> B.orderedList
              mempty
              OrderedNum
              '.'
              [ B.para mempty "our",
                B.para mempty "moment's",
                B.para mempty "else's"
              ]
        ],
      "Ordered Lists" ~: plainList $
        [ --
          unlines
            [ " 1. our",
              " 2. moment's",
              " 3. else's"
            ]
            =?> B.orderedList
              mempty
              OrderedNum
              '.'
              [ B.para mempty "our",
                B.para mempty "moment's",
                B.para mempty "else's"
              ]
        ],
      "Descriptive Lists" ~: plainList $
        [ "- foo ::   bar"
            =?> B.descriptiveList mempty [("foo", B.para mempty "bar")],
          "- foo bar  :: baz"
            =?> B.descriptiveList mempty [("foo bar", B.para mempty "baz")],
          "- /foo/ :: bar"
            =?> B.descriptiveList mempty [(B.italic "foo", B.para mempty "bar")],
          "- [[foo][bar]] :: bar"
            =?> B.descriptiveList mempty [(B.link (UnresolvedLink "foo") "bar", B.para mempty "bar")],
          "- [[foo:prot.co][bar baz]] :: bla :: ble"
            =?> B.descriptiveList mempty [(B.link (UnresolvedLink "foo:prot.co") "bar baz", B.para mempty "bla :: ble")]
        ],
      "Greater Blocks" ~: greaterBlock $
        [ --
          unlines
            [ "#+begin_fun",
              "    ",
              "#+end_fun"
            ]
            =?> B.greaterBlock
              mempty
              (Special "fun")
              mempty
        ],
      "Fixed width" ~: fixedWidth $
        [ --
          [text|
                :   fooblabla boo
             :  foooo
                  :       booo
            |]
            =?> B.example
              mempty
              mempty
              [ SrcLine " fooblabla boo",
                SrcLine "foooo",
                SrcLine "     booo"
              ]
        ],
      "Horizontal Rules" ~: horizontalRule $
        [ "    ----------------   "
            =?> B.horizontalRule,
          "    --   "
            =!> ()
        ],
      "Tables" ~: table $
        [ --
          [text|
             | foo | bar | baz |
                |foo bar | baz
            |----
            |<r>  | | <l>|<c>
            | <r> | foo /bar/ | *ba* | baz
            | foo || bar | |
          |]
            =?> B.table
              mempty
              [ B.standardRow ["foo", "bar", "baz"],
                B.standardRow ["foo bar", "baz"],
                RuleRow,
                ColumnPropsRow [Just AlignRight, Nothing, Just AlignLeft, Just AlignCenter],
                B.standardRow ["<r>", "foo " <> B.italic "bar", B.bold "ba", "baz"],
                B.standardRow ["foo", mempty, "bar", mempty]
              ]
        ],
      "Tricky whitespace" ~: elements $
        [ "\n    " =?> mempty,
          "" =?> mempty,
          "\n" =?> mempty,
          "\n\n a" =?> B.para mempty "a"
        ]
    ]
