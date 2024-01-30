module Tests.Elements where

import NeatInterpolation
import Org.Parser.Elements
import Tests.Helpers

testElements :: TestTree
testElements =
  goldenGroup
    "elements"
    [ "clock" ~: clock $
        [ "CLOCK: [2012-11-18 Sun 19:26]--[2012-11-18 Sun 19:33] =>  0:07\n"
        ]
    , "clocks in context" ~: elements $
        [ [text|
            foo
            CLOCK: [2012-11-18 Sun 19:26]--[2012-11-18 Sun 19:33] =>  0:07
            bar
          |]
        ]
    , "comment line" ~: commentLine $
        [ "# this is a comment"
        , "#this line is not a comment"
        ]
    , "paragraph" ~: elements $
        [ --
          [text|
            foobar
            baz
          |]
        , [text|
            with /wrapped
            markup/ and markup *at end*
            =at start= but not~here~ and
            not _here_right.
          |]
        ]
    , "affiliated keywords in context" ~: elements $
        [ --
          [text|
            #+attr_html: :width 40px :foo bar:joined space :liz buuz
            Hi
          |]
        , [text|
            Some para
            #+caption: hi /guys/

            Hi
          |]
        , [text|
            #+attr_html: :style color: red
              - foo
          |]
        , [text|
            Some para
            #+caption: hi /guys/
            Hi
          |]
        , [text|
            #+attr_org: :foo bar
            #+begin_center
            Some para
            #+caption: hi /guys/
            #+end_center
            I don't have a caption
          |]
        ]
    , "ordered lists" ~: plainList $
        [ --
          unlines
            [ "1. our"
            , "2. moment's"
            , "3. else's"
            ]
        ]
    , "descriptive lists" ~: plainList $
        [ "- foo ::   bar"
        , "- foo bar  :: baz"
        , "-   :: ::"
        , "-   :: foo ::"
        , "-   :: :: bar"
        , "-  ::  :::"
        , "- /foo/ :: bar"
        , "- [[foo][bar]] :: bar"
        , "- [[foo:prot.co][bar baz]] :: bla :: ble"
        ]
    , "lists in context" ~: elements $
        [ --
          unlines
            [ "- foo bar"
            , ""
            , "  #+caption: foo"
            , "bla"
            ]
        , unlines
            [ "- foo bar"
            , "#+caption: foo"
            , "  bla"
            ]
        , unlines
            [ "- "
            , " * "
            , " - foo"
            , " -"
            , " + "
            , "+"
            ]
        , unlines
            [ "- "
            , ""
            , "- foo"
            , "  "
            , "  "
            , " * bar"
            , " *"
            , ""
            , ""
            , " - doo"
            ]
        , unlines
            [ " "
            , " 1. our"
            , " 2. moment's"
            , " 3. else's"
            ]
        ]
    , "greater blocks" ~: greaterBlock $
        [ --
          unlines
            [ "#+begin_fun"
            , "    "
            , "#+end_fun"
            ]
        ]
    , "fixed width" ~: fixedWidth $
        [ --
          [text|
                :   fooblabla boo
             :  foooo
                  :       booo
            |]
        ]
    , "horizontal rules" ~: horizontalRule $
        [ "----------------   "
        , "--   "
        ]
    , "tables" ~: table $
        [ --
          [text|
             | foo | bar | baz |
                |foo bar | baz
            |----
            |<r>  | | <l>|<c>
            | <r> | foo /bar/ | *ba* | baz
            | foo || bar | |
          |]
        ]
    , "tricky whitespace" ~: elements $
        [ "\n    "
        , ""
        , "\n"
        , "\n\n a"
        ]
    ]
