module Tests.Objects where

import Org.Parser.Objects
import Tests.Helpers

testObjects :: TestTree
testObjects =
  goldenGroup
    "objects"
    [ "timestamp" ~: timestamp $
        [ "<1997-11-03 Mon 19:15>"
        , "[2020-03-04 20:20]"
        , "[2020-03-04 0:20]"
        ]
    , "citations" ~: citation $
        [ "[cite:/foo/;/bar/@bef=bof=;/baz/]"
        ]
    , "targets" ~: target $
        [ "<<this is a target>>"
        , "<< not a target>>"
        , "<<not a target >>"
        , "<<this < is not a target>>"
        , "<<this \n is not a target>>"
        , "<<this > is not a target>>"
        ]
    , "math fragment" ~: latexFragment $
        [ "\\(\\LaTeX + 2\\)"
        , "\\[\\LaTeX + 2\\]"
        ]
    , "tex math fragments" ~: plainMarkupContext (mapMarkedP withPos texMathFragment) $
        [ "$e = mc^2$"
        , "$$foo bar$"
        , "$foo bar$a"
        , "($foo bar$)"
        , "This is $1 buck, not math ($1! so cheap!)"
        , "two$$always means$$math"
        ]
    , "subscripts and superscripts" ~: plainMarkupContext (mapMarkedP withPos suscript) $
        [ "not a _suscript"
        , "not_{{suscript}"
        , "a_{balanced^{crazy} ok}"
        , "a_{balanced {suscript} ok}"
        , "a_{bala\nnced {sus\ncript} ok}"
        , "a^+strange,suscript,"
        , "a^*suspicious suscript"
        , "a_bad,.,.,maleficent, one"
        , "a_some\\LaTeX"
        ]
    , "line breaks" ~: plainMarkupContext (mapMarkedP withPos linebreak) $
        [ "this is a \\\\  \t\n\
          \line break"
        , "also linebreak \\\\"
        ]
    , "image or links" ~: regularLink $
        [ "[[http://blablebli.com]]"
        , "[[http://blablebli.com][/uh/ duh! *foo*]]"
        ]
    , "statistic cookies" ~: statisticCookie $
        [ "[13/18]"
        , "[33%]"
        ]
    , "footnote references" ~: footnoteReference $
        [ "[fn::simple]"
        , "[fn::s[imple]"
        , "[fn:mydef:s[imp]le]"
        ]
    , "macros" ~: macro $
        [ "{{{fooo()}}}"
        , "{{{função()}}}"
        , "{{{2fun()}}}"
        , "{{{fun-2_3(bar,(bar,baz){a})}}}"
        ]
    , "italic" ~: italic $
        [ "// foo/"
        , "/foo //"
        , "/foo / f/"
        ]
    ]
