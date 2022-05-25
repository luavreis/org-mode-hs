-- |

module Tests.Objects where
import Tests.Helpers
import Org.Parser.Objects
import Org.Types
import qualified Org.Builder as B

testObjects :: TestTree
testObjects = testGroup "Objects"
  [ "Timestamp" ~: timestamp $
    [
      "<1997-11-03 Mon 19:15>" =?>
      B.timestamp
        (TimestampData True ((1997,11,3, Just "Mon"), Just (19,15), Nothing, Nothing))

    , "[2020-03-04 20:20]" =?>
      B.timestamp
        (TimestampData False ((2020,03,04, Nothing), Just (20,20), Nothing, Nothing))

    , "[2020-03-04 0:20]" =?>
      B.timestamp
        (TimestampData False ((2020,03,04, Nothing), Just (0,20), Nothing, Nothing))
    ]

  , "Citations" ~: citation $
    [
      "[cite:/foo/;/bar/@bef=bof=;/baz/]" =?>
      let ref = CiteReference
            { refId = "bef"
            , refPrefix = [ Italic [ Plain "bar" ] ]
            , refSuffix = [ Verbatim "bof" ]
            }
      in B.citation
        Citation
          { citationStyle = ""
          , citationVariant = ""
          , citationPrefix = [ Italic [ Plain "foo" ] ]
          , citationSuffix = [ Italic [ Plain "baz" ] ]
          , citationReferences = [ref]
          }
    ]

  , "Targets" ~: target $
    [
      "<<this is a target>>" =?> B.target "0"

    , "<< not a target>>" =!> ()

    , "<<not a target >>" =!> ()

    , "<<this < is not a target>>" =!> ()

    , "<<this \n is not a target>>" =!> ()

    , "<<this > is not a target>>" =!> ()
    ]

  , "TeX Math Fragments" ~: plainMarkupContext texMathFragment $
    [
      "$e = mc^2$" =?> B.inlMath "e = mc^2"

    , "$$foo bar$" =?> "$$foo bar$"

    , "$foo bar$a" =?> "$foo bar$a"

    , "($foo bar$)" =?> "(" <> B.inlMath "foo bar" <> ")"

    , "This is $1 buck, not math ($1! so cheap!)" =?> "This is $1 buck, not math ($1! so cheap!)"

    , "two$$always means$$math" =?> "two" <> B.dispMath "always means" <> "math"
    ]

  , "Subscripts and superscripts" ~: plainMarkupContext suscript $
    [
      "not a _suscript" =?> "not a _suscript"

    , "not_{{suscript}" =?> "not_{{suscript}"

    , "a_{balanced {suscript} ok}" =?> "a" <> B.subscript "balanced {suscript} ok"

    , "a^+strange,suscript," =?> "a" <> B.superscript "+strange,suscript" <> ","

    , "a^*suspicious suscript" =?> "a" <> B.superscript "*" <> "suspicious suscript"

    , "a_bad,.,.,maleficent, one" =?> "a" <> B.subscript "bad,.,.,maleficent" <> ", one"

    , "a_some\\LaTeX" =?> "a" <> B.subscript ("some" <> B.fragment "\\LaTeX")
    ]
  ]
