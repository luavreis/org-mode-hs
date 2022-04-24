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
      "<<this is a target>>" =?>
      B.target "0"

    , "<< not a target>>" =!> ()

    , "<<not a target >>" =!> ()

    , "<<this < is not a target>>" =!> ()

    , "<<this \n is not a target>>" =!> ()

    , "<<this > is not a target>>" =!> ()
    ]

  , "Subscripts and superscripts" ~: plainMarkupContext suscript $
    [
      "not a _suscript" =?>
      B.text "not a _suscript"

    , "not_{{suscript}" =?>
      B.text "not_{{suscript}"

    , "a_{balanced {suscript} ok}" =?>
      B.plain "a" <> B.subscript (B.text "balanced {suscript} ok")

    , "a^+strange,suscript," =?>
      B.plain "a" <> B.superscript (B.text "+strange,suscript") <> B.plain ","

    , "a^*suspicious suscript" =?>
      B.plain "a" <> B.superscript (B.plain "*") <> B.plain "suspicious suscript"

    , "a_bad,.,.,maleficent, one" =?>
      B.plain "a" <> B.subscript (B.plain "bad,.,.,maleficent") <> B.text ", one"

    , "a_some\\LaTeX" =?>
      B.plain "a" <> B.subscript (B.plain "some" <> B.fragment "\\LaTeX")
    ]
  ]
