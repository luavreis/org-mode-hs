module Org.Exporters.Citeproc where
import Org.Types
import Citeproc.CslJson
import qualified Data.Text as T

toCslJson :: [OrgInline] -> CslJson Text
toCslJson [] = CslEmpty
toCslJson (x : xs) = to x <> toCslJson xs
  where
    to (Plain t) = CslText t
    to SoftBreak = CslText " "
    to LineBreak = CslText " "
    to (NBSpace n) = CslText $ T.replicate n " "
    to (Italic o) = CslItalic $ toCslJson o
    to (Underline o) = CslUnderline $ toCslJson o
    to (Bold o) = CslBold $ toCslJson o
    to (Superscript o) = CslSup $ toCslJson o
    to (Subscript o) = CslSub $ toCslJson o
    to (Strikethrough o) = CslDiv "striketrough" $ toCslJson o
    to (Quoted _ o) = CslQuoted $ toCslJson o
    to (Verbatim o) = CslNoDecoration $ CslText o
    to (Code o) = CslNoDecoration $ CslText o
    to (Timestamp _) = CslEmpty
