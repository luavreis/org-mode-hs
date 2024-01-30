{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

{- | This module implements semantic comparasion of Org contents. You should use
 this instead of the default Ord instances when you want to order Org content
 semantically.
-}
module Org.Compare (compareContent, compareContents, toAtoms, Atom) where

import Control.Category.Natural (type (~>) (..))
import Data.Ix.Foldable (ifold)
import Data.Ix.RecursionSchemes qualified as R
import Data.Text qualified as T
import Org.Types.Data.Element (OrgElementData (..), srcLinesToText)
import Org.Types.Data.Object (OrgObjectData (..))
import Org.Types.Data.Timestamp (OrgDateTime, TimestampData (..))
import Org.Types.Ix
import Org.Types.Variants.Plain (Org, OrgF (..))

type Res = Endo [Atom]

runRes :: Const Res (ix :: OrgIx) -> [Atom]
runRes r = appEndo (getConst r) []

tellOne :: Atom -> Res
tellOne = Endo . (:)

tellMany :: [Atom] -> Res
tellMany = Endo . (++)

data Atom
  = Separator
  | Word Text
  | Literal Text
  | Time OrgDateTime
  deriving (Eq, Ord)

compareContent :: Org ix -> Org ix -> Ordering
compareContent = comparing toAtoms

compareContents :: [Org ix] -> [Org ix] -> Ordering
compareContents = comparing (foldMap toAtoms)

toAtoms :: forall ix. Org ix -> [Atom]
toAtoms x = runRes $ c # x
  where
    c = R.fold $ NT \(ComposeIx l) ->
      (Const . (`foldMap` l)) \case
        OrgObjectF d -> objToAtoms d
        OrgElementF _k d -> elmToAtoms d
        OrgSectionF d -> ifold d

elmToAtoms :: OrgElementData (Const Res) ix -> Res
elmToAtoms el = do
  tellOne Separator
    <> case el of
      ExportBlock _ t -> tellOne $ Literal t
      ExampleBlock _ t -> tellOne $ Literal $ srcLinesToText t
      SrcBlock {} -> tellOne $ Literal $ srcLinesToText el.srcLines
      LaTeXEnvironment _ t -> tellOne $ Literal t
      x -> ifold x

objToAtoms :: OrgObjectData (Const Res) ix -> Res
objToAtoms = \case
  Plain t -> tellMany (Word <$> T.words t)
  Code t -> tellOne $ Literal t
  Verbatim t -> tellOne $ Literal t
  Timestamp ts@TimestampData {} -> tellOne $ Time ts.time
  Timestamp ts@TimestampRange {} -> tellMany [Time ts.start, Time ts.end]
  Entity t -> tellOne $ Literal t
  LaTeXFragment _ t -> tellOne $ Literal t
  ExportSnippet _ t -> tellOne $ Literal t
  Src _ _ t -> tellOne $ Literal t
  x -> ifold x
