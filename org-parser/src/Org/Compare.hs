{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

{- | This module implements semantic comparasion of Org contents. You should use
 this instead of the default Ord instances when you want to order Org content
 semantically.
-}
module Org.Compare (compareContents, toAtoms, Atom) where

import Data.Ix.Foldable (IFoldable (..))
import Data.Ix.Functor (IFunctor)
import Data.Text qualified as T
import Org.Types.Variants.Plain
import Org.Types.Walk (queryTopDown)

type Res = Endo [Atom]

tellOne :: Atom -> Res
tellOne = Endo . (:)

tellMany :: [Atom] -> Res
tellMany = Endo . (++)

data Atom
  = Separator
  | Word !Text
  | Literal !Text
  | Time !OrgDateTime
  deriving (Eq, Ord)

compareContents :: (IFunctor f, IFoldable f) => Org f ix -> Org f ix -> Ordering
compareContents = comparing toAtoms

toAtoms :: (IFunctor f, IFoldable f) => Org f ix -> [Atom]
toAtoms x = appEndo (queryTopDown f x) []
  where
    f :: OrgF k jx -> Res
    f = \case
      OrgObjectF d -> objToAtoms d
      OrgElementF _k d -> elmToAtoms d
      OrgSectionF {} -> mempty

elmToAtoms :: OrgElementData k ix -> Res
elmToAtoms el = do
  tellOne Separator
    <> case el of
      ExportBlock _ t -> tellOne $ Literal t
      ExampleBlock _ t -> tellOne $ Literal $ srcLinesToText t
      SrcBlock {} -> tellOne $ Literal $ srcLinesToText el.srcLines
      LaTeXEnvironment _ t -> tellOne $ Literal t
      _other -> mempty

objToAtoms :: OrgObjectData k ix -> Res
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
  _other -> mempty
