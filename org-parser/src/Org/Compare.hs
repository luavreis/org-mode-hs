{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{- | This module implements comparasion of Org contents, using multiwalk. You
 should use this instead of the default Ord instances when you want to compare
 Org content semantically.
-}
module Org.Compare (compareContent, compareContents, toAtoms) where

import Data.Text qualified as T
import Org.Types
import Org.Walk

data Atom
  = Separator
  | Word Text
  | Literal Text
  | Time DateTime
  deriving (Eq, Ord)

compareContent :: MultiWalk MWTag a => a -> a -> Ordering
compareContent = comparing toAtoms

compareContents :: MultiWalk MWTag a => [a] -> [a] -> Ordering
compareContents = comparing (foldMap toAtoms)

toAtoms :: MultiWalk MWTag a => a -> [Atom]
toAtoms = buildMultiQ \f l ->
  l ?> objToAtoms f ?> elmToAtoms f

elmToAtoms :: Query [Atom] -> OrgElement -> [Atom]
elmToAtoms f = (Separator :) . \case
  ExportBlock _ t -> [Literal t]
  ExampleBlock _ _ t -> [Literal $ srcLinesToText t]
  SrcBlock {..} -> [Literal $ srcLinesToText srcBlkLines]
  LaTeXEnvironment _ _ t -> [Literal t]
  x -> f x

objToAtoms :: Query [Atom] -> OrgObject -> [Atom]
objToAtoms f = \case
  Plain t -> Word <$> T.words t
  Code t -> [Literal t]
  Verbatim t -> [Literal t]
  Timestamp (TimestampData _ t) -> [Time t]
  Timestamp (TimestampRange _ t s) -> [Time t, Time s]
  Entity t -> [Literal t]
  LaTeXFragment _ t -> [Literal t]
  ExportSnippet _ t -> [Literal t]
  FootnoteRef {} -> []
  Cite (Citation {..}) ->
    (citationPrefix >>= toAtoms)
      ++ ( citationReferences >>= \CiteReference {..} ->
            (refPrefix >>= toAtoms)
              ++ [Literal refId]
              ++ (refSuffix >>= toAtoms)
         )
      ++ (citationSuffix >>= toAtoms)
  Src _ _ t -> [Literal t]
  x -> f x
