{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Org.Walk
  ( module Org.Walk
  , MultiWalk
  , (.>)
  , (?>)
  )
where

import Control.MultiWalk
import Org.Types

data MWTag

type Walk = Control.MultiWalk.Walk MWTag Identity

type WalkM m = Control.MultiWalk.Walk MWTag m

type Query m = Control.MultiWalk.Query MWTag m

query :: (MultiWalk MWTag c, MultiWalk MWTag t, Monoid m) => (t -> m) -> c -> m
query = Control.MultiWalk.query @MWTag

walkM :: (MultiWalk MWTag c, MultiWalk MWTag t, Monad m) => (t -> m t) -> c -> m c
walkM = Control.MultiWalk.walkM @MWTag

walk :: (MultiWalk MWTag c, MultiWalk MWTag t) => (t -> t) -> c -> c
walk = Control.MultiWalk.walk @MWTag

buildMultiQ ::
  (MultiWalk MWTag a, Monoid m) =>
  (Org.Walk.Query m -> QList m (MultiTypes MWTag) -> QList m (MultiTypes MWTag)) ->
  a ->
  m
buildMultiQ = Control.MultiWalk.buildMultiQ @MWTag

buildMultiW ::
  (MultiWalk MWTag a, Applicative m) =>
  (Org.Walk.WalkM m -> FList m (MultiTypes MWTag) -> FList m (MultiTypes MWTag)) ->
  a ->
  m a
buildMultiW = Control.MultiWalk.buildMultiW @MWTag

instance MultiTag MWTag where
  type
    MultiTypes MWTag =
      '[ OrgDocument
       , OrgSection
       , -- Element stuff
         OrgElement
       , OrgElementData
       , ListItem
       , -- Object stuff
         OrgObject
       , Citation
       ]

type List a = Trav [] a

type DoubleList a = MatchWith [[a]] (Trav (Compose [] []) a)

instance MultiSub MWTag OrgDocument where
  type SubTypes MWTag OrgDocument = 'SpecList '[ToSpec (List OrgElement), ToSpec (List OrgSection)]

instance MultiSub MWTag OrgElement where
  type
    SubTypes MWTag OrgElement =
      'SpecList
        '[ ToSpec OrgElementData
         , ToSpec (Trav (Map Text) (Under KeywordValue 'NoSel (List OrgObject))) -- Objects under affiliated keywords
         ]

instance MultiSub MWTag OrgElementData where
  type
    SubTypes MWTag OrgElementData =
      'SpecList
        '[ ToSpec (List OrgElement)
         , ToSpec (List OrgObject)
         , ToSpec (Under KeywordValue 'NoSel (List OrgObject)) -- Objects under keywords
         , ToSpec (List ListItem)
         , ToSpec (List (Under TableRow 'NoSel (DoubleList OrgObject))) -- Objects under table rows
         , ToSpec (DoubleList OrgObject) -- Objects under verse blocks
         ]

instance MultiSub MWTag ListItem where
  type
    SubTypes MWTag ListItem =
      'SpecList
        '[ ToSpec (List OrgObject)
         , ToSpec (List OrgElement)
         ]

instance MultiSub MWTag OrgSection where
  type
    SubTypes MWTag OrgSection =
      'SpecList
        '[ ToSpec (List OrgObject)
         , ToSpec (List OrgElement)
         , ToSpec (List OrgSection)
         ]

instance MultiSub MWTag OrgObject where
  type
    SubTypes MWTag OrgObject =
      'SpecList
        '[ ToSpec (List OrgObject)
         , ToSpec (Under FootnoteRefData 'NoSel (List OrgElement))
         , ToSpec Citation
         ]

instance MultiSub MWTag Citation where
  type
    SubTypes MWTag Citation =
      'SpecList
        '[ ToSpec (List OrgObject)
         , ToSpec (List (Under CiteReference 'NoSel (List OrgObject)))
         ]
