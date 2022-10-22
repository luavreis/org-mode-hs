{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Org.Walk where

import Control.HasSub
import Control.MultiWalk
import Org.Types

data MWTag

instance MultiTag MWTag where
  type
    MultiTypes MWTag =
      '[ OrgDocument,
         OrgSection,
         -- Element stuff
         OrgElement,
         [ListItem],
         -- Object stuff
         OrgObject,
         Citation
       ]

type DoubleList a = MatchWith [[a]] (Compose [] [])

instance MultiWalk MWTag OrgDocument where
  type SubTypes OrgDocument = '[OrgElement, OrgSection]

instance MultiWalk MWTag OrgElement where
  type
    SubTypes OrgElement =
      '[ OrgElement,
         OrgObject,
         OrgObject, -- Objects under affiliated keywords
         OrgObject, -- Objects under regular keywords
         [ListItem],
         OrgObject, -- Objects under table rows
         OrgObject -- Objects under verse blocks
       ]
  type
    Containers OrgElement =
      '[ [],
         [],
         Under (Map KeywordKey) KeywordValue [],
         Under Identity KeywordValue [],
         Identity,
         Under [] TableRow (DoubleList OrgObject),
         DoubleList OrgObject
       ]

instance MultiWalk MWTag [ListItem] where
  type SubTypes [ListItem] = '[OrgObject, OrgElement]
  type
    Containers [ListItem] =
      '[ Under [] ListItem [],
         Under [] ListItem []
       ]

instance MultiWalk MWTag OrgSection where
  type SubTypes OrgSection = '[OrgObject, OrgElement, OrgSection]

instance MultiWalk MWTag OrgObject where
  type
    SubTypes OrgObject =
      '[ OrgObject,
         OrgElement,
         Citation
       ]
  type
    Containers OrgObject =
      '[ [],
         Under Identity FootnoteRefData [],
         Identity
       ]

instance MultiWalk MWTag Citation where
  type SubTypes Citation = '[OrgObject, OrgObject]
  type Containers Citation = '[[], Under [] CiteReference []]
