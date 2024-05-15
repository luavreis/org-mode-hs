{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Org.Types.Variants.Plain
  ( OrgObjects
  , OrgElements
  , OrgSections
  , OrgDocument

    -- * Constructors and patterns
  , element

    -- * Re-exports
  , module Org.Types.Variants.Basic
  )
where

import Org.Types.Variants.Basic

type OrgPlain = Org (Compose [])

type OrgObjects = OrgPlain ObjIx
type OrgElements = OrgPlain ElmIx
type OrgSections = OrgPlain SecIx
type OrgDocument = OrgDocumentData OrgPlain ElmIx

element :: [(Text, KeywordValue (k ObjIx))] -> OrgElementData k ElmIx -> OrgF k ElmIx
element aff = OrgElementF (fromList aff)
