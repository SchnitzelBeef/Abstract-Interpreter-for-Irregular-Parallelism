module DEP.AST
  ( VName
  , Exp (..)
  )
where

type VName = String

{-# LANGUAGE Strict #-}

-- | The Futhark source language AST definition.  Many types, such as
-- 'ExpBase', are parametrised by type and name representation.
-- E.g. in a value of type @ExpBase f vn@, annotations are wrapped in
-- the functor @f@, and all names are of type @vn@.  See
-- https://futhark.readthedocs.org for a language reference, or this
-- module may be a little hard to understand.
--
-- The system of primitive types is interesting in itself.  See
-- "Language.Futhark.Primitive".
module Language.Futhark.Syntax
  ( module Language.Futhark.Core,
    prettyString,
    prettyText,

    -- * Types
    Uniqueness (..),
    IntType (..),
    FloatType (..),
    PrimType (..),
    Size,
    Shape (..),
    shapeRank,
    stripDims,
    TypeBase (..),
    TypeArg (..),
    SizeExp (..),
    TypeExp (..),
    TypeArgExp (..),
    PName (..),
    ScalarTypeBase (..),
    RetTypeBase (..),
    StructType,
    ParamType,
    ResType,
    StructRetType,
    ResRetType,
    ValueType,
    Diet (..),

    -- * Values
    IntValue (..),
    FloatValue (..),
    PrimValue (..),
    IsPrimValue (..),

    -- * Abstract syntax tree
    AttrInfo (..),
    AttrAtom (..),
    BinOp (..),
    IdentBase (..),
    Inclusiveness (..),
    DimIndexBase (..),
    SliceBase,
    SizeBinder (..),
    AppExpBase (..),
    AppRes (..),
    ExpBase (..),
    FieldBase (..),
    CaseBase (..),
    LoopInitBase (..),
    LoopFormBase (..),
    PatLit (..),
    PatBase (..),

    -- * Module language
    ImportName (..),
    SpecBase (..),
    ModTypeExpBase (..),
    TypeRefBase (..),
    ModTypeBindBase (..),
    ModExpBase (..),
    ModBindBase (..),
    ModParamBase (..),

    -- * Definitions
    DocComment (..),
    ValBindBase (..),
    EntryPoint (..),
    EntryType (..),
    EntryParam (..),
    Liftedness (..),
    TypeBindBase (..),
    TypeParamBase (..),
    typeParamName,
    ProgBase (..),
    DecBase (..),

    -- * Miscellaneous
    L (..),
    NoInfo (..),
    Info (..),
    QualName (..),
    mkApply,
    mkApplyUT,
    sizeFromName,
    sizeFromInteger,
    loopInitExp,
  )
where
