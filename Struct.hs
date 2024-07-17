{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, TemplateHaskell #-}

-- Struct.hs used getField from GHC.Records, but defines its own HasField (and thus setField)
-- DuplicateRecordFields is going to be enabled, so we need to import Prelude
module Struct
  ( module Struct
  , module GHC.Records
  , module Prelude
  ) where

import GHC.Records hiding (HasField, setField)
import Language.Haskell.TH
import Prelude

-- We want do define our own HasField, as we don't want to include setField, only getField
class HasField x r a | x r -> a where
  hasField :: r -> (a -> r, a)
setField :: forall x r a . HasField x r a => r -> a -> r
setField = fst . hasField @x

-- Generates all the fields for a given Record (Create a Struct based on a Record)
struct :: Name -> Q [Dec]
struct structType = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify structType
  concat <$> mapM (field fields structType) fields
  where
    -- Helper function that generates an instance of hasField for a given field of the Record
    field :: [VarBangType] -> Name -> VarBangType -> Q [Dec]
    field fields structType (fieldName, _, fieldType) = pure [decl]
      where
        structType' = mkName $ nameBase structType
        r = mkName "r"
        f = mkName "f"
        fieldP = map (\(n, _, _) -> (mkName $ nameBase n, VarP $ mkName $ nameBase n)) fields
        fieldE = map (\(n, _, _) -> if fieldName /= n then (mkName $ nameBase n, VarE $ mkName $ nameBase n) else (mkName $ nameBase n, VarE f)) fields

        -- *
        -- If you are confused by this. Try runQ [e|34+35|] in GHCI
        -- The point is that this generates the following instance template for all fields:
        --
        -- > instance HasField "field" RecordType FieldType where
        -- >   hasField r = (\f -> case r of RecordType { .. } -> RecordType { field = f, .. }, r.field)
        --
        -- This way we inherit getField, but define our own setField
        -- *
        decl = InstanceD Nothing [] (AppT (AppT (AppT (ConT $ mkName "HasField") (LitT (StrTyLit (nameBase fieldName)))) (ConT structType')) fieldType) [FunD (mkName "hasField") [Clause [VarP r] (NormalB (TupE [Just (LamE [VarP f] (CaseE (VarE r) [Match (RecP structType' fieldP) (NormalB (RecConE structType' fieldE)) []])),Just (AppE (AppTypeE (VarE 'GHC.Records.getField) (LitT (StrTyLit (nameBase fieldName)))) (VarE r))])) []]]
