-- BSD 3-Clause License
-- 
-- Copyright (c) 2024, Pieter Stevens
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
-- 
-- 3. Neither the name of the copyright holder nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, TemplateHaskell #-}

-- Struct.hs uses getField from GHC.Records, but defines its own HasField (and also setField)
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
          fieldP = map (\(n, _, _) -> let newN = mkName $ nameBase n in (newN, VarP newN)) fields
          fieldE = map (\(n, _, _) -> let newN = mkName $ nameBase n in if fieldName /= n then (newN, VarE newN) else (newN, VarE f)) fields

          -- If you are confused by this. Try runQ [e|34+35|] in GHCI
          -- The point is that this generates the following instance template for all fields:
          --
          -- > instance HasField "field" RecordType FieldType where
          -- >   hasField r = (\f -> case r of RecordType { .. } -> RecordType { field = f, .. }, r.field)
          --
          -- This way we inherit getField, but define our own setField
          decl = InstanceD Nothing [] (AppT (AppT (AppT (ConT $ mkName "HasField") (LitT $ StrTyLit $ nameBase fieldName)) $ ConT structType') fieldType)
            [FunD (mkName "hasField") [Clause [VarP r] (NormalB (TupE [Just (LamE [VarP f] (CaseE (VarE r) [Match (RecP structType' fieldP)
            (NormalB (RecConE structType' fieldE)) []])),Just (AppE (AppTypeE (VarE 'GHC.Records.getField) (LitT $ StrTyLit $ nameBase fieldName)) $ VarE r)])) []]]
