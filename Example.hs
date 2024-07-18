-- These language extensions are required in a module using Struct.hs
--  * DataKinds for referring to the fields as their string representation
--  * DuplicateRecordFields IFF you want to allow.. duplicate record fields
--  * OverloadedRecordDot for accessing the fields with . -> kind of falls apart without it
--  * OverloadedRecordUpdate for being able to update records
--  * RebindableSyntax because OverloadedRecordUpdate and idk GHC told me to
--  * TemplateHaskell for being able to execute the compile time generation of instances
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}

module Example (main) where

import Struct

-- First declare your Record ...
data Vec2 = Vec2
  { x :: Double
  , y :: Double
  } deriving Show

-- ... Then declare your struct!
struct ''Vec2

data Vec3 = Vec3
  { x :: Double
  , y :: Double
  , z :: Double
  } deriving Show
struct ''Vec3

data Mat2x2 = Mat2x2
  { x :: Vec2
  , y :: Vec2
  } deriving Show
struct ''Mat2x2

-- Doing so will enable:
--  * Unambiguous field names (Haskell won't be confused about Vec2.x vs Vec3.x)
--  * Dot access notation
--  * Nested dot access AND nested dot update notation

main = do
  -- Define a Struct
  let u = Vec2 { x = 1, y = 2 }

  -- Update a struct
  let v = u { x = 2, y = 3 }
  
  -- This also still works
  let s = Vec3 3 4 5
  
  let t = s { x = 5, z = 7 }

  -- The fields of a Mat2x2 are Vec2's, but Haskell remains unconfused!
  let m = Mat2x2 { x = u, y = v }

  -- Nested dot access updating
  let n = m { x.x = 4 }

  -- Dot accessing and nested dot accessing
  print (v.x, v.y)
  print (t.x, t.y, t.z)
  print (m.x, m.y)
  print (n.x.x, n.x.y, n.y.x, n.y.y)
