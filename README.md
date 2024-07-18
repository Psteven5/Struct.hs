# Struct.hs
C-struct like usage of records in Haskell, enabling shared fieldnames, dot access notation, and dot access updating.

While writing an Error class and inherited Error classes for my compiler, I came upon the dreaded Haskell namespace issue it has with fields, and ended up wishing it was kind of more like C in a few aspects. I then started using DuplicateRecordFields and a whole new series of problems started to emerge which led to this.

Disclaimer: This (for now) does not make the memory representation the same as a C-struct: that wasn't the goal.

Struct.hs defines a single Template Haskell function to be used: struct :: Name -> Q [Dec]

Calling "struct ''RecordType" after defining a Record generates the necessary instances for its fields to enable using it as a Struct.

```haskell
data Vec2 = Vec2 { x :: Double, y :: Double }
struct ''Vec2

data Point2D = Point2D { x :: Double, y :: Double }
struct ''Point2D

data Line2D = Line2D { start :: Point2D, end :: Point2D }
struct ''Line2D

main = do
  let v = Vec2    { x = 1, y = 2 }
  let p = Point2D { x = 3, y = 4 }
  print (v.x, v.y, p.x, p.y)

  let p' = p { y = 2 }
  let l  = Line2D { start = p', end = p }
  let l' = l { start.x = 1 }
  print (l'.start.x, l'.start.y, l'.end.x, l'.end.y)
```
```
(1.0,2.0,3.0,4.0)
(1.0,2.0,3.0,4.0)
```
