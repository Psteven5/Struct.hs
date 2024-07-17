# Struct.hs
C-struct like usage of records in Haskell, enabling shared fieldnames, dot access notation, and dot access updating

```haskell
data Vec2 = Vec2 { x :: Double, y :: Double }
$(struct ''Vec2)

data Point2D = Point2D { x :: Double, y :: Double }
$(struct ''Point2D)

data Line2D = Line2D { start :: Point2D, end :: Point2D }
$(struct ''Line2D)

main = do
  let v = Vec2    { x = 1, y = 2 }
  let p = Point2D { x = 3, y = 4 }
  print (v.x, v.y, p.x, p.y)

  let p' = p { y = 2 }
  let l  = Line2D { start = p', end = p }
  let l' = l { start.x = 1 }
  print (l.start.x, l.start.y, l.end.x, l.end.y)
```
