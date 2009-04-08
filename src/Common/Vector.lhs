> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
>
> module Common.Vector where
>
> import Control.Applicative
> import FRP.Elerea
> import Graphics.Rendering.OpenGL
>
> data Vec = V !Float !Float
>
> infixl 7 ^*.
> infixl 7 .*^
> infixl 7 `dot`
> infixl 7 `cross`
> infixl 6 ^+^
> infixl 6 ^-^
>
> class Vector2D v c | v -> c where
>     (^+^) :: v -> v -> v
>     (^-^) :: v -> v -> v
>     (^*.) :: v -> c -> v
>     (.*^) :: c -> v -> v
>     vnull :: v
>     dot :: v -> v -> c
>     cross :: v -> v -> c
>
> instance Vector2D Vec Float where
>     V x1 y1 ^+^ V x2 y2 = V (x1+x2) (y1+y2)
>     V x1 y1 ^-^ V x2 y2 = V (x1-x2) (y1-y2)
>     V x y ^*. t = V (x*t) (y*t)
>     t .*^ V x y = V (x*t) (y*t)
>     vnull = V 0 0
>     V x1 y1 `dot` V x2 y2 = x1*y1+x2*y2
>     V x1 y1 `cross` V x2 y2 = x1*y2-x2*y1
>
> instance Vector2D (Signal Vec) (Signal Float) where
>     (^+^) = liftA2 (^+^)
>     (^-^) = liftA2 (^-^)
>     (^*.) = liftA2 (^*.)
>     (.*^) = liftA2 (.*^)
>     vnull = pure vnull
>     dot = liftA2 dot
>     cross = liftA2 cross
