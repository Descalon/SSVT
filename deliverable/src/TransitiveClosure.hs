module TransitiveClosure where
    import Lib
    import Control.Monad.Fix
    import Data.List

    type Rel a = [(a,a)]

    infixr 5 @@

    (@@) :: Eq a => Rel a -> Rel a -> Rel a
    r @@ s =
        nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

    fp :: Eq a => (a -> a) -> a -> a
    fp f = fix (\g x -> if x == f x then x else g (f x))

    trClos :: (Ord a, Eq a) => Rel a -> Rel a
    trClos = sort . fp (\r -> nub $ (r @@ r) ++ r)