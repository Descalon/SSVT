module Lib where
    import Data.List
    import Data.Char
    import System.Random
    import Test.QuickCheck
    import Data.Map (Map)
    import qualified Data.Map as Map
    
    forall :: [a] -> (a -> Bool) -> Bool
    forall = flip all

    reversal :: Integer -> Integer
    reversal = read . reverse . show

    infix 1 --> 
    
    (-->) :: Bool -> Bool -> Bool
    p --> q = (not p) || q
    
    probs :: Int -> IO [Float]
    probs 0 = return []
    probs n = do
                 p <- getStdRandom random
                 ps <- probs (n-1) 
                 return (p:ps)
    
    data Shape = NoTriangle | Equilateral 
               | Isosceles  | Rectangular | Other deriving (Eq,Show)

    (.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    p .||. q = (\x -> p x || q x)
    
