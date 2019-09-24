module Cnf where
    import Control.Monad
    import Lib
    import SetOrd
    import Propositions
    import Test.QuickCheck
    import Generator

    has :: (Eq a) => [a] -> a -> Bool
    has = flip elem

    unique :: (Eq a) => [a] -> [a]
    unique [] = []
    unique (x:xs)
        | has xs x = unique xs
        | otherwise = x : unique xs

    flattenDsj :: [Form] -> [Form]
    flattenDsj = foldr(\x s -> case x of 
                                    (Dsj forms) -> forms ++ s
                                    (Cnj forms) -> [Cnj (unique $ flattenCnj forms)] ++ s
                                    otherwise -> x : s) []

    flattenCnj :: [Form] -> [Form]
    flattenCnj = foldr(\x s -> case x of 
                                    (Cnj forms) -> forms ++ s
                                    (Dsj forms) -> [Dsj (unique $ flattenDsj forms)] ++ s
                                    otherwise -> x : s) []

    flatten :: Form -> Form
    flatten (Dsj forms) = Dsj $ unique . flattenDsj $ forms
    flatten (Cnj forms) = Cnj $ unique . flattenCnj $ forms
    flatten x = x

    f = Dsj [p, Cnj[q,r]]

    getCnjProps :: Form -> [Form]
    getCnjProps (Prop a) = [Prop a]
    getCnjProps (Neg (Prop a)) = [Neg (Prop a)]
    getCnjProps (Cnj forms) = foldr (\x s -> (getCnjProps x) ++ s) [] forms
    getCnjProps _ = []

    getDsjProps :: Form -> [Form]
    getDsjProps (Prop a) = [Prop a]
    getDsjProps (Neg (Prop a)) = [Neg (Prop a)]
    getDsjProps (Dsj forms) = foldr (\x s -> (getDsjProps x) ++ s) [] forms
    getDsjProps _ = []

    distribution :: [Form] -> [Form] -> Form
    distribution ds cs = Cnj [Dsj [x,y] | x <- ds, y <- cs]

    isDisjunction :: Form -> Bool
    isDisjunction (Dsj _) = True
    isDisjunction _ = False

    isProp :: Form -> Bool
    isProp (Prop _) = True
    isProp (Neg (Prop _)) = True
    isProp _ = False

    isCnf :: Form -> Bool
    isCnf (Prop a)          = True
    isCnf (Neg (Prop a))    = True
    isCnf (Cnj forms)       = all (\x -> isProp x || isDisjunction x && isCnf x) forms
    isCnf (Dsj forms)       = all isProp forms
    isCnf _                 = False