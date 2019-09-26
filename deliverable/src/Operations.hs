module Operations where
    import SetOrd
    import Lib
    import Generator
    import Data.List
    import Test.QuickCheck

    data SetOperation a = Op (Set a -> Set a -> Set a)

    setIntersection ::Eq a => SetOperation a
    setIntersection = Op (\ (Set a) (Set b) -> Set $ intersect a b)

    setUnion :: Eq a => SetOperation a
    setUnion = Op (\ (Set a) (Set b) -> Set $ union a b)

    setDifference :: Eq a => SetOperation a
    setDifference = Op (\ (Set a) (Set b) -> Set $ a \\ b)

    prop_Same_Size :: Eq a => Set a -> Set a -> Bool
    prop_Same_Size (Set a) (Set b) = length a == length b 

    prop_Elem_Only_In_Left :: Eq a => SetOperation a -> Set a -> Set a -> Bool
    prop_Elem_Only_In_Left (Op fn) left@(Set l) right@(Set r) = 
        let (Set applied) = fn left right
            notElemOf x = (not . elem x)
        in all (\x -> notElemOf x r --> elem x applied) l

    prop_Elem_Only_In_Right o l r = prop_Elem_Only_In_Left o r l
    
    prop_Elem_Only o l r = prop_Elem_Only_In_Left o l r && prop_Elem_Only_In_Right o l r

    prop_Elem_In_Both :: Eq a => SetOperation a -> Set a -> Set a -> Bool
    prop_Elem_In_Both (Op fn) left@(Set l) right@(Set r) = 
        let (Set applied) = fn left right
            condition = (\x -> elem x r && elem x l)
        in all condition applied

    