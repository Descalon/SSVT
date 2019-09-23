{--Exercise 4 (50 min)
    Notes: the generater is limited to produce more manageable test inputs.

    Test report: ran the two test_* properties with quickcheck. All cases successful
--}

module Generator where
    import Lib
    import Control.Monad
    import Test.QuickCheck
    import Data.List

    chooseVar :: Gen Form
    chooseVar = oneof [return p, return q, return r]

    formGenerator :: Int -> Gen Form
    formGenerator 0 = chooseVar
    formGenerator n = oneof [chooseVar,
                    liftM Neg someForm,
                    liftM Cnj someForms,
                    liftM Dsj someForms,
                    liftM2 Impl someForm someForm,
                    liftM2 Equiv someForm someForm]
                where someForm = formGenerator (n `div` 2)
                      someForms = vectorOf 2 someForm

    sizedForm = sized formGenerator

    test = forAll sizedForm
    
    prop_should_be_same :: Form -> Bool
    prop_should_be_same f = post ((head . parse) f')
                        where 
                            f' = show f
                            post x = (show x) == f'

    test_should_be_same = test prop_should_be_same
    
    findString :: (Eq a) => [a] -> [a] -> Bool
    findString search str = case findIndex (isPrefixOf search) (tails str) of   Nothing -> False
                                                                                Just _ -> True

    prop_arrow_free :: Form -> Bool
    prop_arrow_free f = post ((arrowfree . head . parse) f')
                    where 
                        f' = show f
                        implArrow = (not . findString "==>")
                        equivArrow = (not . findString "<=>")
                        post x =  implArrow (show x) && equivArrow (show x)
    
    test_arrow_free = test prop_arrow_free