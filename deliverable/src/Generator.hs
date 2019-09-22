module Generator where
    import Lib
    import Control.Monad
    import Test.QuickCheck

    sizedForm = sized f

    chooseVar :: Gen Form
    chooseVar = oneof [return p, return q, return r]

    f :: Int -> Gen Form
    f 0 = chooseVar
    f n = oneof [chooseVar,
                    liftM Neg someForm,
                    liftM Cnj someForms,
                    liftM Dsj someForms,
                    liftM2 Impl someForm someForm,
                    liftM2 Equiv someForm someForm]
                where someForm = f (n `div` 2)
                      someForms = vectorOf 2 someForm

    
    prop_should_be_same :: Form -> Bool
    prop_should_be_same f = post ((head . parse) f')
                        where 
                            f' = show f
                            post x = (show x) == f'

