module SubSets where
    import Lib
    import SetOrd

    sub :: Form -> Set Form
    sub (Prop x) = Set [Prop x]
    sub (Neg f) = unionSet (Set [Neg f]) (sub f)
    sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
    sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
    sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
    sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


    -- simple method to count the props in a form
    propCount :: Form -> Integer
    propCount (Prop x) = 1
    propCount (Neg x) = 1 + propCount x
    propCount (Dsj [a, b]) = 1 + propCount a + propCount b
    propCount (Cnj [a, b]) = 1 + propCount a + propCount b
    propCount (Impl f1 f2) = 1 + propCount f1 + propCount f2
    propCount (Equiv e1 e2) = 1 + propCount e1 + propCount e2


    -- method to check whether all formulas from a form are found within the result of sub <form>
    allSet :: Form -> Set Form -> Bool
    allSet (Prop x) s = inSet (Prop x) s
    allSet (Neg x) s = allSet x s
    allSet (Dsj [a, b]) s = inSet (Dsj [a, b]) s && allSet a s && allSet b s
    allSet (Cnj [a, b]) s = inSet (Cnj [a, b]) s && allSet a s && allSet b s
    allSet (Impl f1 f2) s = inSet (Impl f1 f2) s && allSet f1 s && allSet f2 s && inSet f1 s && inSet f2 s
    allSet (Equiv f1 f2) s = inSet (Equiv f1 f2) s && allSet f1 s && allSet f2 s && inSet f1 s && inSet f2 s

    formSize :: Form -> Int
    formSize f = formSize' f + (length (propNames f))
                where
                    formSize' (Prop x) = 0
                    formSize' (Neg x) = formSize' x + 1
                    formSize' (Dsj [x, y]) = formSize' x + formSize' y + 1
                    formSize' (Cnj [x, y]) = formSize' x + formSize' y + 1
                    formSize' (Impl x y) = formSize' x + formSize' y + 1
                    formSize' (Equiv x y) = formSize' x + formSize' y + 1

    -- Returns cardinality of a set of subformulas
    setSize :: Set a -> Int
    setSize f = setSize' f 0
                where
                    setSize' (Set []) c = c;
                    setSize' (Set (x:xs)) c = setSize' (Set xs) (c+1);

    -- Tests the propery of equal cardinality between form and set of subformulas
    propertyEqualLengthTest :: Form -> Bool
    propertyEqualLengthTest f = (formSize f) == (setSize (sub f))



    nsub :: Form -> Int
    nsub f = nsub' (sub f) 0
            where
                nsub' (Set []) c = c;
                nsub' (Set (x:xs)) c = nsub' (Set xs) (c+1);

    -- Quicktest that compares the nsub against formsize function
    testForCardinality :: Form -> Bool
    testForCardinality f = (formSize f) == (nsub f)