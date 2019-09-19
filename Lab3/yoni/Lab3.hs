module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Time spent - slightly under an hour
-- not even a single eval should succeed
contradiction :: Form -> Bool
contradiction = not . satisfiable

-- they should all succeed, therefore, loop over all possiblities an all should be true
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- For all value combinations for form1, whatever form 1 returns should be valid for form 2
entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> if (evl v f1) then (evl v f2) else True) (genVals (propNames f1 ++ propNames f2))

-- Just like entails, but vice versa as well
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1

