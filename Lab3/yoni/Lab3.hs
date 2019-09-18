module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> evl v f1 == evl v f2) (allVals f1)

equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1