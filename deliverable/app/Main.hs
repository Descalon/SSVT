module Main where

import Text.Read
import Lib

main :: IO ()
main = do 
        userInput <- getLine
        let rawInput = readMaybe userInput
        let input = case rawInput of    
                        Nothing -> error "please enter a number between 1 and 8"
                        Just x -> x
        return input
            


        