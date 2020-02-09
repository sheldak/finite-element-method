module Main where

import FiniteElementMethod
import System.Environment

main :: IO ()
main = do
    elementsNum <- getArgs
    putStrLn "Calculation in progress..."
    finiteElementMethod (read (head elementsNum))
    putStrLn $ "Chart of solution made using " ++ (head elementsNum) ++ " elements has been drawn!"