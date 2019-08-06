module Lib
    ( egmain
    ) where

import ChristoffelEquationSym4
import Equation
import Floyd

egmain :: IO ()
egmain = do
            let eq = sym4Eq :: System Rational
--            putStrLn $ prettySys eq;
            {-
            let adj = adjacencyMatrix eq
            let mat = matrixFromAdjacency 40 adj
            print $ floyd 40 mat
            -}
            putStrLn "with(LinearAlgebra);"
            putStrLn ""
            putStrLn "mat := Matrix(140, 64, {"
            putStrLn $ prettySysMatrix eq
            putStrLn "}, storage=sparse);"
            putStrLn ""
            putStrLn "vec := Vector(140, {"
            putStrLn $ prettySysVector eq
            putStrLn "}, storage=sparse);"
