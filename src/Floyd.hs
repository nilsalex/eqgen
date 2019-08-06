module Floyd where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.List (foldl')

import Data.Maybe

data Matrix a = Matrix (M.Map (Int, Int) a) deriving (Show, Eq, Ord)

matrixAt :: Matrix a -> Int -> Int -> a
matrixAt (Matrix mat) i j = mat M.! (min i j, max i j)

matrixUpdate :: Matrix a -> Int -> Int -> a -> Matrix a
matrixUpdate (Matrix mat) i j val = Matrix $ M.insert (min i j, max i j) val mat

matrixFromAdjacency :: Int -> S.Set (Int, Int) -> Matrix (Maybe Int)
matrixFromAdjacency dim adj = Matrix $ M.fromList $
                              do
                                row <- [0..dim-1]
                                col <- [0..dim-1]
                                let a = min row col
                                let b = max row col
                                let val = if row == col
                                          then Just 0
                                          else if S.member (a, b) adj
                                               then Just 1
                                               else Nothing
                                return ((a, b), val)

floyd :: Int -> Matrix (Maybe Int) -> Matrix (Maybe Int)
floyd dim mat = foldl' floydStep mat $
                (\k i j -> (k, i, j)) <$>
                [0..dim-1] <*>
                [0..dim-1] <*>
                [0..dim-1]

floydStep :: Matrix (Maybe Int) -> (Int, Int, Int) -> Matrix (Maybe Int)
floydStep mat (k, i, j) = if s == Nothing
                          then mat
                          else if ij == Nothing || fromJust ij > fromJust s
                               then matrixUpdate mat i j s
                               else mat
    where ij = matrixAt mat i j
          ik = matrixAt mat i k
          kj = matrixAt mat k j
          s = (+) <$> ik <*> kj
