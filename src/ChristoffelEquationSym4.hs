module ChristoffelEquationSym4 (sym4Eq, christoffelToNum, sym2ToNum) where

import Equation
import Index

import Data.List (foldl', sort)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as S

spacetimeDimension :: Int
spacetimeDimension = 4

sym4Dimension :: Int
sym4Dimension = 35

spacetimeRange :: [Int]
spacetimeRange = [0..spacetimeDimension - 1]

sym4Range :: [Int]
sym4Range = [0..sym4Dimension - 1]

sym4Indices :: [(Int, Int, Int, Int)]
sym4Indices = 
                       [ (0, 0, 0, 0),
                         (0, 0, 0, 1),
                         (0, 0, 0, 2),
                         (0, 0, 0, 3),
                         (0, 0, 1, 1),
                         (0, 0, 1, 2),
                         (0, 0, 1, 3),
                         (0, 0, 2, 2),
                         (0, 0, 2, 3),
                         (0, 0, 3, 3),
                         (0, 1, 1, 1),
                         (0, 1, 1, 2),
                         (0, 1, 1, 3),
                         (0, 1, 2, 2),
                         (0, 1, 2, 3),
                         (0, 1, 3, 3),
                         (0, 2, 2, 2),
                         (0, 2, 2, 3),
                         (0, 2, 3, 3),
                         (0, 3, 3, 3),
                         (1, 1, 1, 1),
                         (1, 1, 1, 2),
                         (1, 1, 1, 3),
                         (1, 1, 2, 2),
                         (1, 1, 2, 3),
                         (1, 1, 3, 3),
                         (1, 2, 2, 2),
                         (1, 2, 2, 3),
                         (1, 2, 3, 3),
                         (1, 3, 3, 3),
                         (2, 2, 2, 2),
                         (2, 2, 2, 3),
                         (2, 2, 3, 3),
                         (2, 3, 3, 3),
                         (3, 3, 3, 3) ]

sym4ToNumMap :: Map.Map (Int, Int, Int, Int) Int
sym4ToNumMap = Map.fromList $ zip 
                       sym4Indices
                       [0..]

sym4ToNum :: (Int, Int, Int, Int) -> Int
sym4ToNum (a, b, c, d) = smap Map.! (a', b', c', d')
    where smap = sym4ToNumMap
          [a', b', c', d'] = sort [a, b, c, d]

permutations :: [(Int, Int, Int, Int)]
permutations =         [ (0, 1, 2, 3),
                         (0, 1, 3, 2),
                         (0, 2, 1, 3),
                         (0, 2, 3, 1),
                         (0, 3, 1, 2),
                         (0, 3, 2, 1),
                         (1, 0, 2, 3),
                         (1, 0, 3, 2),
                         (1, 2, 0, 3),
                         (1, 2, 3, 0),
                         (1, 3, 0, 2),
                         (1, 3, 2, 0),
                         (2, 0, 1, 3),
                         (2, 0, 3, 1),
                         (2, 1, 0, 3),
                         (2, 1, 3, 0),
                         (2, 3, 0, 1),
                         (2, 3, 1, 0),
                         (3, 0, 1, 2),
                         (3, 0, 2, 1),
                         (3, 1, 0, 2),
                         (3, 1, 2, 0),
                         (3, 2, 0, 1),
                         (3, 2, 1, 2) ]

permute :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
permute (a, b, c, d) (p1, p2, p3, p4) = (a', b', c', d')
    where [a', b', c', d'] = map snd . sort $ zip [p1, p2, p3, p4] [a, b, c, d]

sym2ToNum :: Int -> Int -> Int
sym2ToNum a b = foldl' (\acc r -> acc + 4 - (r+1)) 0 [0, 1 .. p-1] + q
      where p = min a b
            q = max a b

--christoffelToNum :: Int -> Int -> Int -> Int
--christoffelToNum a m n = a * 10 + sym2ToNum m n
christoffelToNum :: Int -> Int -> Int -> Int
christoffelToNum a m n = a * 16 + m * 4 + n

sym4Eq :: (Fractional a, Eq a) => System a
sym4Eq = System $ S.fromList $
         do
            p <- spacetimeRange
            (a, b, c, d) <- sym4Indices
            return $ Equation (linear a b c d p) (hom p a b c d)

hom :: (Fractional a, Eq a) => Int -> Int -> Int -> Int -> Int -> Coefficient a
hom p a b c d = foldl' addCoefficients (coeffFromConst 0) $
                        coeffFromConstIdep (fromIntegral 6) num :
                        do
                            perm <- permutations
                            let (a', b', c', d') = permute (a, b, c, d) perm
                            let numperm = sym4Dimension + sym4Dimension * d' + sym4ToNum (p, a', b', c')
                            return $ coeffFromConstIdep (fromIntegral (-1)) numperm
          where num = sym4Dimension + sym4Dimension * p + sym4ToNum (a, b, c, d)


linear :: (Fractional a, Eq a) => Int -> Int -> Int -> Int -> Int -> Linear a
linear a b c d p = foldl' addLinear (Linear $ IM.empty) $
                             do
                                  m <- spacetimeRange
                                  perm <- permutations
                                  let (a', b', c', d') = permute (a, b, c, d) perm
                                  let coeff = coeffFromConstIdep (fromIntegral 3) (sym4ToNum (m, a', b', c'))
                                  return $ Linear $ IM.singleton (christoffelToNum m d' p) coeff
