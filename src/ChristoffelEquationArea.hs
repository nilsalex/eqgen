module ChristoffelEquationArea (areaEq, christoffelToNum, sym2ToNum) where

import Equation
import Index
import qualified IntertwinerArea

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as S

spacetimeDimension :: Int
spacetimeDimension = 4

areaDimension :: Int
areaDimension = 21

spacetimeRange :: [Int]
spacetimeRange = [0..spacetimeDimension - 1]

spacetimeRangeUp :: [ISUp]
spacetimeRangeUp = map ISUp spacetimeRange

spacetimeRangeDown :: [ISDown]
spacetimeRangeDown = map ISDown spacetimeRange

areaRange :: [Int]
areaRange = [0..areaDimension - 1]

areaRangeUp :: [IGUp]
areaRangeUp = map IGUp areaRange

areaRangeDown :: [IGDown]
areaRangeDown = map IGDown areaRange

sym2ToNum :: Int -> Int -> Int
sym2ToNum a b = foldl' (\acc r -> acc + 4 - (r+1)) 0 [0, 1 .. p-1] + q
      where p = min a b
            q = max a b

--christoffelToNum :: Int -> Int -> Int -> Int
--christoffelToNum a m n = a * 10 + sym2ToNum m n
christoffelToNum :: Int -> Int -> Int -> Int
christoffelToNum a m n = a * 16 + m * 4 + n

areaEq :: (Fractional a, Eq a) => System a
areaEq = System $ S.fromList $
         do
            indexa <- spacetimeRangeDown
            indexA <- areaRangeDown
            return $ Equation (linear aIMap indexa indexA) (hom indexa indexA)
    where aIMap = IntertwinerArea.buildAreaIntertwinerMap

hom :: (Fractional a, Eq a) => ISDown -> IGDown -> Coefficient a
hom indexa indexA = coeffFromConstIdep (fromIntegral (-1)) num
          where ia = unISDown indexa
                iA = unIGDown indexA
                num = areaDimension + areaDimension * ia + iA

linear :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                  ISDown -> IGDown -> Linear a
linear aIMap indexa indexA = foldl' addLinear (Linear $ IM.empty) $
                             catMaybes $
                             do
                                  indexB <- areaRangeUp
                                  indexm <- spacetimeRangeUp
                                  indexn <- spacetimeRangeDown
                                  let factor = aIMap Map.! (indexA, indexB, indexm, indexn)
                                  let coeff = coeffFromConstIdep factor (unIGUp indexB)
                                  let m = unISUp indexm
                                  let n = unISDown indexn
                                  let a = unISDown indexa
                                  return (if factor == 0 then Nothing
                                                         else Just $ Linear $ IM.singleton (christoffelToNum n a m) coeff)
