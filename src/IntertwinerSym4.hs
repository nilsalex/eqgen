module IntertwinerSym4 where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import Index

type Sym4Indices     = (Int, Int, Int, Int)
type Sym4IndicesUp   = (ISUp, ISUp, ISUp, ISUp)
type Sym4IndicesDown = (ISDown, ISDown, ISDown, ISDown)

s4IUpfromS4I :: Sym4Indices -> Sym4IndicesUp
s4IUpfromS4I (a, b, c, d) = (ISUp a, ISUp b, ISUp c, ISUp d)

s4IDownfromS4I :: Sym4Indices -> Sym4IndicesDown
s4IDownfromS4I (a, b, c, d) = (ISDown a, ISDown b, ISDown c, ISDown d)

{- dimension of spacetime -}
spacetimeDimension :: Int
spacetimeDimension = 4

{- Range of sym4 variables -}
sym4Range :: [Int]
sym4Range = [0..((spacetimeDimension * (spacetimeDimension + 1) * (spacetimeDimension + 2) * (spacetimeDimension + 3)) `div` (1*2*3*4)) - 1]

sym4RangeUp :: [IS4Up]
sym4RangeUp = map IS4Up sym4Range

sym4RangeDown :: [IS4Down]
sym4RangeDown = map IS4Down sym4Range

{- Range of spacetime dimensions -}
spacetimeRange :: [Int]
spacetimeRange = [0..spacetimeDimension - 1]

spacetimeRangeUp :: [ISUp]
spacetimeRangeUp = map ISUp spacetimeRange

spacetimeRangeDown :: [ISDown]
spacetimeRangeDown = map ISDown spacetimeRange

{- distribution of sym4 dof in sym4 spacetime tensor -}
buildSym4IndicesMap :: Map.Map Int Sym4Indices
buildSym4IndicesMap = Map.fromList $ zip 
                       [0..]
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

{- product of five spacetime deltas -}
spacetimeDelta :: Num a => Sym4IndicesUp -> Sym4IndicesDown -> ISUp -> ISDown -> a
spacetimeDelta (a, b, c, d) (e, f, g, h) m n =
    if (unISUp m, unISUp a, unISUp b, unISUp c, unISUp d) ==
       (unISDown e, unISDown n, unISDown f, unISDown g, unISDown h) then 1 else 0

{- factor in front of intertwiners
 - dependent on distribution of dof above -}
intertwinerFactor :: Num a => Sym4IndicesUp -> a
intertwinerFactor (a, b, c, d)
        | count == 1 = 1
        | count == 2 = if (snd $ Map.findMin hist) == 2 then 6 else 1
        | count == 3 = 12
        | count == 4 = 24
        | otherwise = undefined
    where hist = histogram spacetimeRangeUp [a, b, c, d]
          count = Map.size hist

histogram :: (Eq a, Ord a) => [a] -> [a] -> Map.Map a Int
histogram range vals = Map.fromList $
                       filter (\(i, c) -> c /= 0) $
                       zip range $
                       map (\ix -> length . filter (==ix) $ vals) range
{-
dofIntertwiner :: Fractional a => Sym4Indices -> Int -> Int -> Int -> Int -> a
dofIntertwiner (a, b, c, d) m n p q = sum [ if (a == m) && (b == n) && (c == p) && (d == q) then (1 / 24) else 0,
                                            if (a == m) && (b == n) && (c == q) && (d == p) then (1 / 24) else 0,
                                            if (a == m) && (b == p) && (c == n) && (d == q) then (1 / 24) else 0,
                                            if (a == m) && (b == p) && (c == q) && (d == n) then (1 / 24) else 0,
                                            if (a == m) && (b == q) && (c == n) && (d == p) then (1 / 24) else 0,
                                            if (a == m) && (b == q) && (c == p) && (d == n) then (1 / 24) else 0,
                                            if (a == n) && (b == m) && (c == p) && (d == q) then (1 / 24) else 0,
                                            if (a == n) && (b == m) && (c == q) && (d == p) then (1 / 24) else 0,
                                            if (a == n) && (b == p) && (c == m) && (d == q) then (1 / 24) else 0,
                                            if (a == n) && (b == p) && (c == q) && (d == m) then (1 / 24) else 0,
                                            if (a == n) && (b == q) && (c == m) && (d == p) then (1 / 24) else 0,
                                            if (a == n) && (b == q) && (c == p) && (d == m) then (1 / 24) else 0,
                                            if (a == p) && (b == m) && (c == n) && (d == q) then (1 / 24) else 0,
                                            if (a == p) && (b == m) && (c == q) && (d == n) then (1 / 24) else 0,
                                            if (a == p) && (b == n) && (c == m) && (d == q) then (1 / 24) else 0,
                                            if (a == p) && (b == n) && (c == q) && (d == m) then (1 / 24) else 0,
                                            if (a == p) && (b == q) && (c == m) && (d == n) then (1 / 24) else 0,
                                            if (a == p) && (b == q) && (c == n) && (d == m) then (1 / 24) else 0,
                                            if (a == q) && (b == m) && (c == n) && (d == p) then (1 / 24) else 0,
                                            if (a == q) && (b == m) && (c == p) && (d == n) then (1 / 24) else 0,
                                            if (a == q) && (b == n) && (c == m) && (d == p) then (1 / 24) else 0,
                                            if (a == q) && (b == n) && (c == p) && (d == m) then (1 / 24) else 0,
                                            if (a == q) && (b == p) && (c == m) && (d == n) then (1 / 24) else 0,
                                            if (a == q) && (b == p) && (c == n) && (d == m) then (1 / 24) else 0 ]

{- calculate value of dof-to-spacetime intertwiner -}
sym4Intertwiner :: Fractional a => Map.Map Int Sym4Indices -> IS4Up -> (ISDown, ISDown, ISDown, ISDown) -> a
sym4Intertwiner indicesMap (IS4Up a) (ISDown m, ISDown n, ISDown p, ISDown q) = intertwinerFactor (s4IUpfromS4I (indicesMap Map.! a)) *
                                                                                dofIntertwiner (indicesMap Map.! a) m n p q

buildSym4IntertwinerMap :: Fractional a => Map.Map (IS4Up, (ISDown, ISDown, ISDown, ISDown)) a
buildSym4IntertwinerMap = Map.fromList $ let indicesMap = buildSym4IndicesMap in
                           do a <- sym4RangeUp
                              m <- spacetimeRangeDown
                              n <- spacetimeRangeDown
                              p <- spacetimeRangeDown
                              q <- spacetimeRangeDown
                              return ((a, (m, n, p, q)), sym4Intertwiner indicesMap a (m, n, p, q))

{- calculate value of spacetime-to-dof projector -}
sym4Projector :: Fractional a => Map.Map Int Sym4Indices -> (ISUp, ISUp, ISUp, ISUp) -> IS4Down -> a
sym4Projector indicesMap (ISUp m, ISUp n, ISUp p, ISUp q) (IS4Down a) = dofIntertwiner (indicesMap Map.! a) m n p q

buildSym4ProjectorMap :: Fractional a => Map.Map ((ISUp, ISUp, ISUp, ISUp), IS4Down) a
buildSym4ProjectorMap = Map.fromList $ let indicesMap = buildSym4IndicesMap in
                                        do a <- sym4RangeDown
                                           m <- spacetimeRangeUp
                                           n <- spacetimeRangeUp
                                           p <- spacetimeRangeUp
                                           q <- spacetimeRangeUp
                                           return (((m, n, p, q), a), sym4Projector indicesMap (m, n, p, q) a)
-}

symmetrizeFour :: (i, i, i, i) -> [(i, i, i, i)]
symmetrizeFour (a, b, c, d) = [ (a, b, c, d),
                                (a, b, d, c),
                                (a, c, b, d),
                                (a, c, d, b),
                                (a, d, b, c),
                                (a, d, c, b),
                                (b, a, c, d),
                                (b, a, d, c),
                                (b, c, a, d),
                                (b, c, d, a),
                                (b, d, a, c),
                                (b, d, c, a),
                                (c, a, b, d),
                                (c, a, d, b),
                                (c, b, a, d),
                                (c, b, d, a),
                                (c, d, a, b),
                                (c, d, b, a),
                                (d, a, b, c),
                                (d, a, c, b),
                                (d, b, a, c),
                                (d, b, c, a),
                                (d, c, a, b),
                                (d, c, b, a) ]

{- symmetrizations by virtue of index swapping functions
 - rational prefactors are picked up -}
symmetrizeSym4Indices1 :: Fractional a => [(a, (i, i, i, i), (j, j, j, j))] -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeSym4Indices1 = concat . map (\(r, a, b) -> map (\a' -> (r/24, a', b)) . symmetrizeFour $ a)

symmetrizeSym4Indices2 :: Fractional a => [(a, (i, i, i, i), (j, j, j, j))] -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeSym4Indices2 = concat . map (\(r, a, b) -> map (\b' -> (r/24, a, b')) . symmetrizeFour $ b)

{- apply all symmetrizations -}
symmetrizeSym4Indices :: Fractional a => (i, i, i, i) -> (j, j, j, j) -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeSym4Indices a b = symmetrizeSym4Indices1 . symmetrizeSym4Indices2 $ [(1, a, b)]

{- calculate value of spacetime Gotay-Marsden intertwiners
 - fold and inner function -}
innerIntertwiner :: Fractional a => ISUp -> ISDown -> a -> (a, Sym4IndicesDown, Sym4IndicesUp) -> a
innerIntertwiner m n r1 (r2, a, b) = r1 + r2 * spacetimeDelta b a m n

spacetimeIntertwiner :: Fractional a => Sym4IndicesDown -> Sym4IndicesUp -> ISUp -> ISDown -> a
spacetimeIntertwiner a b m n = let symSym4 = symmetrizeSym4Indices a b
                                in foldl (innerIntertwiner m n) 0 symSym4

{- calculate value of Gotay-Marsden intertwiner using dof indices -}
sym4Intertwiner :: Fractional a => Map.Map Int Sym4Indices -> IS4Down -> IS4Up -> ISUp -> ISDown -> a
sym4Intertwiner indicesMap a b m n = -4 * intertwinerFactor (s4IUpfromS4I (indicesMap Map.! (unIS4Up b))) *
                                      spacetimeIntertwiner (s4IDownfromS4I (indicesMap Map.! (unIS4Down a)))
                                                           (s4IUpfromS4I (indicesMap Map.! (unIS4Up b))) m n

buildSym4IntertwinerMap :: Fractional a => Map.Map (IS4Down, IS4Up, ISUp, ISDown) a
buildSym4IntertwinerMap = Map.fromList $ let indicesMap = buildSym4IndicesMap in
                           do a <- sym4RangeDown
                              b <- sym4RangeUp
                              m <- spacetimeRangeUp
                              n <- spacetimeRangeDown
                              return ((a, b, m, n), sym4Intertwiner indicesMap a b m n)
