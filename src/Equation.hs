module Equation where

import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as S

data Coefficient a = Null | Constant a | Affine a (IM.IntMap a) deriving (Show, Eq, Ord)

data Linear a = Linear (IM.IntMap (Coefficient a)) deriving (Show, Eq, Ord)

data Equation a = Equation (Linear a) (Coefficient a) deriving (Show, Eq, Ord)

data System a = System (S.Seq (Equation a)) deriving (Show, Eq, Ord)

coeffFromConst :: (Eq a, Num a) => a -> Coefficient a
coeffFromConst 0 = Null
coeffFromConst c = Constant c

coeffFromMap :: (Num a, Eq a) => IM.IntMap a -> Coefficient a
coeffFromMap imap = if nonZero then Affine 0 nonZeroMap else Null
                    where nonZeroMap = IM.filter ((/=) 0) imap
                          nonZero = not (IM.null nonZeroMap)

coeffFromConstMap :: (Num a, Eq a) => a -> IM.IntMap a -> Coefficient a
coeffFromConstMap c imap = if nonZero then Affine c nonZeroMap else coeffFromConst c
                           where nonZeroMap = IM.filter ((/=) 0) imap
                                 nonZero = not (IM.null nonZeroMap)

coeffFromConstIdep :: (Eq a, Num a) => a -> Int -> Coefficient a
coeffFromConstIdep 0 _ = Null
coeffFromConstIdep c i = coeffFromMap $ IM.singleton i c

getCoeffMap :: Num a => Coefficient a -> Either a (a, IM.IntMap a)
getCoeffMap (Null)           = Left 0
getCoeffMap (Constant a)     = Left a
getCoeffMap (Affine a cmap)  = Right (a, cmap)

isZero :: (Num a, Eq a) => Coefficient a -> Bool
isZero (Null)          = True
isZero (Constant c  )  = c == 0
isZero (Affine c imap) = c == 0 && all ((==) 0) imap

addCoefficients :: (Num a, Eq a) => Coefficient a -> Coefficient a -> Coefficient a
addCoefficients (Null) coeff = coeff
addCoefficients coeff (Null) = coeff
addCoefficients (Constant a) (Constant b) = coeffFromConst (a+b)
addCoefficients (Constant a) (Affine b imap) = Affine (a+b) imap
addCoefficients (Affine a imap) (Constant b) = Affine (a+b) imap
addCoefficients (Affine a imap) (Affine b imap') = coeffFromConstMap (a+b) $ IM.unionWith (+) imap imap'

prettyCoefficient :: (Show a, Num a, Eq a, Ord a) => Coefficient a -> String
prettyCoefficient Null = "0"
prettyCoefficient (Constant c) = show c
prettyCoefficient (Affine 0 imap) = let parentheses = IM.size imap > 1 in
                                        (if parentheses then "(" else "") ++
                                            (prettyCoefficientLinear imap) ++
                                            (if parentheses then ")" else "")
prettyCoefficient (Affine c imap) = "(" ++ show c ++ (prettyCoefficientLinearWithSign imap) ++ ")"

prettyCoefficientWithSign :: (Show a, Num a, Eq a, Ord a) => Coefficient a -> String
prettyCoefficientWithSign Null = " + 0"
prettyCoefficientWithSign (Constant c)
        | c < 0 = " - " ++ show (abs c)
        | c > 0 = " + " ++ show c
prettyCoefficientWithSign (Affine 0 imap) = if IM.size imap == 1 then prettyCoefficientLinearWithSign imap
                                                                 else " + (" ++ prettyCoefficientLinear imap ++ ")"
prettyCoefficientWithSign (Affine c imap) = " + (" ++ show c ++ (prettyCoefficientLinearWithSign imap) ++ ")"

prettyCoefficientLinearWithSign :: (Show a, Num a, Ord a) => IM.IntMap a -> String
prettyCoefficientLinearWithSign = IM.foldMapWithKey (\i c -> (if signum c < 0 then " - " else " + ") ++ show (abs c) ++ " x_" ++ show (i+1))

prettyCoefficientLinear :: (Show a, Num a, Ord a) => IM.IntMap a -> String
prettyCoefficientLinear imap = let (Just (_, c)) = IM.lookupMin imap
                                   dropFun = if c < 0 then drop 1
                                                        else drop 3
                               in dropFun $
                                  IM.foldMapWithKey (\i c -> (if signum c < 0 then " - " else " + ") ++ show (abs c) ++ " x_" ++ show (i+1)) $
                                  imap

addLinear :: (Num a, Eq a) => Linear a -> Linear a -> Linear a
addLinear (Linear mapA) (Linear mapB) = Linear $ IM.unionWith addCoefficients mapA mapB

prettySys :: (Show a, Num a, Eq a, Ord a) => System a -> String
prettySys (System seq) = foldMap prettyEq seq

prettyEq :: (Show a, Num a, Eq a, Ord a) => Equation a -> String
prettyEq (Equation lin hom) = "0 = " ++ prettyLinear lin ++ prettyCoefficient hom ++ "\n"

prettyLinear :: (Show a, Num a, Eq a, Ord a) => Linear a -> String
prettyLinear (Linear linmap) = let (x:y:z:xs) = IM.foldMapWithKey (\ix coeff -> (prettyCoefficientWithSign coeff) ++ " " ++ "C_" ++ show (ix+1)) linmap ++ " "
                               in if y == '+' then xs else y:z:xs

prettySysMatrix :: (Num a, Eq a, Show a, Ord a) => System a -> String
prettySysMatrix (System seq) = S.foldMapWithIndex prettySysMatrixRow seq

prettySysMatrixRow :: (Num a, Eq a, Show a, Ord a) => Int -> Equation a -> String
prettySysMatrixRow eqnum (Equation (Linear linmap) _) = IM.foldMapWithKey
                                                        (\ix coeff -> "(" ++
                                                                       show (eqnum+1) ++
                                                                       ", " ++
                                                                       show (ix+1) ++
                                                                       ") = " ++
                                                                       prettyCoefficient coeff ++
                                                                       ",\n")
                                                          linmap

prettySysVector :: (Num a, Eq a, Show a, Ord a) => System a -> String
prettySysVector (System seq) = S.foldMapWithIndex prettySysVectorRow seq

prettySysVectorRow :: (Num a, Eq a, Show a, Ord a) => Int -> Equation a -> String
prettySysVectorRow eqnum (Equation _ coeff) = "(" ++ show (eqnum+1) ++ ") = " ++ prettyCoefficient coeff ++ ",\n"

sortPair :: (Ord a) => (a, a) -> (a, a)
sortPair (a, b) = (min a b, max a b)

equationToAdjacency :: Equation a -> Set.Set (Int, Int)
equationToAdjacency equation = Set.fromList $ map sortPair $
                               (,) <$> i <*> i
                         where i = ideps equation

adjacencyMatrix :: System a -> Set.Set (Int, Int)
adjacencyMatrix (System seq) = foldMap equationToAdjacency seq

ideps :: Equation a -> [Int]
ideps (Equation (Linear linmap) _) = IM.keys linmap

prettyAdjacency :: Int -> Set.Set (Int, Int) -> String
prettyAdjacency dim set = unlines $
                          do
                            row <- [0..dim-1]
                            return $ concat $
                              do
                                column <- [0..dim-1]
                                return $ if Set.member (sortPair (row, column)) set
                                         then "1,"
                                         else "0,"
