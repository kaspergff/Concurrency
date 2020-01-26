{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}



module Quickhull
    ( quickhull
    , Point
    , propagateR
    , segmentedPostscanr
    ) where

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Unsafe as Unsafe
import qualified Prelude as P

-- Accelerate backend
import Data.Array.Accelerate.Interpreter
-- import Data.Array.Accelerate.LLVM.Native
-- import Data.Array.Accelerate.LLVM.PTX


-- for debug
input1 :: Acc (Vector Point)
input1 = use $ fromList (Z :. 15) [(1,4),(8,19),(5,9),(7,9),(4,2),(3,9),(9,16),(1,5),(9,11),(4,0),(8,18),(8,7),(7,18),(6,18),(4,19)]

test :: Acc (Vector Int)
test = use $ fromList (Z :. 4) [1,2,3,4,5] 
type Point = (Int, Int)
type Line = (Point, Point)
type SegmentedPoints = (Vector Bool, Vector Point)

pointIsLeftOfLine :: Exp Line -> Exp Point -> Exp Bool
pointIsLeftOfLine (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx *x + ny * y > c
  where
    nx = y1 - y2
    ny = x2 - x1 
    c  = nx * x1 + ny * y1

nonNormalizedDistance :: Exp Line -> Exp Point -> Exp Int
nonNormalizedDistance (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y - c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

-- * Exercise 1
leftMostPoint :: Acc (Vector Point) -> Acc (Scalar Point)
leftMostPoint = fold min (T2 maxBound maxBound)

rightMostPoint :: Acc (Vector Point) -> Acc (Scalar Point)
rightMostPoint = fold max (T2 minBound minBound)

initialPartition :: Acc (Vector Point) -> Acc SegmentedPoints
initialPartition points =
  let
    p1 = the $ leftMostPoint points
    p2 = the $ rightMostPoint points
    line = T2 p1 p2

    -- * Exercise 2
    isUpper :: Acc (Vector Bool)
    isUpper = map (pointIsLeftOfLine line) points

    isLower :: Acc (Vector Bool)
    isLower = zipWith (\a b -> ifThenElse (a == p1 || a == p2) b (not b)) points isUpper
    
    -- * Exercise 3
    lowerIndices :: Acc (Vector Int)
    lowerIndices = prescanl (+) 0 (map boolToInt isLower)

    -- * Exercise 4
    upperIndices :: Acc (Vector Int)
    countUpper :: Acc (Scalar Int)
    T2 upperIndices countUpper = scanl' (+) 0 (map boolToInt isUpper)

    -- * Exercise 5
    permutation :: Acc (Vector (Z :. Int))
    permutation =
      let
        f :: Exp Point -> Exp Bool -> Exp Int -> Exp Int -> Exp (Z :. Int)
        f p upper idxLower idxUpper
          = ifThenElse (p == p1) (index1 0) $
            ifThenElse (upper) (index1 (1 + idxUpper)) $
            ifThenElse (p == p2) (index1 (1 + the countUpper)) $
            index1 (2 + (the countUpper) + idxLower)
      in
        zipWith4 f points isUpper lowerIndices upperIndices

    -- * Exercise 6
    empty :: Acc (Vector Point)
    empty = fill sha p1
        where 
            len = 1 + length permutation 
            sha = index1 len

    newPoints :: Acc (Vector Point)
    newPoints = permute const empty (permutation !) points

    -- * Exercise 7
    headFlags :: Acc (Vector Bool)
    headFlags = map (\a-> ifThenElse (a == p1 || a == p2) (lift True) (lift False)) newPoints
  in
    -- debug
    --error $ P.show $ run headFlags
    T2 headFlags newPoints
    

-- * Exercise 8
segmentedPostscanl :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedPostscanl f vec seg = map snd $ postscanl (segmented f) Unsafe.undef (zip vec seg)
    where
        segmented ::  Elt a => (Exp a -> Exp a -> Exp a) -> (Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)) 
        segmented op (T2 fx x) (T2 fy y) = T2 ( fx || fy ) ( fy ? (y, op x y))
 
segmentedPostscanr :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
segmentedPostscanr f vec seg = map snd $ postscanr (segmented f) Unsafe.undef (zip vec seg)
    where
        segmented ::  Elt a => (Exp a -> Exp a -> Exp a) -> (Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)) 
        segmented op (T2 fx x) (T2 fy y) = T2 ( fx || fy ) ( fx ? (x, op x y) )

-- * Exercise 9
propagateL :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateL = segmentedPostscanl const

propagateR :: Elt a => Acc (Vector Bool) -> Acc (Vector a) -> Acc (Vector a)
propagateR = segmentedPostscanr (P.flip const)

-- * Exercise 10
propagateLine :: Acc SegmentedPoints -> Acc (Vector Line)
propagateLine (T2 headFlags points) = zip (propagateL headFlags points) (propagateR headFlags points) 

-- * Exercise 11
shiftHeadFlagsL :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsL flags = (tail flags) ++ scalarFalse

shiftHeadFlagsR :: Acc (Vector Bool) -> Acc (Vector Bool)
shiftHeadFlagsR flags = scalarFalse ++ (init flags)

-- helper function for exercise 11
scalarFalse :: Acc ( Vector Bool)
scalarFalse = fill (constant (Z :. 1)) (lift False)

-- use for debug
-- foo :: Acc ( Vector Bool)
-- foo = fill (constant (Z :. 10)) (lift True)

partition :: Acc SegmentedPoints -> Acc SegmentedPoints
partition (T2 headFlags points) =
  let
    vecLine :: Acc (Vector Line)
    vecLine    = propagateLine (T2 headFlags points)
    headFlagsL = shiftHeadFlagsL headFlags
    headFlagsR = shiftHeadFlagsR headFlags

    -- * Exercise 12
    furthest :: Acc (Vector Point)
    furthest = 
        let disToLine = zipWith (\a b -> T2 (nonNormalizedDistance b a) a) points vecLine
        in map snd $ propagateR headFlags $ segmentedPostscanl max headFlagsR disToLine

    -- * Exercise 13
    isLeft  :: Acc (Vector Bool)
    isLeft  = zipWith3 (\(T2 p1 _) pf point -> pointIsLeftOfLine (T2 p1 pf) point) vecLine furthest points

    isRight :: Acc (Vector Bool)
    isRight = zipWith3 (\(T2 _ p2) pf point -> pointIsLeftOfLine (T2 pf p2) point) vecLine furthest points

    -- * Exercise 14
    segmentIdxLeft :: Acc (Vector Int)
    segmentIdxLeft  = segmentedPostscanl (+) headFlags (map getNumValue isLeft)

    segmentIdxRight :: Acc (Vector Int)
    segmentIdxRight = segmentedPostscanl (+) headFlags (map getNumValue isRight)

    -- helper function for execise 14 for going from boolean value to an int value
    getNumValue   ::  Exp Bool -> Exp Int
    getNumValue b = ifThenElse (b) (1) (0)
                 
    -- * Exercise 15
    -- je kan dus die segmentInxLeft gebruiken omdat die dus cumulatief bijhoudt hoeveel van die punten left zijn, als je die flags eentje opschuift naar links krijg je dus de meest rechter waarde van een segment, die meest rechter waarde is de hoeveelheid van punten die links zijn in het segment
    countLeft  :: Acc (Vector Int)
    countLeft  = propagateR headFlagsL segmentIdxLeft

    countRight :: Acc (Vector Int)
    countRight = propagateR headFlagsL segmentIdxRight

    -- * Exercise 16
    -- als de huidige flag true is dan is het resultaat, als de volgende flag true is dan is het huidige punt de laatste van het segment en is de resulterende waarde van dat veld de som van countright + countleft + 1 
    segmentSize :: Acc (Vector Int)
    segmentSize = zipWith4 (\flag flag1 cright cntleft -> segmentSize' flag flag1 cright cntleft) headFlags headFlagsL countRight countLeft
      where 
        segmentSize' flag flag1 cright cntLeft  = ifThenElse (flag) (1) $
                                                  ifThenElse (flag1) (cright+cntLeft+1) (0)

    --de segment offset is de som van alle voorgaande segmentgroottes
    segmentOffset :: Acc (Vector Int)
    size :: Acc (Scalar Int)
    T2 segmentOffset size = scanl' (+) 0 segmentSize 

    -- * Exercise 17
    permutation :: Acc (Vector (Z :. Int))
    permutation =
      let
        f :: Exp Bool -> Exp Point -> Exp Point -> Exp Bool -> Exp Bool -> Exp Int -> Exp Int -> Exp Int -> Exp Int -> Exp (Z :. Int)
        f flag p furthestP left right offset cntLeft idxLeft idxRight
          = ifThenElse flag (index1 offset) $
            ifThenElse left (index1 $ offset + idxLeft -1) $
            ifThenElse right (index1 $ offset + idxRight + cntLeft) $
            ifThenElse (p == furthestP) (index1 $ offset + cntLeft) $
            ignore
      in
        zipWith9 f headFlags points furthest isLeft isRight segmentOffset countLeft segmentIdxLeft segmentIdxRight

    -- * Exercise 18
    empty :: Acc (Vector Point)
    empty = fill (index1 $ the size) (T2 0 0)
    
    -- kunnen we deze niet ff liften uit de initialpartition functie?? is gwn exact hetzelfde ding nm
    newPoints :: Acc (Vector Point)
    newPoints = permute const empty (permutation !) points

    -- * Exercise 19
    newHeadFlags :: Acc (Vector Bool)
    newHeadFlags = undefined
  in
    error $ P.show $ run newPoints
    --T2 newHeadFlags newPoints

-- * Exercise 20
condition :: Acc SegmentedPoints -> Acc (Scalar Bool)
condition = undefined

-- * Exercise 21
quickhull' :: Acc (Vector Point) -> Acc (Vector Point)
quickhull' = undefined

quickhull :: Vector Point -> Vector Point
quickhull = run1 quickhull'

-- * Bonus
quickhullSort' :: Acc (Vector Int) -> Acc (Vector Int)
quickhullSort' = undefined

quickhullSort :: Vector Int -> Vector Int
quickhullSort = run1 quickhullSort'
