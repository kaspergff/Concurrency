{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}



module Oefening where

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Unsafe as Unsafe
import qualified Prelude as P

-- Accelerate backend
import Data.Array.Accelerate.Interpreter
-- import Data.Array.Accelerate.LLVM.Native
-- import Data.Array.Accelerate.LLVM.PTX

--vector = fromList (Z:. 13) [1,2,3,6,6,4,2,1,1,6,7,8,9]:: Array DIM1 Int
vector = fromList (Z:. 10) [1..10]:: Array DIM1 Int
accvector = use vector

-- run $ scanl (+) 10 (use vector)
-- run $ scanl' (+) 10 (use vector)
-- run $ scanl1 (+)  (use vector)
-- run $ prescanl (+) 10 (use vector)
-- run $ postscanl (+) 10 (use vector)

mkHeadFlags :: Acc (Vector Int) -> Acc (Vector Bool)
mkHeadFlags seg =
 let
 T2 offset len = scanl' (+) 0 seg
 zeros = fill (I1 (the len)) (constant False)
 ones = fill (shape seg) (constant True)
 in
 permute const zeros (\ix -> I1 (offset ! ix)) ones 


fuckingfilter :: Acc (Vector Int) -> (Exp Int -> Exp Bool) -> (Vector Int)
fuckingfilter ar f =
  let
    foo' = map f ar 
    foo  = map boolToInt (foo') 
    bar =  prescanl (+) 0 foo 
    numb = the $ sum foo   
    def  = fill (index1 numb ) Unsafe.undef 
 in 
     --als er op index ix een true staat in foo' dan moet de ar op de indexplaats van ix in bar opgehaald worden.
    run $ permute const (def) (\ix ->ifThenElse (foo' ! ix)  (index1 ( bar ! ix ) ) ignore)  ar

fuckinghistogram :: Acc (Vector Int) -> Acc (Vector Int)
fuckinghistogram vec =
    let
    numb  = length accvector
    numb' = the $ fold (max) 0 accvector
    enen  = fill (index1 numb)  1 :: Acc (Vector Int)
    def   = fill (index1 (numb'+1)) 0 :: Acc(Vector Int)
    in 
    permute (+) def (\ix -> (index1 (vec ! ix))) enen





