{-# LANGUAGE FlexibleContexts , BangPatterns #-}
module Perceptron.Vector  
    ( SparseVector
    , LocalSparseVector
    , DenseVector
    , DenseVectorST
    , toSV
    , for_
    , plus_
    , minus_
    , plus
    , minus
    , scale
    , dot 
    , dot'
    )
where

import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad.ST
import Data.STRef
import Control.Monad
import qualified Data.Map as Map
import Data.List (foldl')
import Debug.Trace (trace)
import Config
import qualified Data.Vector.Unboxed as V

type SparseVector i = Map.Map i Float
type LocalSparseVector y i = (y,V.Vector i)
type DenseVectorST s i = STUArray s i Float
type DenseVector i = UArray i Float



for_ xs f = mapM_ f xs

plus_ :: (Show i,Ix i) => DenseVectorST s i -> SparseVector i -> ST s ()
plus_ w v = do
  for_ (Map.toList v) $ \(i,vi) -> do
             wi <- readArray w i 
             writeArray w i (wi + vi)
minus_ w v = plus_ w (v `scale` (-1))

scale :: (Ix i)  => SparseVector i -> Float -> SparseVector i
scale v n = Map.map (*n) v

plus :: (Ix i)  => SparseVector i -> SparseVector i -> SparseVector i
plus u v = Map.unionWith (+) u v
minus :: (Ix i)  => SparseVector i -> SparseVector i -> SparseVector i
minus u v = u `plus` (v `scale` (-1))


dot :: DenseVector (Int,Int) -> LocalSparseVector Int Int -> Float
{-# INLINE dot #-}
dot w (!y,x) = V.foldl' (\ !z !i -> z + w ! (y,i)) 0 x
{-
dot w (!y,v) = go 0 v
    where go !s [] = s
          go !s (!i:v) = go (s + w ! (y,i)) v
-}

dot' :: (Float,DenseVector (Int,Int),DenseVector (Int,Int)) 
     -> LocalSparseVector Int Int 
     -> Float
{-# INLINE dot' #-}
dot' (!c,params,params_a) (!y,x) = V.foldl' (\ !z !i -> 
                                                 let e   = params   ! (y,i)
                                                     e_a = params_a ! (y,i)
                                                 in z + (e - (e_a * (1/c))))
                                            0
                                            x
{-
dot' (!c,params,params_a) (!y,x) = go 0 x
  where go !s [] = s
        go !s (!i:x) = 
            let e   = params   ! (y,i)
                e_a = params_a ! (y,i)
            in go (s + (e - (e_a * (1/c)))) x
-}

toSV :: (V.Unbox i, Ord y,Ord i) => LocalSparseVector y i -> SparseVector (y,i)
toSV (y,v) = Map.fromList [ ((y,i),1) | i <- V.toList v ]
