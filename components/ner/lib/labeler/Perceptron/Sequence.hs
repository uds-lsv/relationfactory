{-# LANGUAGE NoMonomorphismRestriction 
  , BangPatterns
  , FlexibleInstances
 #-}
module Perceptron.Sequence
    (
      Model
    , Options(..)
    , Eval
    , YMap
    , train
    , decode
    )
where

import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.Array as A
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST
import Data.STRef
import Control.Monad
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Perceptron.Vector
import System.IO
import Debug.Trace
import Config 
import Data.List (inits,foldl',sortBy)
import Data.Ord (comparing)
import ListZipper 
import qualified Data.Binary as Binary

data Model = Model { options :: Options 
                   , weights :: UArray I Float }
type X = [Xi]
type Y = [Yi]
type I = (Yi,Xii) 
type Xi = V.Vector Xii
type Xii = Int
type Yi = Int
type Dot = LocalSparseVector Yi Xii -> Float

data Options = Options { oYMap       :: YMap
                       , oIndexSet   :: IntSet.IntSet
                       , oYDict      :: IntMap.IntMap [Yi]
                       , oYs         :: [Yi]
                       , oBeam       :: Int 
                       , oRate       :: Float
                       , oEpochs     :: Int 
                       } deriving Eq

type YMap = (Xi,A.Array Yi Xi,A.Array (Yi,Yi) Xi)

instance Binary.Binary (V.Vector Int) where
    put v = Binary.put $ V.toList v
    get = V.fromList `fmap` Binary.get
         
instance Binary.Binary Model where
    put m = do 
      Binary.put (options m)
      -- Binary.put (weights m)
      let (lo,hi) = bounds . weights $ m
          xs = filter (\(_,e) -> e /= 0.0) . assocs . weights $ m
      Binary.put (lo,hi)
      Binary.put xs

    get = {-# SCC "get1" #-} do 
      os <- Binary.get 
      os == os `seq` return ()
      ws <- do
        (lo,hi) <- Binary.get
        xs <- Binary.get
        xs == xs `seq` return ()
        return $ accumArray (+) 0 (lo,hi) $ xs
      ws == ws `seq` return ()
      return $ Model os ws

instance Binary.Binary Options where
    put (Options a b c d e f g) = Binary.put a >> Binary.put b >> Binary.put c 
                               >> Binary.put d >>  Binary.put e >> Binary.put f
                               >> Binary.put g
    get = {-# SCC "get2" #-} do
      a <- Binary.get
      a == a `seq` return ()
      b <- Binary.get
      b == b `seq` return ()
      c <- Binary.get 
      c == c `seq` return ()
      d <- Binary.get 
      d == d `seq` return ()
      e <- Binary.get
      e == e `seq` return ()
      f <- Binary.get
      f == f `seq` return ()
      g <- Binary.get
      g == g `seq` return ()
      return $ Options a b c d e f g

yDictFind :: Options -> Xi -> [Yi]
yDictFind opts fs = 
    let mk = V.find (`IntSet.member` oIndexSet opts) $ fs
        def = oYs opts
    in case mk of
         Just k -> IntMap.findWithDefault def k . oYDict $ opts
         Nothing -> def

-- | DECODING 
decode :: Model -> X -> Y
decode m = fst . decode' (options m) (weights m `dot`) 

data Cell = Cell { cScore :: !Float
                 , cPhi   :: SparseVector I
                 , cPath  :: Y
                 , cStep  :: ListZipper Xi  } deriving (Show,Eq)

decode' :: Options -> Dot -> X -> (Y,SparseVector I)
decode' opts w x = 
  bestPath opts w [Cell { cScore = 0 
                        , cPhi = Map.empty
                        , cPath = []
                        , cStep = fromList x } ]


phi :: Options -> X -> Y -> SparseVector I
phi opts x y = foldl' f Map.empty . zip x . map reverse . tail . inits $ y
    where f z (xi,ys) = z `plus` toSV  (features (oYMap opts) xi ys)

{-# INLINE features #-}          
features :: YMap -> Xi -> [Yi] -> LocalSparseVector Yi Xii
features (!zero,uni,bi) xi (y:ys) = 
    case ys of
      []            -> (y, zero V.++  xi)
      [y1]          -> (y, uni A.! y1 V.++ xi)
      (y1 : y2 : _) -> let r = bi A.! (y1,y2) 
                       in  if V.null r 
                           then  (y, uni A.! y1  V.++ xi)
                           else  (y, r           V.++ xi)

beamSearch ::  Options
           -> Dot
           -> [Cell] 
           -> [Cell]
beamSearch opts w cs = 
    let f cs = if any (atEnd . cStep) cs then cs 
               else 
                   let cs' =   [    let fs =  features (oYMap opts) xi (y':ys)
                                    in Cell { cScore = 
                                                  s + w fs 
                                            , cPhi = ph `plus`  (toSV fs)
                                            , cPath = (y':ys)
                                            , cStep = next x } 
                                        | Cell { cScore = s 
                                               , cPhi = ph 
                                               , cPath = ys 
                                               , cStep = x } <- cs 
                               , let Just xi = focus x::Maybe Xi
                               , y' <- yDictFind opts xi
                               ]
                   in f . take (oBeam opts) 
                        . sortBy  (flip $ comparing cScore) 
                        $ cs'
    in f cs 

bestPath :: Options
            -> Dot
            -> [Cell]
            -> (Y, SparseVector I)
bestPath opts w xs = 
  let xs' =  beamSearch opts w xs
      first =  (\(x:_) -> x) xs'
  in ( reverse . cPath $ first
          , cPhi first )

-- | TRAINING

iter :: Options 
        -> Int
        -> [(X,Y)]
        -> (STRef s Int, DenseVectorST s I, DenseVectorST s I)
        -> ST s ()
iter opts _ ss (c,params,params_a) = do
    for_ ss $ \ (x,y) -> do
      params' <- unsafeFreeze params
      let (y',phi_xy') = decode' opts (params'`dot`) x
      when (y' /= y) $ do 
        let phi_xy = phi opts x y 
            update = (phi_xy `minus` phi_xy') `scale` oRate opts
        params `plus_` update
        c' <- readSTRef c
        params_a `plus_` (update `scale` fromIntegral c')
      modifySTRef c (+1)


type Eval = Int -> [(Y,Y)] -> [(Y,Y)] -> String

train :: Options -> [(X, Y)] -> Eval -> [(X,Y)] -> Model
train opts heldout eval ss =  Model opts $ runSTUArray $ do
    let bs = computeBounds opts ss
    trace (show bs) () `seq` return ()
    params <- newArray bs 0
    params_a <- newArray bs 0
    c <- newSTRef 1
    let undef = error "Perceptron.Sequence.train: undefined"
    runLogger . hPutStrLn stderr $ eval 0 undef undef
    for_ [1..oEpochs opts] $ 
             \i -> do iter opts i ss (c,params,params_a)
                      c' <- readSTRef c
                      params' <- unsafeFreeze params
                      params_a' <- unsafeFreeze params_a
                      let w  = (fromIntegral c',params',params_a')
                          ys xys = [ fst . decode' opts (w`dot'`) $ x 
                                            | (x,_) <- xys ]
                      runLogger 
                             . hPutStrLn stderr
                             $ eval i (zip (map snd ss) (ys ss))
                                      (zip (map snd heldout) (ys heldout)) 
                             
    finalParams (c, params,  params_a)
    return params


{-# NOINLINE runLogger #-}
runLogger f = unsafeIOToST f

finalParams :: (STRef s Int, DenseVectorST s I, DenseVectorST s I) 
            -> ST s ()
finalParams (c,params,params_a) = do
  (l,u) <- getBounds params
  c' <- fmap fromIntegral (readSTRef c)
  for_ (range (l,u)) $ \i -> do
      e   <- readArray params   i
      e_a <- readArray params_a i
      writeArray params i (e - (e_a * (1/c')))

computeBounds :: Options -> [(X,Y)] -> (I,I)
computeBounds opts =   foldl' f ((maxBound,minimum xis)
                               ,(minBound,maximum xis)) 
                . (\(xs,ys) -> zip (concat xs) (concat ys))
                . unzip
    where f ((!miny,!minx),(!maxy,!maxx)) (xs,!y) =
              ((min miny y,V.minimum $ minx`V.cons`xs)
              ,(max maxy y,V.maximum $ maxx`V.cons`xs))
          xis = let (zero,uni,bi) = oYMap opts
                in      uniq
                      . concatMap V.toList
                      $
                      [zero]
                      ++
                      (filter (not . V.null)
                        . A.elems
                        $ bi)
                      ++
                      (filter (not . V.null) 
                        . A.elems 
                        $ uni)
                    
