{-# LANGUAGE OverloadedStrings #-}
module Labeler 
    ( ModelData(..)
    , Config(..)
    , train
    , predict
    , readClusterDict
    , readWikiDict
    )
    
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (foldl',tails,maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import ListZipper
import qualified Perceptron.Sequence as P
import Perceptron.Sequence (Options(..))
import Utils (splitWith,uniq)
import Text.Printf
import Atom
import Control.Monad.RWS
import Features (maybeFeatures,features,outputFeatures,indexFeatures)
import Token (Token)
import qualified Data.Array as A
import qualified Data.Vector.Unboxed as V
import qualified Data.Binary as Binary
import qualified Text
import Data.Char
import Config 

data ModelData = ModelData { model :: P.Model
                           , config :: Config
                           } 
instance Binary.Binary ModelData where
    get = return ModelData `ap` Binary.get `ap` Binary.get
    put (ModelData a b) = Binary.put a >> Binary.put b 



--  Main exported functions 
predict :: ModelData -> [[ListZipper Token]] -> [[Txt]]
predict m testdat =                  
    fst . flip runAtoms (atomTable . config $ m) $
        do flip mapM testdat $ \x -> 
               do x' <- mapM (maybeFeatures (config m)) $ x
                  predict' (P.decode (model m)) $ x'

train :: Config 
      -> Float
      -> Int 
      -> Int 
      -> [([ListZipper Token],[Txt])]
      -> [([ListZipper Token],[Txt])]
      -> ModelData
train conf rate limit beam traindat heldout = 
        let ((m,_predicted),_atoms) = 
                 runAtoms (run conf 
                               (rate,limit,beam) 
                               traindat 
                               heldout) 
                              $ empty
        in m

readClusterDict :: Txt -> Map.Map Txt Txt
readClusterDict str = 
    Map.fromList [ let ws = Text.words $ ln 
                   in (last ws , Text.unwords . init . init $ ws)
                   | ln <- Text.lines str ]

readWikiDict :: Txt -> Map.Map (Either Txt Txt) [Txt]
readWikiDict = 
    Map.fromList
    . concat
    . map (\(title:defs') -> 
                     let defs = map (  Text.take 50 
                                     . Text.takeWhile (not . isPunctuation)) 
                                defs'
                     in case Text.words . Text.takeWhile (/='(') $ title of
                          first : rest ->(Left first,defs)
                                         :map (\r -> (Right r,defs)) rest
                          _ -> [])
    
    . splitWith Text.null
    . Text.lines

-- Implementation

tagDictionary ::  IntSet.IntSet 
              -> Int 
              -> [([V.Vector F], [Tag])] 
              -> IntMap.IntMap [Tag]
tagDictionary indexFeatureSet wmin trainset = 
    let tags = concat . map snd $ trainset
        ws   = map (fromMaybe (error $ "tagDictionary: not found ") 
                              . V.find (`IntSet.member` indexFeatureSet))
                                  
               . concat 
               . map fst 
               $ trainset
        count_ws = IntMap.fromListWith (+) [ (w,1) | w <- ws ]
        dict =   IntMap.map Set.toList
               . IntMap.fromListWith Set.union 
               $ [ (w,Set.singleton t) | (w,t) <- zip ws tags 
               , count_ws IntMap.! w >= wmin]
    in dict == dict `seq` dict

pruneLabels :: Int -> [(x,[Txt])] -> [(x,[Txt])]
pruneLabels lim xys =
    let freq =   Map.fromListWith (+)
               . map (\y -> (y,1))
               . concat
               . map snd
               $ xys
        undet =   fst 
                . maximumBy (comparing snd) 
                . Map.toList
                $ freq
    in [ (x,[ if freq Map.! yi < lim then undet else yi | yi <- y ]) 
         | (x,y) <- xys ]

run :: (Functor m, MonadAtoms m) =>
       Config
    ->  (Float, Int,Int)
    ->  [([ListZipper Token], [Txt])]
    ->  [([ListZipper Token], [Txt])]
    -> m (ModelData, [[Txt]])
run conf (rate, limit,beamp) trainset_in_full testset_in = do
  let trainset_in = pruneLabels (minLabelFreq conf) trainset_in_full
      ys = uniq . concat . map snd $ trainset_in :: [Txt]
  ys' <- mapM toAtom ys
  trainset <- mapM (mkfs $ features conf) trainset_in
  outm <- mkOutputFeatureAtoms . map snd $ trainset_in 
  testset <- mapM (mkfs $ maybeFeatures conf) testset_in 
  tab <- table
  let indexFeatureSet = indexFeatures tab
      conf' = conf {atomTable = tab }
      opts = Options { oYMap = outm
                     , oIndexSet =  indexFeatureSet
                     , oYDict = tagDictionary indexFeatureSet 
                                     (wordMinCount conf') trainset
                     , oYs   = ys'
                     , oBeam = beamp
                     , oRate = rate
                     , oEpochs = limit
                     }
      m = P.train opts testset formatEval trainset
  ps <- mapM (predict' (P.decode m . fst)) testset
  return $ (ModelData { model = m , config = conf' }
           ,ps)

predict' :: (MonadAtoms m) =>
            (t -> [Int]) -> t -> m [Txt]
predict' dec x = do
        let xr = dec  x
        xr'<- mapM fromAtom xr
        return xr'

mkOutputFeatureAtoms :: (MonadAtoms m) => [[Txt]] -> m P.YMap
mkOutputFeatureAtoms yss = do
  let unigrams = map return . uniq . concat $ yss
      bigrams = uniq $ concat [   filter ((==2) . length) 
                                . map (take 2) 
                                . tails 
                                $ ys | ys <- yss ]
  unigramis <- mapM (mapM toAtom) unigrams
  bigramis  <- mapM (mapM toAtom) bigrams
  let ys = map head unigramis
      (lo,hi) = (minimum ys,maximum ys)
  unigramfs <- mapM (mapM toAtom) . map outputFeatures $ unigrams
  bigramfs  <- mapM (mapM toAtom) . map outputFeatures $ bigrams
  zerofs <- mapM toAtom . outputFeatures $ []
  let ymap1 =   A.accumArray (V.++) V.empty (lo,hi) 
              . zip (map head unigramis) 
              . map V.fromList
              $ unigramfs
      ymap2 =    A.accumArray (V.++) V.empty ((lo,lo),(hi,hi)) 
               . zip (map (\ [y1,y2] -> (y1,y2)) bigramis)
               . map V.fromList
               $ bigramfs 
  return $ (V.fromList zerofs, ymap1, ymap2)
                             
mkfs :: (MonadAtoms m) => 
        (ListZipper Token -> m (V.Vector F)) 
     ->   ([ListZipper Token], [Txt]) 
     -> m ([V.Vector F], [Tag])
mkfs f (x,y) = do
  fs <- mapM f x
  fs == fs `seq` return ()
  y' <- mapM toAtom y
  y' == y' `seq` return ()
  return $ (fs,y')


formatEval :: P.Eval 
formatEval 0 _ _        = printf "%10s %10s %10s" ("Iter"::String) 
                                                  ("Train"::String)
                                                  ("Heldout"::String)
formatEval i ss heldout = printf "%10d %10.4f %10.4f" i (eval ss) (eval heldout)
    

eval :: Eq a => [([a],[a])] -> Double
eval ys = 
    let corr =   foldl' (+) 0 
               . concat
               $ [ [ 1 | (y,y') <- ys , (yi,yi') <- zip y y' 
                                  , yi == yi' ] ]
    in corr / fromIntegral (length . concatMap fst $ ys)
