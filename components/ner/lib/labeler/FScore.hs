module FScore ( scores
              , scoresFromCard
              , precision
              , recall
              , fscore
              ) 
               
where

import qualified Data.Set as Set


class Set s where
    intersection :: s -> s -> s
    size :: (Num n) => s -> n

instance (Ord a) => Set (Set.Set a) where
    intersection = Set.intersection
    size = fromIntegral . Set.size

scores :: (Set s) => s -> s -> (Double,Double,Double)
scores gold test = scoresFromCard (size (intersection gold test)) (size gold) (size test)

scoresFromCard :: (Fractional a) => a -> a -> a -> (a, a, a)
scoresFromCard truepos gold test =
    let precision = truepos / test
        recall    = truepos / gold
        fscore    = if precision == 0 || recall == 0 
                    then 0 
                    else 2 * precision * recall / (precision + recall)
    in (precision , recall , fscore )


precision :: (Double,Double,Double) -> Double
precision (p,_,_) = p
recall :: (Double,Double,Double) -> Double
recall (_,r,_) = r
fscore :: (Double,Double,Double) -> Double
fscore (_,_,f) = f
