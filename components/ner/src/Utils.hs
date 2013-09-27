module Utils ( splitOn
             , splitWith
             , padRight
             , uniq
             , accuracy
             )
where
import Data.List (foldl')
import qualified Data.Set as Set

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn = splitWith . (==)

splitWith ::  (a -> Bool) -> [a] -> [[a]]
splitWith f s =  case dropWhile f s of
                   [] -> []
                   s' -> w : splitWith f s''
                       where (w, s'') = break f s'

padRight :: a -> Int -> [a] -> [a]
padRight x i xs = take i (xs ++ repeat x)

uniq :: (Ord a) => [a] -> [a]
uniq = Set.toList . Set.fromList

accuracy predicted testset  = 
    fromIntegral (foldl' (+) 0 (zipWith ((fromEnum .) . (==)) predicted 
                                                              testset))
                              / 
                              fromIntegral (length testset)
