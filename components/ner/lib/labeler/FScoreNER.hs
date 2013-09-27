module FScoreNER ( entitySpans
                 , accumScores
                 , totalScores
                 )
where
import FScore (scoresFromCard)
import qualified Data.Set as Set
import Data.List (maximumBy,minimumBy,foldl')
import Data.Ord (comparing)
import Utils (splitOn)

data Tag = Begin String Int
         | In String Int
         | Out deriving (Show)

type Token = (Int,Tag)

toTag x i = case splitOn '-' x of
                ["B",tag] -> Begin tag i
                ["I",tag] -> In tag i
                ["O"]     -> Out

toTags xs = zipWith toTag xs [1..]

phrases curr acc [] = curr:acc
phrases curr acc (x:xs) = 
    case x of
      Begin tag i -> phrases [(tag,i)]      (curr:acc) xs
      In tag i    -> phrases ((tag,i):curr) acc        xs
      Out         -> phrases curr           acc        xs

  
spans xs = [ (fst y, minimum (map snd (y:ys)), maximum (map snd (y:ys))) | y:ys <- xs ]

entitySpans = Set.fromList . spans . phrases [] [] . toTags 

zipTriple f (a,b,c) (x,y,z) = (f a x, f b y, f c z)

size :: (Ord a, Num n) => Set.Set a -> n
size = fromIntegral . Set.size

accumScores :: (Num n) => [[String]] -> [[String]] -> ((n, n, n), [(n, n, n)])
accumScores gold test = let local  = zipWith f gold test
                            global = foldl' (zipTriple (+)) (0,0,0) local
                            f g t = let ges = entitySpans g
                                        tes = entitySpans t
                                    in  (size (Set.intersection ges tes), size ges, size tes)
                        in (global,local)

totalScores :: [[String]] -> [[String]] -> (Double, Double, Double)
totalScores gold test = let ((truePos,goldCard,testCard),_) = accumScores gold test in scoresFromCard truePos goldCard testCard
