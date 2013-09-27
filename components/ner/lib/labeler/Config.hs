{-# LANGUAGE BangPatterns , TypeSynonymInstances , NoMonomorphismRestriction #-}
module Config (  
                F
              , Tag
              , Txt
              , Config (..)
              , Text.suffix
              , Text.spellingSpec
              , toLower
              , conjoin
              , uniq
              )
where
import Data.Char
import Data.List (group)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Token
import Data.Ix
import ListZipper
import Utils (uniq)
import Atom (AtomTable)
import qualified Data.Binary as B
import qualified Data.Set as Set
import Control.Monad (ap)
import qualified Text 
import Text (Txt)

type X = ListZipper Token
type Y = ListZipper String
type Tag = Int
type F = Int

data Config = Config { clusterDict :: Map.Map Txt Txt
                     , lexicon :: Map.Map (Either Txt Txt) [Txt]
                     , conjunction :: Bool
                     , wordMinCount :: Int
                     , atomTable :: AtomTable 
                     , minLabelFreq :: Int
                     , hasLemma :: Bool
                     }

instance B.Binary Config where
    get = let g = B.get
          in return Config `ap` g `ap` g `ap` g `ap` g `ap` g `ap` g `ap` g

    put (Config a b c d e f g) =
        let p = B.put 
        in p a >> p b >> p c >> p d >> p e >> p f >> p g
    

conjoin :: (Ord b) => (a -> a-> Bool) -> (a -> a -> b) -> [a] -> [b]
conjoin p f xs = 
    [ f x x' | x <- xs , x' <- xs , p x x' || p x' x ] 

