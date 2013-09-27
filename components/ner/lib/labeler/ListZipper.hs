module ListZipper ( ListZipper(..)
                  , focus
                  , left
                  , right
                  , reset
                  , fromList
                  , toWindows
                  , toList
                  , next
                  , atEnd 
                  , at
                  )
where
import Data.Maybe
import Data.Monoid
import Data.Foldable (Foldable,foldMap)


data ListZipper a = LZ [a] (Maybe a) [a]  deriving (Show,Eq,Ord)


left  (LZ xs _ _) = xs
focus (LZ _ x _)  = x
right (LZ _ _ ys) = ys

fromList []     = LZ [] Nothing [] 
fromList (x:xs) = LZ [] (Just x) xs

toWindows xs = let zs = iterate next . fromList $ xs
               in map snd . zip xs $ zs

toList (LZ xs (Just x) ys) = reverse xs ++ [x] ++ ys
toList (LZ xs Nothing [])  = reverse xs

next :: ListZipper a -> ListZipper a
next = nextWith id id
nextWith f g (LZ lxs (Just x) rxs)  =
    case rxs of
      y:ys -> LZ (f x:lxs) (Just (g y)) ys
      []   -> LZ (f x:lxs) Nothing []

atEnd = isNothing . focus

reset (LZ [] (Just y) ys)     = LZ [] (Just y) ys
reset (LZ [] Nothing [])      = LZ [] Nothing []
reset (LZ xs (Just y) ys) = LZ [] (Just z) (zs++[y]++ys)
    where z:zs = reverse xs                    
reset (LZ xs Nothing [])  = LZ [] (Just z) zs
    where z:zs = reverse xs

from :: (Monoid m) => Maybe m -> m
from Nothing  = mempty
from (Just x) = x

index 1 (x:xs)  = Just x
index i []  = Nothing
index i (x:xs) = index (i-1) xs

at :: (Monoid m) =>  ListZipper m -> Int -> m
at  z 0 = from . focus $ z
at  z i | i < 0 = from .  index (negate i) . left  $ z
at  z i         = from .  index i          . right $ z

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (fmap f ls) (fmap f x) (fmap f rs) 
instance Foldable ListZipper where
    foldMap f (LZ ls x rs) = mconcat $ fmap f (reverse ls) 
                             ++ [from $ fmap f x] ++ fmap f rs

