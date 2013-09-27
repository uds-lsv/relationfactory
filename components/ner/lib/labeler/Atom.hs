{-# LANGUAGE  GeneralizedNewtypeDeriving , NoMonomorphismRestriction #-}
module Atom ( MonadAtoms (..)
            , AtomTable (..)
            , Atoms
            , AtomsT
            , empty
            , evalAtoms
            , evalAtomsT
            , runAtoms
            , runAtomsT
            )
where
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Binary as B
import qualified Text
type Txt = Text.Txt


data AtomTable = T { lastID :: !Int 
                   , to :: Map.Map Txt Int 
                   , from :: IntMap.IntMap Txt } 
                   deriving (Eq,Show)


instance B.Binary AtomTable where
    put t = do B.put (lastID t) 
               B.put (to t)
               B.put (from t)
    get = do liftM3 T B.get B.get B.get


class Monad m => MonadAtoms m where
    toAtom :: Txt -> m Int
    maybeToAtom :: Txt -> m (Maybe Int)
    fromAtom :: Int -> m Txt
    table    :: m AtomTable

instance Monad m => MonadAtoms (AtomsT m) where
    toAtom x = AtomsT $ do
      t <- get
      case Map.lookup x (to t) of
        Just j -> return $! j
        Nothing -> do 
                 let i = lastID t
                     i' = i + 1 
                     !t' = t { lastID = i'
                             , to = Map.insert x  i (to t) 
                             , from = IntMap.insert i x (from t) }
                 put t'
                 return $! lastID t

    maybeToAtom x = 
        AtomsT $ do
          t <- get
          return . Map.lookup x . to $ t
            
    fromAtom i = AtomsT $ do
      t <- get
      return $ (from t) IntMap.! i
    table = AtomsT get

empty :: AtomTable
empty = T 0 Map.empty IntMap.empty

runAtomsT :: AtomsT t t1 -> AtomTable -> t (t1, AtomTable)
runAtomsT (AtomsT x) s = runStateT x s

runAtoms :: Atoms t -> AtomTable -> (t, AtomTable)
runAtoms (Atoms x) s = runIdentity (runAtomsT x s)


evalAtoms :: Atoms t -> t
evalAtoms = fst . flip runAtoms empty

evalAtomsT :: (Monad m) => AtomsT m a -> m a
evalAtomsT = liftM fst . flip runAtomsT empty

newtype AtomsT m r = AtomsT (StateT AtomTable m r)
    deriving (Functor,Monad,MonadTrans,MonadIO)

newtype Atoms r = Atoms (AtomsT Identity r)
    deriving (Functor,Monad,MonadAtoms)
