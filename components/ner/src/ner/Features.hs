{-# LANGUAGE OverloadedStrings #-}
module Features ( features 
                , maybeFeatures
                , outputFeatures
                , indexFeatures
                , Token (..)
                )
where
import ListZipper
import Atom
import Data.List (sortBy,isPrefixOf,find)
import Data.Ord
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Token
import Data.Maybe
import Config 
import Debug.Trace
import qualified Text as T
import qualified Data.Vector.Unboxed as V

append = T.append

indexFeatures :: AtomTable -> IntSet.IntSet 
indexFeatures  =
     IntMap.keysSet 
           . IntMap.filter ("WF0=" `T.isPrefixOf`) 
           . from 
           
    

format (k,v) = k `append` "=" `append` v
inputFeatures :: Config -> ListZipper Token -> [Txt]
inputFeatures config x = 
    let fs = filter (not . T.null . snd) . infs config $ x
        fs' =   uniq
              . conjoin (\f f' -> fst f `conj` fst f')
                        (\f f' -> 
                             if f < f' 
                             then format f  `append` "&" `append` format f'
                             else format f' `append` "&" `append` format f )
              $ fs
    in if conjunction config 
       then map format fs ++ fs'
       else map format fs
       
fetch k  = Map.findWithDefault "" k 

type FID  = Txt
type FVal = Txt
infs :: Config -> ListZipper Token -> [(FID,FVal)]
infs config z = 
    let tok0 = z `at` 0
        fs tok = map (\x -> if T.null x then "<NULL>" else x) 
                 [form tok,pos tok,chunk tok]
    in
      [ ("WF0"     , form tok0)
      , ("LWF0"    ,  T.toLower . form $ tok0)
--      , ("LF0"     ,  lemma tok0)
      , ("Suf30"   ,  suffix 3 . form $ tok0)
      , ("Suf20"   ,  suffix 2 . form $ tok0)
      , ("Suf10"   ,  suffix 1 . form $ tok0)
      , ("WSh0"    ,  spellingSpec (form  tok0))
--      , ("POS0"    ,  pos tok0)
--      , ("Ch0"     ,  chunk tok0) 
      ]
    ++ [("CWF0",fetch (form tok0) (clusterDict config))]
{-
    ++ [ ("IBX0",f) | 
         f <- Map.findWithDefault [] (Left (form tok0)) (lexicon config) ]
    ++ [ ("IBXs0",f) | 
         f <- Map.findWithDefault [] (Right (form tok0)) (lexicon config) ]
-}
    ++ [("BOS",if T.null $ form (z `at` (-1)) then "True" else "")]
    ++ zip ["WFL1"{-,"POSL1","ChL1"-}] (fs $ z `at` (-1))
    ++ zip ["WFL2"{-,"POSL2","ChL2"-}] (fs $ z `at` (-2))
    ++ zip ["WFR1"{-,"POSR1","ChR1"-}] (fs $ z `at` 1)
    ++ zip ["WFR2"{-,"POSR2","ChR2"-}] (fs $ z `at` 2)

outfs :: [Txt] -> [(FID,FVal)]
outfs (y:y':_) = [("OutL1",y),("OutL12",y`append`y')]
outfs [y]      = [("OutL1",y)]
outfs []       = []

contentFS =  ["WF0","LWF0","Suf30","Suf20","Suf10","WSh0"
             ,"POS0","Ch0"
             ]
genContentFS = ["LWF0","Suf30","Suf20","Suf10","WSh0"
               ,"POS0","Ch0"
               ]
contextFS =  ["WFL1"
             ,"POSL1"
             ,"ChL1"
             ,"WFL2"
             ,"POSL2"
             ,"ChL2"
             ,"WFR1"
             ,"POSR1"
             ,"ChR1"
             ,"WFR2"
             ,"POSR2"
             ,"ChR2"
             ]

conj f f' =    f `elem` ["CWF0"]
            && f' `elem` genContentFS  

atomizeSnd fs = mapM f fs
    where f (f,v) = do va <- toAtom v
                       return (f,va)
features :: (MonadAtoms m) => Config -> ListZipper Token -> m (V.Vector F)
features config x = do
  ifs <- mapM toAtom (inputFeatures config x)
  return $ V.fromList ifs

maybeFeatures :: (MonadAtoms m) => Config -> ListZipper Token -> m (V.Vector F)
maybeFeatures config x = do
  ifs <- mapM maybeToAtom (inputFeatures config x)
  return (V.fromList $ catMaybes $ ifs)

outputFeatures :: [Txt] -> [Txt]
outputFeatures = map format . outfs 