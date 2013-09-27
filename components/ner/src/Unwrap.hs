{-# LANGUAGE OverloadedStrings #-}
import System (getArgs)
import Utils (splitOn,splitWith)
import Labeler (predict)
import CorpusReader (fromWords)
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import Data.ByteString.Internal (c2w)
import qualified Text
import Text (Txt)
import Data.Maybe (fromJust, isJust)
import ListZipper (focus)
import Data.List (find,isPrefixOf,sortBy,zipWith4, maximumBy)
import System.FilePath (takeBaseName)
import Data.Ord (comparing)

main = do
  (modelf:nefiles) <- getArgs
  model <- decode `fmap` BS.readFile modelf
  pats <- getPatterns nefiles
  txt <- BS.getContents
  let (hs,ss) =   unzip 
                . map (\(h:lns) -> (h,map trim lns))
                . splitOn sep 
                . Text.lines 
                $ txt
      yss = predict model . map fromWords $ ss
      zss = zipWith (\ws ys -> 
                         let wys = chunks (\(w,y) -> y == "O") $ zip ws ys
                         in  concat [ specialEntities pats (map fst ch) 
                                      | ch <- wys ])
                     ss 
            $ yss
  BS.putStr . Text.unlines $ zipWith4 format hs ss yss zss

format :: Txt -> [Txt] -> [Txt] -> [Txt] -> Txt
format h ws ys zs = Text.unlines 
                         (h:(zipWith3 (\w y z ->
                                        Text.unwords [w,if y == "O" 
                                                        then z 
                                                        else y]) ws ys zs))
                 `Text.append` sep

sep = "</D>"

trim = Text.unwords . Text.words

chunks :: (a -> Bool) -> [a] -> [[a]]
chunks f xs = case break f xs of
                ([],zs) -> chunks (not . f) zs
                (zs,[]) -> [zs]
                (pre,rest) -> pre:chunks (not . f) rest

specialEntities :: [(Txt,Int,Set.Set [Txt])] -> [Txt] -> [Txt]
specialEntities pats ws = f ws 
    where f [] = []
          f (w:ws) |    "http://"  `Text.isPrefixOf` w 
                     || "https://" `Text.isPrefixOf` w
                     || "ftp://"   `Text.isPrefixOf` w
                     || "www."     `Text.isPrefixOf` w     
                   = "B-URL" : f ws
          f (w:ws) = case match (w:ws) pats of
                   Nothing    -> "O": f ws
                   Just (l,t) -> let tags = "B-"`Text.append`t 
                                                : map ("I-"`Text.append`) 
                                                  (repeat t)
                                   in take l tags ++ f (drop l (w:ws))

match :: [Txt] -> [(Txt,Int,Set.Set [Txt])] -> Maybe (Int, Txt)
match ws pats =
    let prefixes s = map (flip take ws) [1..s]
    in case [ (length p, tag) | (tag, size, set) <- pats
                             , p <- prefixes size
                             , Set.member p set ]
       of [] -> Nothing
          xs -> Just . maximumBy (comparing fst)  $ xs

getPatterns :: [FilePath] -> IO [(Txt,Int,Set.Set [Txt])]
getPatterns fs = 
    flip mapM fs $ \f -> do
      let tag = BS.pack . map c2w . takeBaseName $ f
      pats <- (map Text.words . Text.lines) `fmap` BS.readFile f
      return (tag, maximum . map length $ pats, Set.fromList pats)
