#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO          as Text
import qualified Data.Text.Lazy             as Text
import qualified Data.Set                   as Set
import qualified System.Environment         as Env
import qualified Data.Maybe                 as Maybe
import qualified Data.List                  as List
import qualified System.FilePath            as FilePath
import qualified Data.Ord                   as Ord
import qualified Data.List.Split            as Split

type Txt = Text.Text

main = do
  nefiles <- Env.getArgs
  pats <- getPatterns nefiles
  txt <- Text.getContents
  let ss = parse txt
      zss = map (specials pats) ss
  Text.putStr . Text.unlines $ List.zipWith format ss zss


specials :: [(Txt, Int, Set.Set [Txt])] -> [(Txt, Txt)] -> [Txt]
specials pats wys =
  let chs = chunks (\(w,y) -> y == "O") wys
  in  concat [ specialEntities pats (map fst ch) 
             | ch <- chs ]

parse :: Txt -> [[(Txt, Txt)]]
parse = map (map (\ [word, tag] -> (word, tag)))
    . Split.splitWhen null
    . map Text.words
    . Text.lines 

format :: [(Txt, Txt)] -> [Txt] -> Txt
format wys zs = Text.unlines 
                (zipWith (\(w, y) z ->
                            Text.unwords [w, if y == "O" 
                                             then z 
                                             else y]) 
                 wys 
                 zs)

trim :: Txt -> Txt
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
          xs -> Just . List.maximumBy (Ord.comparing fst)  $ xs

getPatterns :: [FilePath] -> IO [(Txt, Int, Set.Set [Txt])]
getPatterns fs = 
    flip mapM fs $ \f -> do
      let tag = Text.pack . FilePath.takeBaseName $ f
      pats <- (map Text.words . Text.lines) `fmap` Text.readFile f
      return (tag, maximum . map length $ pats, Set.fromList pats)
