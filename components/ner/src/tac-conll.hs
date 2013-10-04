#!/usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO          as Text
import qualified Data.Text.Lazy             as Text
import qualified System.Environment         as Env
import qualified Data.List.Split            as Split

main = do
  command:args <- Env.getArgs
  case command of
    "to-conll"   -> to_conll args
    "from-conll" -> from_conll args
    
separator = "</D>"
    
to_conll :: [String] -> IO ()
to_conll (path:_) = do            
  text <- Text.readFile path
  Text.putStr 
    . Text.unlines 
    . map Text.unlines 
    . map tailOrNil
    . Split.splitOn [separator] 
    . Text.lines
    $ text
    
from_conll :: [String] -> IO ()
from_conll (tac_path: conll_path: _) = do
  tac <- Text.readFile tac_path
  conll <- Text.readFile conll_path
  let ss_tac   = Split.splitOn [separator] . Text.lines $ tac 
      ss_conll = Split.splitWhen Text.null . Text.lines $ conll
      ss_out = zipWith merge ss_tac ss_conll
  Text.putStr . Text.concat . map Text.unlines $ ss_out
  
merge :: [Text.Text] -> [Text.Text] -> [Text.Text]  
merge (header:_) lines = [header] ++ lines ++ [separator]
merge [] _ = []
  
tailOrNil :: [a] -> [a]
tailOrNil [] = []
tailOrNil (_:xs) = xs 
