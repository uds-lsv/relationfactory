{-# LANGUAGE OverloadedStrings #-}
module Text ( Txt
            , lines
            , words
            , unlines
            , unwords
            , append
            , null
            , isPrefixOf
            , suffix
            , prefix
            , spellingSpec
            , take
            , takeWhile
            , fromString
            , toString
            , toLower
            )
where
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.Char as Char
import qualified Data.String as String
import Data.List (group)
import Prelude hiding (lines
            , words
            , unlines
            , unwords
            , take
            , takeWhile
            , drop
            , null)
import qualified Prelude

type Txt = U.ByteString

instance String.IsString U.ByteString where
  fromString = U.fromString

lines :: Txt -> [Txt]
lines = U.lines

words :: Txt -> [Txt]
words = map U.fromString . Prelude.words . U.toString

unlines :: [Txt] -> Txt
unlines = U.fromString . Prelude.unlines . map U.toString

unwords :: [Txt] -> Txt
unwords = U.fromString . Prelude.unwords . map U.toString

append :: Txt -> Txt -> Txt
append u v = U.fromString $ U.toString u ++ U.toString v 

null :: Txt -> Bool
null = (==0) . U.length 

isPrefixOf :: Txt -> Txt -> Bool
isPrefixOf p s = let p' = U.toString p
                     s' = U.toString s
                 in Prelude.take (Prelude.length p') s' == p'

take :: Int -> Txt -> Txt
take i = U.fromString . Prelude.take i . U.toString

takeWhile f = U.fromString . Prelude.takeWhile f . U.toString 
fromString = U.fromString
toString = U.toString
toLower = U.fromString . map Char.toLower . U.toString

prefix i  = take i
suffix i  = U.fromString . reverse . Prelude.take i . reverse . U.toString


spellingSpec  = U.fromString 
                 . map  (\(x:xs) -> x) 
                 . group 
                 . map collapse 
                 . U.toString


collapse c | Char.isAlpha c && Char.isUpper c = 'X'
           | Char.isAlpha c && Char.isLower c = 'x'
           | Char.isDigit c              = '0'
           | c == '-'               = '-'
           | c == '_'               = '_'
           | otherwise              = '*'
