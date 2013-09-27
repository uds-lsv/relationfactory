{-# LANGUAGE OverloadedStrings #-}
module CorpusReader ( corpus
                    , corpusLabeled 
                    , fromWords
                    )
where
import ListZipper 
import Utils (splitWith)
import Token 
import qualified Text
import Text (Txt)

corpus :: Bool -> Txt -> [[ListZipper Token]]
corpus hasLemma =
      map toZippers
    . map (map $ parseFields hasLemma)
    . splitWith null
    . map Text.words
    . Text.lines 

corpusLabeled :: Bool -> Txt -> [([ListZipper Token], [Txt])]
corpusLabeled hasLemma = 
      map (\xys -> let (xs,ys) = unzip xys in (toZippers xs,ys))
    . map (map $ parseFieldsLabeled hasLemma)
    . splitWith null
    . map Text.words
    . Text.lines
 
fromWords :: [Txt] -> [ListZipper Token] 
fromWords = toZippers . map (\ w -> Token w w "" "")

parseFieldsLabeled True  [f,l,y] = (Token f l "" "",y)
parseFieldsLabeled False [f,y] = (Token f f "" "",y)
parseFieldsLabeled flag other = error $ "CorpusReader.parseFieldsLabeled: " 
                                 ++ show flag ++ " " ++ show other

parseFields True [f,l] = Token f l "" ""
parseFields False [f] = Token f f "" ""
parseFields True [f,l,_] = Token f l "" ""
parseFields False [f,_] = Token f f "" ""
parseFields flag other = error $ "CorpusReader.parseFields: " 
                                 ++ show flag ++ " " ++ show other

toZippers :: [a] -> [ListZipper a]
toZippers xs =
    let z = fromList xs
        f z = if atEnd z then []
              else z:f (next z)
    in f z
