module Main (main)
where
import Labeler (Config(..),ModelData(..)
               ,train,predict,readClusterDict,readWikiDict)
import CorpusReader (corpus,corpusLabeled)
import qualified Text
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as Binary
import System.Environment (getArgs)
import System.IO (hPutStrLn,stderr)

main :: IO ()
main = do
  (command:args) <- getArgs
  case command of
    "train" -> do 
         let [haslemma
              ,conj
              ,rate
              ,beamp
              ,limit
              ,mincount
              ,cdictf
              ,lexf
              ,trainf
              ,testf
              ,outf
              ] =  args
         let lem = read haslemma
         traindat <- fmap (corpusLabeled lem) 
                     $ BS.readFile trainf
         testdat <- fmap (corpusLabeled lem) $ BS.readFile testf
         cdict <- fmap (readClusterDict) $ BS.readFile cdictf
         lexm <- fmap readWikiDict $ BS.readFile lexf
         let conf = Config { clusterDict = cdict 
                             , lexicon = lexm
                             , conjunction = read conj
                             , wordMinCount = read mincount
                             , atomTable = error 
                                           "main:Config.atomTable undefined" 
                           , minLabelFreq = 10
                           , hasLemma = lem
                           }
         BS.writeFile outf  
              . Binary.encode 
              . train conf (read rate) (read limit) (read beamp) traindat 
              $ testdat
    "predict" -> do
                 let [modelf] = args
                 m <- fmap Binary.decode (BS.readFile modelf)
                 testdat <- fmap (corpus . hasLemma . config $ m) 
                            $ BS.getContents
                 BS.putStr 
                       . Text.unlines 
                       . map Text.unlines 
                       . predict m
                       $ testdat
    _ -> hPutStrLn stderr "Invalid command"

