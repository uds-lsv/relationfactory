import System
import Text.Printf
main = do
  [f1,f2] <- getArgs
  xs <- fmap (map (map read) . map words . lines) (readFile f1)::IO [[Double]]
  ys <- fmap (map (map read) . map words . lines) (readFile f2)
  putStr 
       . unlines 
       . map (unwords . map (printf "%4.2f"))
       $ zipWith (zipWith f) xs ys

f hi lo = 100 * rer (hi/100) (lo/100)

rer hi lo = ((1-lo)-(1-hi))/(1-lo)
