import System.IO.UTF8 (interact)
import Prelude hiding (interact)
main = interact f

f :: String -> String
f = unlines . map (\ln -> let [w,c] = words ln in unwords [c,"0",w]) . lines