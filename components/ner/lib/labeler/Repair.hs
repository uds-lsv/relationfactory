module Repair (repair, repairMany, Priority(..))
where
import System
import Utils
import Data.List (transpose)
import Debug.Trace

data Tag = B String
         | I String
         | O deriving (Show)

data Priority = Order | Type deriving (Show,Read)

typ (B str) = str
typ (I str) = str
typ O = ""


toString (B str) = "B-" ++ str
toString (I str) = "I-" ++ str
toString O       = "O"

toTag :: String -> Tag
toTag x = case splitOn '-' x of
            ("B":str:_) -> B str
            ("I":str:_) -> I str
            ("O":_)     -> O



repair =   map toString
         . repairMany Order 
         . map toTag

repairOne :: Priority -> String -> Tag -> Tag
--repairOne p t x | trace (show (t,x)) False = undefined
repairOne _ _     (B x) = B x
repairOne p t (I x) | t == x    = I x
                    | otherwise = case t of
                                    "" -> B x
                                    t  -> case p of 
                                            Order -> I t
                                            Type ->  B x
repairOne p _ x = x


repairMany :: Priority -> [Tag] -> [Tag]
repairMany p xs = repairMany' p (typ O) xs

repairMany' p prev (x:xs) = let x' = repairOne p prev x 
                            in x' : repairMany' p (typ x') xs
repairMany' _ prev [] = []

repairSentence :: Priority -> [[String]] -> [[String]]
repairSentence p s = let st = transpose s 
                         tags = last st :: [String]
                         toks = init st :: [[String]]
                     in  transpose $ toks ++ [map toString . repairMany p . map toTag $ tags]

main = do
  [p] <- getArgs
  sentences <- fmap (map (repairSentence (read p)) . splitWith null . map words . lines) getContents
  mapM_ (putStrLn . unlines . map unwords) sentences
  