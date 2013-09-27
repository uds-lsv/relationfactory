import Utils (splitOn)

main = interact f

f str = 
    let ss = map (map parseWord) . splitOn "ID" . lines $ str
    in unlines . map format $ ss

format :: [(String,String,String)] -> String
format = unlines . map (\(w,t,st) -> unwords [w,t,st]) 

parseWord ln = 
    let ["<L",st,_,mt,w,_] = words ln
    in (w,mt,st)
