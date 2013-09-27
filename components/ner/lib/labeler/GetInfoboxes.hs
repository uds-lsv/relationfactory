import Text.HTML.TagSoup
import Text.HTML.TagSoup.Parser
import Data.List 
import qualified Data.Set as Set

main :: IO ()
main = interact $ unlines
       . map toString
       . getInfoboxes
       . parseTags

getInfoboxes :: [Tag] -> [(String,[String])]
getInfoboxes tags = [ (t,i:is)
                    | page <- partitions (~== "<page>") tags
                    , Just t <- return $ title page        -- has title
                    , (i:is) <- return $ infoboxes page    -- has at least 1 infobox 
                    ]

title :: [Tag] -> Maybe String
title tags = case take 1 . sections (~== "<title>") $ tags of
               ((TagOpen "title" _:TagText t:TagClose "title" :_):_) -> Just t
               _ -> Nothing

infoboxes :: [Tag] -> [String]
infoboxes tags = 
    let trigger = "{{Infobox"
        len = length trigger + 1
    in case take 1 . sections (~== "<text>") $ tags of 
                   ((TagOpen "text" _:TagText t:_):_) -> 
                       uniq [ drop len . takeWhile (/= '\n') $ s'
                                | s <- sections ("{{Infobox" `isPrefixOf`) 
                                       . tails 
                                       $ t 
                                , (s':_) <- return s ]
                   _ -> []


uniq = Set.toList . Set.fromList

toString (w,is) = unlines $ w:is
                          