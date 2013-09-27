{-# LANGUAGE OverloadedStrings #-}
module Token (Token (..))
where
import Data.Monoid
import qualified Text as T
import Text (Txt)


data Token = Token { form  :: Txt
                   , lemma :: Txt
                   , pos   :: Txt
                   , chunk :: Txt } deriving (Show,Eq)
instance Monoid Token where
    mempty = Token { form = "", lemma = "" , pos = "" , chunk = "" }
    s `mappend` t = Token  (form  s `T.append` form  t)
                           (lemma s `T.append` lemma t)
                           (pos   s `T.append` pos   t)
                           (chunk s `T.append` chunk t)
