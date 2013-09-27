module TextBinary ()
where
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding
import Data.Binary

instance Binary Text.Text where
    put t = put (encodeUtf8 t)
    get = decodeUtf8 `fmap` get
