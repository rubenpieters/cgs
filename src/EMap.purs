module EMap where
-- newtype alias for Map, EMap
-- EMap has an Encode/Decode instance, since there is no default one

import Prelude

import Data.Either (note)
import Data.Foreign
import Data.Foreign.Class (encode, decode, class Encode, class Decode)
import Data.Foreign.Internal (readStrMap)
import Data.Int (fromString)
import Data.List.NonEmpty (singleton)
import Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Newtype hiding (traverse)
import Data.Semigroup
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Control.Monad.Except.Trans (except)

newtype EMap k v = EMap (M.Map k v)

derive instance newtypeEMap :: Newtype (EMap k v) _

-- Decode/Encode key

class DecodeKey k where
  decodeKey :: String -> Maybe k

instance intDecodeKey :: DecodeKey Int where
  decodeKey = fromString

class EncodeKey k where
  encodeKey :: k -> String

instance intEncodeKey :: EncodeKey Int where
  encodeKey = show

-- Decode/Encode EMap

instance emapDecode :: (Ord k, DecodeKey k, Decode v) => Decode (EMap k v) where
  decode = map wrap
           <<< map (M.fromFoldable :: Array (Tuple k v) -> M.Map k v)
           <<< traverse decodeTuple
           <=< map (StrMap.toUnfoldable :: StrMap.StrMap Foreign -> Array (Tuple String Foreign))
           <<< readStrMap
    where
      decodeTuple :: Ord k => DecodeKey k => Decode v
                  => Tuple String Foreign -> F (Tuple k v)
      decodeTuple (Tuple k v) = do
        decodedV <- decode v
        decodedK <- except $ note (singleton $ ErrorAtProperty k (ForeignError "Cannot decode key")) (decodeKey k)
        pure $ Tuple decodedK decodedV

instance emapEncode :: (EncodeKey k, Encode v) => Encode (EMap k v) where
  encode = toForeign
           <<< StrMap.fromFoldable
           <<< map (\(Tuple k v) -> (Tuple (encodeKey k) (encode v)))
           <<< (M.toUnfoldable :: M.Map k v -> Array (Tuple k v))
           <<< unwrap

-- reexport Map functions, but for EMap

empty :: forall k v. EMap k v
empty = wrap M.empty

lookup :: forall k v. Ord k => k -> EMap k v -> Maybe v
lookup k m = M.lookup k (unwrap m)

update :: forall k v. Ord k => (v -> Maybe v) -> k -> EMap k v -> EMap k v
update f k m = wrap $ M.alter (maybe Nothing f) k (unwrap m)
