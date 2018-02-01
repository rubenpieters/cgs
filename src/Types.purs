module Types (module Types, module Exported) where

import Prelude as Exported

import Data.Either as Exported
import Data.Maybe as Exported
import Data.Tuple as Exported

import Control.Monad.Eff as Exported

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)

-- pack's global identifier
type Gid = Int

-- player global identifier
type PlayerId = Int

-- location for texture
-- TODO: currently assumed all textures have String identifier and are loaded by Phaser
type TextureLoc = String

-- direction which card is facing
-- Up: front texture should be shown
-- Down: back texture should be shown
data FaceDir = FaceUp | FaceDown

-- represent a singular card
-- is always part of a pack
data Card = Card
  -- texture for card front
  { textureFront :: TextureLoc
  -- texture for card back
  , textureBack :: TextureLoc
  -- direction of card (Up/Down)
  , faceDir :: FaceDir
  -- card text
  , cardText :: String
  }

type PackData r =
  { gid :: Gid
  , cards :: Array Card
  , lockedBy :: Exported.Maybe PlayerId
  | r
  }

-- instances

derive instance genericFaceDir :: Rep.Generic FaceDir _
instance encodeJsonFaceDir :: EncodeJson FaceDir
  where encodeJson = genericEncodeJson
instance decodeJsonFaceDir :: DecodeJson FaceDir
  where decodeJson = genericDecodeJson
instance showFaceDir :: Exported.Show FaceDir
  where show = genericShow

derive instance genericCard :: Rep.Generic Card _
instance encodeJsonCard :: EncodeJson Card
  where encodeJson = genericEncodeJson
instance decodeJsonCard :: DecodeJson Card
  where decodeJson = genericDecodeJson
instance showCard :: Exported.Show Card
  where show = genericShow
