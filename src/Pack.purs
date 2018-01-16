module Pack where

import Types

import Data.Array

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)

data Pack = Pack
  { position :: Position
  | PackInfo
  }

type PackInfo =
  ( gid :: Gid
  , cards :: Array Card
  , lockedBy :: Maybe PlayerId
  )

data Position
  = OnBoard { x :: Int, y :: Int}
  | InHandOf { pid :: PlayerId }

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

-- location for texture
-- TODO: currently assumed all textures have String identifier and are loaded by Phaser
type TextureLoc = String

-- direction which card is facing
-- Up: front texture should be shown
-- Down: back texture should be shown
data FaceDir = FaceUp | FaceDown

dropAt :: {x :: Int, y :: Int} -> Pack -> Pack
dropAt {x: x, y: y} (Pack p) = Pack $ p { position= OnBoard {x: x, y: y} }

packToHand :: PlayerId -> Pack -> Pack
packToHand pid (Pack p) = Pack $ p { position= InHandOf {pid: pid} }

drawFromPack :: Int -> Pack -> {remaining :: Array Card, drawn :: Array Card}
drawFromPack x (Pack p) | x <= 0 =
  { remaining: p.cards, drawn: []}
drawFromPack n (Pack p) =
  { remaining: p.cards # drop n, drawn: p.cards # take n }

lockPack :: PlayerId -> Pack -> Pack
lockPack pid (Pack p) = Pack $ p { lockedBy= Just pid }

unlockPack :: Pack -> Pack
unlockPack (Pack p) = Pack $ p { lockedBy= Nothing }

cardTexture :: Card -> TextureLoc
cardTexture (Card c) = case c.faceDir of
  FaceUp -> c.textureFront
  FaceDown -> c.textureBack

oppositeDir :: FaceDir -> FaceDir
oppositeDir FaceUp = FaceDown
oppositeDir FaceDown = FaceUp

mkCard :: TextureLoc -> TextureLoc -> FaceDir -> String -> Card
mkCard a b c d = Card {textureFront: a, textureBack: b, faceDir: c, cardText: d}

mkCardDown :: TextureLoc -> TextureLoc -> String -> Card
mkCardDown a b d = mkCard a b FaceDown d

flipCard :: Card -> Card
flipCard (Card c) = Card $ c { faceDir= oppositeDir c.faceDir }

flipTop :: Pack -> Pack
flipTop (Pack p) = Pack $ case uncons p.cards of
  Just {head: topCard, tail: tail} -> p { cards= cons (flipCard topCard) tail }
  Nothing -> p

-- instances

derive instance genericPack :: Rep.Generic Pack _
instance encodeJsonPack :: EncodeJson Pack
  where encodeJson = genericEncodeJson
instance decodeJsonPack :: DecodeJson Pack
  where decodeJson = genericDecodeJson
instance showPack :: Show Pack
  where show = genericShow

derive instance genericPosition :: Rep.Generic Position _
derive instance eqPosition :: Eq Position
instance encodeJsonPosition :: EncodeJson Position
  where encodeJson = genericEncodeJson
instance decodeJsonPosition :: DecodeJson Position
  where decodeJson = genericDecodeJson
instance showPosition :: Show Position
  where show = genericShow

derive instance genericCard :: Rep.Generic Card _
instance encodeJsonCard :: EncodeJson Card
  where encodeJson = genericEncodeJson
instance decodeJsonCard :: DecodeJson Card
  where decodeJson = genericDecodeJson
instance showCard :: Show Card
  where show = genericShow

derive instance genericFaceDir :: Rep.Generic FaceDir _
instance encodeJsonFaceDir :: EncodeJson FaceDir
  where encodeJson = genericEncodeJson
instance decodeJsonFaceDir :: DecodeJson FaceDir
  where decodeJson = genericDecodeJson
instance showFaceDir :: Show FaceDir
  where show = genericShow
