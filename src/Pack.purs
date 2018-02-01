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

dropAt :: {x :: Int, y :: Int} -> Pack -> Pack
dropAt {x: x, y: y} (Pack p) = Pack $ p { position= OnBoard {x: x, y: y} }

packToHand :: PlayerId -> Pack -> Pack
packToHand pid (Pack p) = Pack $ p { position= InHandOf {pid: pid} }

--drawFromPack :: Int -> Pack -> {remaining :: Array Card, drawn :: Array Card}
drawFromPack x p | x <= 0 =
  { remaining: p.cards, drawn: []}
drawFromPack n p =
  { remaining: p.cards # drop n, drawn: p.cards # take n }

--lockPack :: PlayerId -> Pack -> Pack
lockPack pid p = p { lockedBy= Just pid }

unlockPack :: Pack -> Pack
unlockPack (Pack p) = Pack $ p { lockedBy= Nothing }

dropPack :: PlayerId -> Pack -> Pack
dropPack pid (Pack p) = case Tuple p.position p.lockedBy of
  Tuple (InHandOf {pid: pid'}) _ | pid == pid' ->
    Pack $ p {position= OnBoard {x: 0, y: 0}, lockedBy= Nothing}
  Tuple _ (Just pid') | pid == pid' ->
    Pack $ p {position= OnBoard {x: 0, y: 0}, lockedBy= Nothing}
  _ ->
    Pack p

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

--flipTop :: Pack -> Pack
flipTop p = case uncons p.cards of
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
