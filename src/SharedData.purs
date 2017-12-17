module SharedData where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (class DecodeLiteral, decodeLiteralSumWithTransform, genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (class EncodeLiteral, encodeLiteralSumWithTransform, genericEncodeJson)
import Data.Either
import Data.Foldable
import Data.Foreign
import Data.Maybe
import Data.Traversable
import Data.Tuple

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array
import Data.Foreign.Generic as DFG
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList(..))
--import EMap as M
import Data.Map as M
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))

import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)

type Player =
  { playerId :: String
  , displayName :: String
  }

-- location for texture
-- TODO: currently assumed all textures have String identifier and are loaded by Phaser
type TextureLoc = String

-- direction of card
data FaceDir = FaceUp | FaceDown

derive instance eqFaceDir :: Eq FaceDir
derive instance genericFaceDir :: Rep.Generic FaceDir _
{-instance encodeFaceDir :: Encode FaceDir
  where encode = genericEncode $ DFG.defaultOptions
instance decodeFaceDir :: Decode FaceDir
  where decode = genericDecode $ DFG.defaultOptions
-}
instance encodeJsonFaceDir :: EncodeJson FaceDir
  where encodeJson = genericEncodeJson
instance decodeJsonFaceDir :: DecodeJson FaceDir
  where decodeJson = genericDecodeJson
instance showFaceDir :: Show FaceDir
  where show = genericShow

oppositeDir :: FaceDir -> FaceDir
oppositeDir FaceUp = FaceDown
oppositeDir FaceDown = FaceUp

-- represent a singular card
-- is always part of a pack
data Card = Card
  -- texture for card front
  { textureFront :: TextureLoc
  -- texture for card back
  , textureBack :: TextureLoc
  -- direction of card (Up/Down)
  , faceDir :: FaceDir
  }

derive instance genericCard :: Rep.Generic Card _
instance encodeJsonCard :: EncodeJson Card
  where encodeJson = genericEncodeJson
instance decodeJsonCard :: DecodeJson Card
  where decodeJson = genericDecodeJson
instance showCard :: Show Card
  where show = genericShow

cardTexture :: Card -> TextureLoc
cardTexture (Card c) = case c.faceDir of
  FaceUp -> c.textureFront
  FaceDown -> c.textureBack
--cardTexture c | c.faceDir == FaceUp = c.textureFront
--cardTexture c | c.faceDir == FaceDown = c.textureBack

-- player global identifier
type PlayerId = Int

-- pack's global identifier
type Gid = Int

-- pack's board position
data Position = Pos
  { x :: Int
  , y :: Int
  }

derive instance genericPosition :: Rep.Generic Position _
instance encodeJsonPosition :: EncodeJson Position
  where encodeJson = genericEncodeJson
instance decodeJsonPosition :: DecodeJson Position
  where decodeJson = genericDecodeJson
instance showPosition :: Show Position
  where show = genericShow

data Pack = Pack
  { gid :: Gid
  , cards :: Array Card
  , position :: Position
--  , lockedBy :: Maybe PlayerId
  }

derive instance genericPack :: Rep.Generic Pack _

instance encodeJsonPack :: EncodeJson Pack
  where encodeJson = genericEncodeJson
instance decodeJsonPack :: DecodeJson Pack
  where decodeJson = genericDecodeJson
instance showPack :: Show Pack
  where show = genericShow

-- frontCard :: Pack -> Card
-- packSize :: Pack -> Int

-- GAME STATE

-- gamestate which is shared by every client
-- needs to be obfuscated to hide asymmetric information
data SharedGameState = SharedGameState
  { cardsByGid :: M.Map Gid Pack
  }

emptyGameState :: SharedGameState
emptyGameState = SharedGameState {cardsByGid : M.empty}

derive instance genericSharedGameState :: Rep.Generic SharedGameState _
instance encodeJsonSharedGameState :: EncodeJson SharedGameState
  where encodeJson = genericEncodeJson
instance decodeJsonSharedGameState :: DecodeJson SharedGameState
  where decodeJson = genericDecodeJson
instance showSharedGameState :: Show SharedGameState
  where show = genericShow

-- TODO: create obfuscation of gamestate

-- obfuscateSharedState :: SharedGameState -> ObfuscatedState

type ObfuscatedGameState = SharedGameState

data SharedGameEvent
  -- flips the direction of the top card in a pack
  = FlipTop Gid
  -- move pack to specified position
  | MoveTo Position Gid

derive instance genericSharedGameEvent :: Rep.Generic SharedGameEvent _
instance encodeJsonSharedGameEvent :: EncodeJson SharedGameEvent
  where encodeJson = genericEncodeJson
instance decodeJsonSharedGameEvent :: DecodeJson SharedGameEvent
  where decodeJson = genericDecodeJson
instance showSharedGameEvent :: Show SharedGameEvent
  where show = genericShow

updateSGE :: SharedGameEvent -> SharedGameState -> SharedGameState
updateSGE (FlipTop gid) gs = onGid gid flipTop gs
updateSGE (MoveTo l gid) gs = onGid gid (moveTo l) gs

onGid :: Gid
      -> (Pack -> Pack)
      -> SharedGameState
      -> SharedGameState
onGid gid f (SharedGameState gs) = case M.lookup gid gs.cardsByGid of
    Just (pack :: Pack) -> SharedGameState gs { cardsByGid = M.update (Just <<< f) gid gs.cardsByGid}
    -- TODO: log unexpected gid somewhere?
    Nothing -> SharedGameState gs

flipTop :: Pack -> Pack
flipTop (Pack p) = Pack $ case head p.cards of
  Just topCard -> p { cards = cons (flipCard topCard) otherCards }
  Nothing -> p
  where
    otherCards = case tail p.cards of
      Just o -> o
      Nothing -> []

flipCard :: Card -> Card
flipCard (Card c) = Card $ c { faceDir = oppositeDir c.faceDir }

moveTo :: Position -> Pack -> Pack
moveTo (Pos l) (Pack p) = Pack $ p { position = Pos l }

drawFromPack :: Pack -> Int -> {remaining :: Array Card, drawn :: Array Card}
drawFromPack _ x | x <= 0 = unsafeThrowException (error "drawing <= 0")
drawFromPack (Pack p) n = { remaining : remaining, drawn : drawn }
  where
    remaining = drop n p.cards
    drawn = take n p.cards



-- GAME EVENT

data GameEvent = Select Gid
               | Gather
               | Remove Gid
               | Flip Gid
               | Lock Gid
               | Draw Int Gid

derive instance genericGameEvent :: Rep.Generic GameEvent _
instance encodeJsonGameEvent :: EncodeJson GameEvent
  where encodeJson = genericEncodeJson
instance decodeJsonGameEvent :: DecodeJson GameEvent
  where decodeJson = genericDecodeJson
instance gameEventShow :: Show GameEvent
  where show = genericShow



-- messages server -> client
data ServerMessage
  = ConfirmJoin { assignedId :: Int, roomGameState :: ObfuscatedGameState }
  | NewPlayer { id :: Int }
  | SvMoveGid { id :: Int, x :: Int, y :: Int }
  | ConfirmUpdates { events :: Array GameEvent }

derive instance genericServerMessage :: Rep.Generic ServerMessage _
instance encodeJsonServerMessage :: EncodeJson ServerMessage
  where encodeJson = genericEncodeJson
instance decodeJsonServerMessage :: DecodeJson ServerMessage
  where decodeJson = genericDecodeJson
instance showServerMessage :: Show ServerMessage
  where show = genericShow

-- messages client -> server
data ClientMessage
  = ClMoveGid { id :: Int, x :: Int, y :: Int }
  | ClGameStateUpdate { events :: Array GameEvent}

derive instance genericClientMessage :: Rep.Generic ClientMessage _
instance encodeJsonClientMessage :: EncodeJson ClientMessage
  where encodeJson = genericEncodeJson
instance decodeClientMessage :: DecodeJson ClientMessage
  where decodeJson = genericDecodeJson
instance showClientMessage :: Show ClientMessage
  where show = genericShow

