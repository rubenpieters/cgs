module SharedData where


import Control.Monad.Eff.Exception
import Data.Either
import Data.Foldable
import Data.Foreign
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (toUnfoldable, fromFoldable, uncons, cons, head, tail)
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (genericEncode, encodeJSON, genericDecode, decodeJSON)
import Data.Foreign.Generic as DFG
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (NonEmptyList(..))
import EMap as M
--import Data.Map as M
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
instance encodeFaceDir :: Encode FaceDir
  where encode = genericEncode $ DFG.defaultOptions
instance decodeFaceDir :: Decode FaceDir
  where decode = genericDecode $ DFG.defaultOptions

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
instance encodeCard :: Encode Card
  where encode = genericEncode $ DFG.defaultOptions
instance decodeCard :: Decode Card
  where decode = genericDecode $ DFG.defaultOptions

cardTexture :: Card -> TextureLoc
cardTexture (Card c) = case c.faceDir of
  FaceUp -> c.textureFront
  FaceDown -> c.textureBack
--cardTexture c | c.faceDir == FaceUp = c.textureFront
--cardTexture c | c.faceDir == FaceDown = c.textureBack

-- pack's global identifier
type Gid = Int

-- pack's board position
data Position = Pos
  { x :: Int
  , y :: Int
  }

derive instance genericPosition :: Rep.Generic Position _
instance encodePosition :: Encode Position
  where encode = genericEncode $ DFG.defaultOptions
instance decodePosition :: Decode Position
  where decode = genericDecode $ DFG.defaultOptions

data Pack = Pack
  { gid :: Gid
  , cards :: Array Card
  , position :: Position
  }

derive instance genericPack :: Rep.Generic Pack _
instance encodePack :: Encode Pack
  where encode = genericEncode $ DFG.defaultOptions
instance decodePack :: Decode Pack
  where decode = genericDecode $ DFG.defaultOptions

-- frontCard :: Pack -> Card
-- packSize :: Pack -> Int

-- GAME STATE

-- gamestate which is shared by every client
-- needs to be obfuscated to hide asymmetric information
data SharedGameState = SharedGameState
  { cardsByGid :: M.EMap Gid Pack
  }

emptyGameState :: SharedGameState
emptyGameState = SharedGameState {cardsByGid : M.empty}

derive instance genericSharedGameState :: Rep.Generic SharedGameState _
instance encodeSharedGameState :: Encode SharedGameState
  where encode = genericEncode $ DFG.defaultOptions
instance decodeSharedGameState :: Decode SharedGameState
  where decode = genericDecode $ DFG.defaultOptions

-- obfuscateSharedState :: SharedGameState -> ObfuscatedState

type ObfuscatedGameState = SharedGameState

data SharedGameEvent
  -- flips the direction of the top card in a pack
  = FlipTop Gid
  -- move pack to specified position
  | MoveTo Position Gid

derive instance genericSharedGameEvent :: Rep.Generic SharedGameEvent _
instance encodeSharedGameEvent :: Encode SharedGameEvent
  where encode = genericEncode $ DFG.defaultOptions
instance decodeSharedGameEvent :: Decode SharedGameEvent
  where decode = genericDecode $ DFG.defaultOptions

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

-- GAME EVENT

data GameEvent = Select Gid | Gather | Remove Gid | Flip Gid

derive instance genericGameEvent :: Rep.Generic GameEvent _
instance encodeGameEvent :: Encode GameEvent
  where encode = genericEncode $ DFG.defaultOptions
instance decodeGameEvent :: Decode GameEvent
  where decode = genericDecode $ DFG.defaultOptions
instance gameEventShow :: Show GameEvent
  where show = genericShow



-- messages server -> client
data ServerMessage
  = PlayerId { id :: Int }
  --ConfirmJoin { assignedId :: Int, serverGameState :: ObfuscatedGameState }
  | NewPlayer { id :: Int }
  | SvMoveGid { id :: Int, x :: Int, y :: Int }
  | ConfirmUpdates { events :: Array GameEvent }

derive instance genericServerMessage :: Rep.Generic ServerMessage _
instance encodeServerMessage :: Encode ServerMessage
  where encode = genericEncode $ DFG.defaultOptions
instance decodeServerMessage :: Decode ServerMessage
  where decode = genericDecode $ DFG.defaultOptions
instance showServerMessage :: Show ServerMessage
  where show = genericShow

-- messages client -> server
data ClientMessage
  = ClMoveGid { id :: Int, x :: Int, y :: Int }
  | ClGameStateUpdate { events :: Array GameEvent}

derive instance genericClientMessage :: Rep.Generic ClientMessage _
instance encodeClientMessage :: Encode ClientMessage
  where encode = genericEncode $ DFG.defaultOptions
instance decodeClientMessage :: Decode ClientMessage
  where decode = genericDecode $ DFG.defaultOptions
instance showClientMessage :: Show ClientMessage
  where show = genericShow

-- decode utility (move somewhere else?)

decodeJSONEither :: ∀ a. Decode a => String -> Either (NonEmptyList ForeignError) a
decodeJSONEither = decodeJSON >>> runExceptT >>> unwrap

unsafeDecodeJSON :: ∀ e a. Decode a => String -> Eff e a
unsafeDecodeJSON s = do
  case (decodeJSONEither s) of
    -- TODO: print decoding errors
    -- TODO: use a logging effect?
    (Left errList) -> unsafeThrowException (error "error decoding")
    (Right value) -> pure value
