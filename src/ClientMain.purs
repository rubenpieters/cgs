module ClientMain where

import SharedData

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Array (toUnfoldable, fromFoldable, uncons, cons)
import Data.List hiding (null, length)
import Data.Map as M
import Data.Foldable
import Data.Traversable

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Exception.Unsafe (unsafeThrowException)
import Control.Monad.Except.Trans (runExceptT)

import Data.Either
import Data.List.NonEmpty (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic as DFG
import Data.Foreign.Generic (genericEncode, encodeJSON, genericDecode, decodeJSON)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)


main :: forall e. Eff e Unit
main = pure unit

logTest :: forall e. Eff (console :: CONSOLE | e) Unit
logTest = do
  log "Hello sailor!"

clamp :: Int -> {lBound :: Int, uBound :: Int} -> Int
clamp x b | x > b.uBound = b.uBound
clamp x b | x < b.lBound = b.lBound
clamp x _ = x

{-
data DragMode = Drag | Draw

derive instance eqDragMode :: Eq DragMode
derive instance ordDragMode :: Ord DragMode

nextDragMode :: DragMode -> DragMode
nextDragMode Drag = Draw
nextDragMode Draw = Drag
-}

foreign import data PHASER :: Effect
foreign import data PhCard :: Type

foreign import phMkCard :: ∀ e. {x :: Int, y :: Int, pack :: Array Card}
                        -> Eff (ph :: PHASER | e) PhCard
--foreign import cardInfo :: PhCard -> CardInfo
foreign import packInfo :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Packx
foreign import phaserProps :: ∀ e. PhCard -> Eff (ph :: PHASER | e) PhaserProps
foreign import toggleSelected :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
foreign import showCardSelectMenu :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
foreign import hideCardSelectMenu :: ∀ e. Eff (ph :: PHASER | e) Unit
foreign import checkOverlap :: PhCard -> PhCard -> Boolean
foreign import gameState :: ∀ e. Eff (ph :: PHASER | e) GameState
foreign import updateDraggedCard :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
foreign import isConnected :: ∀ e. Eff (ph :: PHASER | e) Boolean
foreign import moveCard :: ∀ e. Int -> Int -> ClPack -> Eff (ph :: PHASER | e) Unit
foreign import setTint :: ∀ e. PhCard -> Int -> Eff (ph :: PHASER | e) Unit
--foreign import setCardInfo :: PhCard -> CardInfo -> Eff (ph :: PHASER) Unit
foreign import updateCardInfo :: ∀ e r. PhCard -> { | r } -> Eff (ph :: PHASER | e) Unit
foreign import updatePackText :: ∀ e. PhCard -> String -> Eff (ph :: PHASER | e) Unit

foreign import phKill :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
foreign import phLoadTexture :: ∀ e. ClPack -> String -> Int -> Boolean -> Eff (ph :: PHASER | e) Unit


updateCardSelectMenu :: ∀ e. Eff (ph :: PHASER | e) Unit
updateCardSelectMenu = do
  gs <- gameState
  sm <- selectMode gs
  case sm of
    (Single c) -> showCardSelectMenu c
    Other -> hideCardSelectMenu

data SelectMode = Single PhCard | Other

selectMode :: ∀ e. GameState -> Eff (ph :: PHASER | e) SelectMode
selectMode gs = do
  f <- filtered
  pure case f of
    (c:Nil) -> Single c
    _ -> Other
  where
    filtered = filterM (\c -> isSelected c) (M.values gs.cards)

isSelected :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Boolean
isSelected c = do
  pi <- packInfo c
  pure pi.selected

isDragging :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Boolean
isDragging c = do
  pi <- packInfo c
  pure pi.dragging

type Packx =
  { pack :: Array Card
  , packText :: String
  , gid :: Int
  , selected :: Boolean
  , dragging :: Boolean
  , overlapped :: Boolean
  }

{-
type Card =
  { texture :: String
  , textureBack :: String
  , faceDir :: FaceDir
  }
-}

--newCard :: Card
--newCard = { texture: "card", textureBack: "empty", faceDir: FaceUp }

type PhaserProps =
  { x :: Int
  , y :: Int
  }

type Cid = Int

showGameEvent :: GameEvent -> String
showGameEvent = show

encodeGE :: GameEvent -> String
encodeGE = encodeJson >>> stringify

decodeEither :: ∀ a. Decode a => String -> Either (NonEmptyList ForeignError) a
decodeEither s = unwrap $ runExceptT $ decodeJSON s

unsafeDecode :: ∀ e a. Decode a => String -> Eff (console :: CONSOLE | e) a
unsafeDecode s = do
  case (decodeEither s) of
    (Left errList) -> unsafeThrowException (error "error decoding GameEvent")
    (Right value) -> pure value

encodeGEA :: Array GameEvent -> String
encodeGEA = encodeJson >>> stringify

type GameState =
  { cards :: M.Map Cid PhCard
  }

emptyGS :: GameState
emptyGS = { cards : M.empty }

data LockStatus = LockedBySelf | LockedByOther | NotLocked

lockStatus :: ClPack -> Eff _ LockStatus
lockStatus c = do
  props <- c # getProps
  -- TODO: get player id and check with that instead of `1`
  let res = case (props.lockedBy) of
              Just playerId -> if playerId == 1
                then LockedBySelf
                else LockedByOther
              Nothing -> NotLocked
  pure res

lockStatusGid :: Gid -> Eff _ LockStatus
lockStatusGid gid = onCard' gid lockStatus

cardLocked :: Gid -> Eff _ Boolean
cardLocked gid = do
  lockStatus <- lockStatusGid gid
  case lockStatus of
    LockedBySelf -> pure true
    LockedByOther -> pure true
    NotLocked -> pure false

updateGameState :: ∀ e. Array GameEvent -> Eff (ph :: PHASER | e) Unit
updateGameState es = do
  -- handle events
  traverse_ update es
  -- update cards
--  updateCards
  where
    update :: ∀ e. GameEvent -> Eff (ph :: PHASER | e) Unit
    --update (Select cid) = selectCard cid
    --update Gather = gatherCards
    --update (Remove cid) = removeCard cid
    update (Select gid) = unsafeThrowException (error "unimplemented")
    update Gather = unsafeThrowException (error "unimplemented")
    update (Remove gid) = unsafeThrowException (error "unimplemented")
    update (Flip gid) = onCard gid flipCard
    update (Lock gid) = onCard gid lockCard
    update (Draw _ _) = unsafeThrowException (error "unimplemented")

updateCards :: ∀ e. Eff (ph :: PHASER | e) Unit
updateCards = do
  gs <- gameState
  traverse_ updateCard gs.cards

updateCard :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
updateCard c = do
  d <- isDragging c
  if d
     then do
            clearOverlaps
            updateDraggedCard c
            connected <- isConnected
            if connected
               then do
                    socket <- getSocket
                    props <- phaserProps c
                    pi <- packInfo c
                    socket `emit` (ClMoveGid {id: pi.gid, x: props.x, y: props.y})
               else pure unit
            mOverlap <- findFirstOverlapCard c
            case mOverlap of
              Just overlap -> do
                updateCardInfo overlap { overlapped: true }
                setTint overlap 0xff0000
              Nothing -> pure unit
     else pure unit

clearOverlaps :: ∀ e. Eff (ph :: PHASER | e) Unit
clearOverlaps = do
  gs <- gameState
  traverse_ clearOverlap gs.cards
  where
    clearOverlap :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
    clearOverlap c = do
      updateCardInfo c { overlapped: false }
      setColor c

findFirstOverlapCard :: ∀ e. PhCard -> Eff (ph :: PHASER | e) (Maybe PhCard)
findFirstOverlapCard c = do
  xp <- packInfo c
  gs <- gameState
  let cards = M.values gs.cards
  p1 <- traverse packInfo cards
  let (dropped :: List PhCard) = snd <$> dropWhile (\x -> overlapCondition x (Tuple xp c)) (zip p1 cards)
  pure $ head dropped
  where
    overlapCondition :: Tuple Packx PhCard -> Tuple Packx PhCard -> Boolean
    overlapCondition (Tuple p1 _) (Tuple p2 _) | p1.gid == p2.gid = true
    overlapCondition (Tuple _ c1) (Tuple _ c2) | checkOverlap c1 c2 = false
    overlapCondition _ _ = true

{-
selectCard :: ∀ e. Cid -> Eff (ph :: PHASER | e) Unit
selectCard cid = do
  onCard cid selectCard'
  --onCard cid toggleSelected
  updateCardSelectMenu

selectCard' :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
selectCard' c = do
  d <- isDragging c
  s <- isSelected c
  selectAction c {dragging: d, selected: s}
-}

selectAction :: ∀ e. PhCard -> {dragging :: Boolean, selected :: Boolean} -> Eff (ph :: PHASER | e) Unit
selectAction c p | p.dragging = do
  updateCardInfo c { dragging: false}
selectAction c p | p.selected = do
  updateCardInfo c {selected: false}
  setColor c
selectAction c p | not (p.selected) = do
  updateCardInfo c {selected: true}
  setColor c
selectAction c p = pure unit

setColor :: ∀ e. PhCard -> Eff (ph :: PHASER | e) Unit
setColor c = do
  pi <- packInfo c
  setTint c (packTint pi)

packTint :: Packx -> Int
packTint p | p.selected = 0x00ff00
packTint p = 0xffffff

{-
toggleSelectProps :: Packx -> {dragging :: Boolean, selected :: Boolean, tint :: String}
toggleSelectProps p | p.dragging = { dragging: false, selected: p.selected, tint: "" }
toggleSelectProps p | p.selected = { selected: false }
toggleSelectProps p | not p.selected = { selected: true }
toggleSelectedProps p = p
-}

flipCard :: ClPack -> Eff _ Unit
flipCard c = do
  props <- c # getProps
  case (uncons props.cards) of
        Just { head: (Card firstCard), tail: t} -> do
          case firstCard.faceDir of
            FaceUp -> do
              let (newCards :: Array Card) = (cons (Card $ firstCard {faceDir=FaceDown}) t)
              let (newProps :: PackProps) = (props { cards = newCards})
              c # setProps newProps
              phLoadTexture c firstCard.textureBack 0 false
            FaceDown -> do
              let (newCards :: Array Card) = (cons (Card $ firstCard {faceDir=FaceUp}) t)
              let (newProps :: PackProps) = (props { cards = newCards})
              c # setProps newProps
              phLoadTexture c firstCard.textureFront 0 false
        -- if pack is empty, flip does nothing
        Nothing -> pure unit

lockCard :: ClPack -> Eff _ Unit
lockCard c = do
  props <- c # getProps
  let (newProps :: PackProps) = (props { lockedBy = Just 1 })
  c # setProps newProps

onCard :: ∀ e. Gid
       -> (ClPack -> Eff (ph :: PHASER | e) Unit)
       -> Eff (ph :: PHASER | e) Unit
onCard gid f = do
  (LocalGameState gs) <- getGameState
  let (mc :: Maybe ClPack) = M.lookup gid gs.cardsByGid
  case mc of
    Just (c :: ClPack) -> f c
    Nothing -> pure unit

onCard' :: ∀ a e. Gid
       -> (ClPack -> Eff (ph :: PHASER | e) a)
       -> Eff (ph :: PHASER | e) a
onCard' gid f = do
  (LocalGameState gs) <- getGameState
  let (mc :: Maybe ClPack) = M.lookup gid gs.cardsByGid
  case mc of
    Just (c :: ClPack) -> f c
    Nothing -> unsafeThrowException (error ("card " <> show gid <> " does not exist"))

{-addNewCard :: GameState -> Eff (ph :: PHASER) GameState
addNewCard gs =
  do
    c <- phMkCard { x: 1, y: 1, pack: [{texture: "card"}]}
    pure $ addCard c gs
-}

addCard :: ∀ e. PhCard -> GameState -> Eff (ph :: PHASER | e) GameState
addCard c gs = do
  pi <- packInfo c
  pure gs { cards = M.insert pi.gid c gs.cards }

-- TODO: keep track of killed cards, so they can be revived
removeCardGS :: ∀ e. PhCard -> GameState -> Eff (ph :: PHASER | e) GameState
removeCardGS c gs = do
  pi <- packInfo c
  pure gs { cards = M.delete pi.gid gs.cards }

averagePos :: ∀ p f. Foldable f
           => Functor f
           => f {x :: Int, y :: Int | p}
           -> {x :: Int, y :: Int}
averagePos l =
  let {tx:totx, ty:toty, l:length} = foldr (\a {tx:tx,ty:ty,l:l} -> {tx:tx+a.x,ty:ty+a.y,l:l+1}) {tx:0,ty:0,l:0} l in
  {x: totx/length, y: toty/length}

gatherCards :: ∀ e. Eff (ph :: PHASER | e) Unit
gatherCards = do
  gs <- gameState
  selectedCards :: List PhCard <- filterM isSelected (M.values gs.cards)
  if (length selectedCards) <= 1
     then pure unit
     else do
            props <- traverse phaserProps selectedCards
            let avgPos = averagePos props
            packs <- traverse packInfo selectedCards
            let (cards :: List Card) = concat $ (arrayToList <<< _.pack) <$> packs
            c <- phMkCard {x: avgPos.x, y: avgPos.y, pack: listToArray cards}
            -- add all cards to c
            traverse_ phKill selectedCards
            pure unit

arrayToList :: ∀ a. Array a -> List a
arrayToList = toUnfoldable

listToArray :: ∀ a. List a -> Array a
listToArray = fromFoldable

{-
removeCard :: ∀ e. Cid -> Eff (ph :: PHASER | e) Unit
removeCard cid = onCard cid phKill
-}

drawFromPack :: ∀ e. {x :: Int, y :: Int} -> PhCard -> Eff (ph :: PHASER | e) PhCard
drawFromPack {x: x, y: y} c = do
  pi <- packInfo c
  case (uncons pi.pack) of
    Just { head: firstCard, tail: t} -> do
      updateCardInfo c { pack: t }
      updatePackText c (show $ (length t :: Int))
      newPack <- phMkCard {x: x, y:y, pack: [firstCard]}
      updateCardInfo newPack { dragging: true }
      pure newPack
    Nothing -> unsafeThrowException (error "drawing from pack of size 1")


-- server interacting code, move to new module (client/server) ?

--foreign import data NETWORK :: Effect
foreign import data Socket :: Type

foreign import getSocket :: ∀ e. Eff (ph :: PHASER | e) Socket
foreign import unsafeEmit :: ∀ e. Socket -> String -> Eff (ph :: PHASER | e) Unit

emit :: ∀ e msg. (EncodeJson msg) => Socket -> msg -> Eff (ph :: PHASER | e) Unit
emit socket msgStr = unsafeEmit socket (stringify $ encodeJson msgStr)

sendUpdates :: ∀ e. Socket -> Array GameEvent-> Eff (ph :: PHASER | e) Unit
sendUpdates socket events = emit socket (ClGameStateUpdate { events: events })

{-
data NetworkMsg
  = NewPlayer {}
  | RemovePlayer {}
  | MoveGid { gid :: Int, x :: Int, y :: Int}

  clients send this message to notify the server that they want to update the shared gamestate

  | GameStateUpdate { events :: Array GameEvent}

networkMsgString :: NetworkMsg -> String
networkMsgString (NewPlayer _) = "new player"
networkMsgString (RemovePlayer _) = "remove player"
networkMsgString (MoveGid _) = "move gid"
networkMsgString (GameStateUpdate _) = "gamestate update"


emit :: ∀ e. Socket -> NetworkMsg -> Eff (network :: NETWORK | e) Unit
emit socket msg@(NewPlayer d) = unsafeEmit socket (networkMsgString msg) d
emit socket msg@(RemovePlayer d) = unsafeEmit socket (networkMsgString msg) d
emit socket msg@(MoveGid d) = unsafeEmit socket (networkMsgString msg) d
emit socket msg@(GameStateUpdate d) = unsafeEmit socket (networkMsgString msg) d
-}

-- client on server message

onServerStrMessage :: ∀ e.
                      String -> Eff (console :: CONSOLE, ph :: PHASER | e) Unit
onServerStrMessage msg = do
  let eSvMsg = jsonParser msg >>= decodeJson
  case eSvMsg of
    Left errs -> log "malformed message"
    Right (svMsg :: ServerMessage) -> onServerMessage svMsg

onServerMessage :: ∀ e.
                   ServerMessage -> Eff (console :: CONSOLE, ph :: PHASER | e) Unit
onServerMessage (ConfirmJoin {assignedId: id, roomGameState: gs}) = do
  log ("assigned player id: " <> show id)
  clearPhaserState
  lgs <- materializeState gs
  setGameState lgs
onServerMessage (NewPlayer {id: id}) = do
  log ("new player connected: " <> show id)
onServerMessage (SvMoveGid {id: id, x: x, y: y}) = do
  log ("mov gid, id: " <> show id <> ", x: " <> show x <> ", y: " <> show y)
  onCard id (moveCard x y)
onServerMessage (ConfirmUpdates {events: events}) = do
  log ("confirmed updates: " <> show events)
  updateGameState events

foreign import data ClPack :: Type

type PackInfoX r =
  { gid :: Gid
  , cards :: Array Card
  , lockedBy :: Maybe PlayerId
  | r
  }

foreign import clearPhaserState :: ∀ e. Eff (ph :: PHASER | e) Unit

foreign import materializeCard :: ∀ e r. { x :: Int, y :: Int, texture :: String, size :: Int, pack :: PackInfoX r } -> Eff (ph :: PHASER | e) ClPack

foreign import getGameState :: ∀ e. Eff (ph :: PHASER | e) LocalGameState
foreign import setGameState :: ∀ e. LocalGameState -> Eff (ph :: PHASER | e) Unit

data LocalGameState = LocalGameState
  { cardsByGid :: M.Map Gid ClPack
  }

materializeCard' :: Pack -> Eff _ ClPack
materializeCard' (Pack pack) = do
  let create texture = materializeCard { texture : texture, x : pack.position # \(Pos p) -> p.x, y : pack.position # \(Pos p) -> p.y, size : length pack.cards, pack : pack }
  case uncons pack.cards of
    Just { head : card, tail : _} -> do
      create (card # cardTexture)
    Nothing -> do
      create "empty"

materializeState :: ObfuscatedGameState -> Eff _ LocalGameState
materializeState (SharedGameState gs) = do
  lgs <- traverse f gs.cardsByGid
  pure (LocalGameState {cardsByGid : lgs})
  where
    f pack = do
               clPack <- materializeCard' pack
--               pure { pack : pack, clPack : clPack }
               pure clPack

type PackProps =
  { selected :: Boolean
  , dragging :: Boolean
  , overlapped :: Boolean
  | PackInfo
  }

foreign import getProps :: ∀ e. ClPack -> Eff (ph :: PHASER | e) PackProps
foreign import setProps :: ∀ e. PackProps -> ClPack -> Eff (ph :: PHASER | e) Unit