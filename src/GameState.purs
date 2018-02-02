module GameState where

import Types
import Pack

import Data.Record.Builder
import Data.Map as M
import Data.Traversable (traverse)

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)

import Control.Monad.State

-- gamestate which is stored on server
-- needs to be obfuscated to hide asymmetric information
data SharedGameState = SharedGameState
  { cardsByGid :: M.Map Gid Pack
  }

emptyGameState :: SharedGameState
emptyGameState = SharedGameState {cardsByGid : M.empty}

forCards :: ∀ t f a.
--            Unfoldable t =>
--            Traversable t =>
            Applicative f =>
            SharedGameState ->
            (Int -> Pack -> f a) ->
            f (Array a)
forCards (SharedGameState state) f = traverse (uncurry f) tupleList
  where
    tupleList = M.toUnfoldable state.cardsByGid

-- TODO: create obfuscation of gamestate

-- obfuscateSharedState :: SharedGameState -> ObfuscatedState

mapCards :: (Pack -> Pack) -> SharedGameState -> SharedGameState
mapCards f (SharedGameState gs) =
  SharedGameState $ gs {cardsByGid= gs.cardsByGid <#> f}

onGid :: Gid -> (Pack -> Pack) -> SharedGameState -> SharedGameState
onGid gid f (SharedGameState gs)= case M.lookup gid gs.cardsByGid of
    Just (pack :: Pack) -> SharedGameState gs { cardsByGid = M.update (Just <<< f) gid gs.cardsByGid}
    -- TODO: log unexpected gid somewhere?
    Nothing -> SharedGameState gs

forGid :: ∀ a. Gid -> (Pack -> a) -> SharedGameState -> Maybe a
forGid gid f (SharedGameState gs) = f <$> M.lookup gid gs.cardsByGid

setGid :: Gid -> Pack -> SharedGameState -> SharedGameState
setGid gid pack (SharedGameState gs) =
  SharedGameState (gs { cardsByGid= M.insert gid pack gs.cardsByGid })

removeGid :: Gid -> SharedGameState -> SharedGameState
removeGid gid (SharedGameState gs) =
  SharedGameState (gs { cardsByGid= M.delete gid gs.cardsByGid })

{-
                 , packByGid :: Gid -> f (Maybe pack)
                 , getPackData :: pack -> f (PackData r)
                 , setPackData :: (PackData r) -> pack -> f Unit
                 , createPack :: forall x. (PackData x) -> f Unit
                 , deletePack :: pack -> f Unit
                 , packToHand :: PlayerId -> pack -> f Unit
                 , dropAt :: { x :: Int, y :: Int } -> pack -> f Unit
-}

type PackDataOnServer =
  { gid :: Gid
  , cards :: Array Card
  , lockedBy :: Maybe PlayerId
  , position :: Position
  }

packByGid :: forall m. (MonadState SharedGameState m) => Gid -> m (Maybe Pack)
packByGid gid = do
  (SharedGameState gs) <- get
  pure $ M.lookup gid gs.cardsByGid

getPackData :: Pack -> PackDataOnServer
getPackData (Pack p) = p

mGetPackData :: forall f. (Applicative f) => Pack -> f PackDataOnServer
mGetPackData (Pack p) = pure p

setPackData :: forall m. (MonadState SharedGameState m) =>
  PackDataOnServer -> Pack -> m Unit
setPackData packData _ = do
  (SharedGameState gs) <- get
  put $ SharedGameState $
    gs { cardsByGid= M.insert packData.gid (Pack packData) gs.cardsByGid }

createPack :: forall m x. (MonadState SharedGameState m) =>
  (PackData x) -> m Unit
createPack packData = do
  (SharedGameState gs) <- get
  let packData' = { gid: packData.gid
                  , cards: packData.cards
                  , lockedBy: packData.lockedBy
                  }
  let fullPackData = build (merge packData') { position: OnBoard { x: 0, y: 0 } }
  put $ SharedGameState $
    gs { cardsByGid= M.insert packData.gid (Pack fullPackData) gs.cardsByGid }

deletePack :: forall m. (MonadState SharedGameState m) =>
  Pack -> m Unit
deletePack (Pack p) = do
  gs <- get
  put $ removeGid p.gid gs

packToHand' :: forall m. (MonadState SharedGameState m) =>
  PlayerId -> Pack -> m Unit
packToHand' pid (Pack p) = do
  gs <- get
  put $ onGid p.gid (packToHand pid) gs

-- instances

derive instance genericSharedGameState :: Rep.Generic SharedGameState _
instance encodeJsonSharedGameState :: EncodeJson SharedGameState
  where encodeJson = genericEncodeJson
instance decodeJsonSharedGameState :: DecodeJson SharedGameState
  where decodeJson = genericDecodeJson
instance showSharedGameState :: Show SharedGameState
  where show = genericShow
