module Test.Expect where

import Prelude

import Data.Array
import Data.Boolean
import Data.Exists
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Data.Map as M

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

-- TODO: house this code in own repo

type E a = { f :: (a -> Boolean), m :: String, t :: String }

mkE :: forall a. (a -> Boolean) -> String -> String -> E a
mkE a b c = {f: a, m: b, t: c}

data Expect a
  = Expect (Array (E a)) (Expect a)
  | Verify (Array String) (Array a -> Boolean) (Expect a)
  | Done

isDone :: forall a. Expect a -> Boolean
isDone Done = true
isDone _ = false

expect12 =
  Expect [{f: (_ == 1), m: "1", t: "a"}, {f: (_ == 3), m: "2", t: "a"}] $
  Expect [{f: (_ == 2), m: "3", t: "a"}] $
  Done

expect3 =
  Expect [{f: (const true), m: "1", t: "a"}, {f: (const true), m: "2", t: "a"}] $
  Verify ["1", "2"] (\([x, y]) -> x == y) $
  Expect [{f: (_ == 2), m: "3", t: "a"}] $
  Done


data TestResult
  = Success String
  | Fail

data LayerResult a
  = Match
  | Wrong a
  | TooMuch a
  | VerificationFail a

instance showLayerResult :: Show a => Show (LayerResult a) where
  show (Match) = "Match"
  show (Wrong a) = "Wrong: " <> show a
  show (TooMuch a) = "TooMuch: " <> show a
  show (VerificationFail a) = "VerificationFail: " <> show a

instance eqLayerResult :: Eq a => Eq (LayerResult a) where
  eq Match Match = true
  eq (Wrong a) (Wrong b) | a == b = true
  eq (TooMuch a) (TooMuch b) | a == b = true
  eq (VerificationFail a) (VerificationFail b) | a == b = true
  eq _ _ = false

isMatch :: forall a. LayerResult a -> Boolean
isMatch Match = true
isMatch _ = false

testMultExpect :: forall a. Array (E a) -> a -> String -> { result :: TestResult, newArray :: Array (E a) }
testMultExpect lf a tag = case uncons lf of
  Just { head: {f: f, m: str, t: tag'}, tail: tf } ->
    if tag == tag'
      then if f a
             then { result: Success str, newArray: tf }
             else let { result: r, newArray: n } = testMultExpect tf a tag in
                    { result: r, newArray: cons (mkE f str tag') n }
      else let { result: r, newArray: n } = testMultExpect tf a tag in
                    { result: r, newArray: cons (mkE f str tag') n }
  Nothing -> { result: Fail, newArray: [] }


expectLayerM :: forall a. a -> String -> Ref (Expect a) -> Ref (M.Map String a) -> Eff _ (LayerResult a)
expectLayerM a tag ref mapRef = do
  e <- readRef ref
  m <- readRef mapRef
  case e of
    (Expect lf e') -> do
      let { result: r, newArray: n } = testMultExpect lf a tag
      case r of
         Success str -> do
           writeRef mapRef (m # M.insert str a)
           if (n # length) == 0
             then do
               writeRef ref e'
               verify <- doVerify ref mapRef
               if verify
                  then pure Match
                  else pure (VerificationFail a)
             else do
               writeRef ref (Expect n e')
               pure Match
         Fail -> do
           writeRef ref Done
           pure (Wrong a)
    Verify _ _ _ -> unsafeThrow "unexpected verify"
    Done -> do
      pure (TooMuch a)

doVerify ref mapRef = do
  e <- readRef ref
  m <- readRef mapRef
  case e of
    Verify ls f e' -> do
      let result = transformF f m ls
      writeRef ref e'
      nested <- doVerify ref mapRef
      pure $ result && nested
    _ -> do
      pure true

transformF :: forall a. (Array a -> Boolean) -> M.Map String a -> Array String -> Boolean
transformF f m ls =
  -- TODO: use validation to gather all unknown variables
  let mas = ls <#> (\x -> case (m # M.lookup x) of
                     Just a -> a
                     Nothing -> unsafeThrow ("unknown variable " <> x)
            )
  in f mas

test = do
  e <- newRef expect12
  m <- newRef (M.empty)
  expectLayerM 1 "a" e m >>= logShow
  expectLayerM 3 "a" e m >>= logShow
  expectLayerM 2 "a" e m >>= logShow

test2 = do
  e <- newRef expect3
  m <- newRef (M.empty)
  expectLayerM 1 "a" e m >>= logShow
  expectLayerM 1 "a" e m >>= logShow
 -- expectLayerM 2 e m >>= logShow
