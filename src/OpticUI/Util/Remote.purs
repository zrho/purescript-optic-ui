module OpticUI.Util.Remote where
--------------------------------------------------------------------------------
import Prelude
import Control.Apply
import Data.Lens
import Data.Monoid
import Data.Maybe
import Data.Either
import Data.Traversable
import Data.Foldable
import Data.Bifunctor

-- | A value retrieved from a remote service.
data Remote e a = Success a | Failed e | Pending | Init

instance remoteFunctor :: Functor (Remote e) where
  map = over _Success

instance remoteApply :: Apply (Remote e) where
  apply fr xr = fr >>= \f -> xr >>= \x -> pure (f x)

instance remoteApplicative :: Applicative (Remote e) where
  pure = Success

instance remoteBind :: Bind (Remote e) where
  bind (Success a) f = f a
  bind (Failed e) _ = Failed e
  bind Pending _ = Pending
  bind Init _ = Init

instance remoteMonad :: Monad (Remote e) where

instance remoteBifunctor :: Bifunctor Remote where
  bimap _ f (Success a) = Success (f a)
  bimap f _ (Failed e) = Failed (f e)
  bimap _ _ Pending = Pending
  bimap _ _ Init = Init

instance remoteSemigroup :: (Semigroup a) => Semigroup (Remote e a) where
  append = lift2 append

instance remoteMonoid :: (Monoid a) => Monoid (Remote e a) where
  mempty = pure mempty

instance remoteFoldable :: Foldable (Remote e) where
  foldr = foldrOf _Success
  foldl = foldlOf _Success
  foldMap = foldMapOf _Success

instance remoteTraversable :: Traversable (Remote e) where
  traverse = traverseOf _Success
  sequence = sequenceOf _Success

toRemote :: forall e a. Either e a -> Remote e a
toRemote = either Failed Success

_Success :: forall e a b. Prism (Remote e a) (Remote e b) a b
_Success = prism Success \r -> case r of
  Success a -> Right a
  Failed e  -> Left (Failed e)
  Pending   -> Left Pending
  Init      -> Left Init

_Failed :: forall e t a. Prism (Remote e a) (Remote t a) e t
_Failed = prism Failed \r -> case r of
  Failed e  -> Right e
  Success a -> Left (Success a)
  Pending   -> Left Pending
  Init      -> Left Init

_Pending :: forall e a. PrismP (Remote e a) Unit
_Pending = prism' (const Pending) \r -> case r of
  Pending -> Just unit
  _      -> Nothing

_Init :: forall e a. PrismP (Remote e a) Unit
_Init = prism' (const Init) \r -> case r of
  Init -> Just unit
  _    -> Nothing
