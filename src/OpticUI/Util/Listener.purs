module OpticUI.Util.Listener where
--------------------------------------------------------------------------------
import Prelude
import OpticUI.Run
import Control.Alt
import Control.Plus
import Data.Maybe
import Data.Monoid
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Profunctor

newtype Listener k a = Listener (k -> Maybe a)

instance listenerFunctor :: Functor (Listener k) where
  map f (Listener h) = Listener \s -> map f (h s)

instance listenerAlt :: Alt (Listener k) where
  alt (Listener a) (Listener b) = Listener \s -> alt (a s) (b s)

instance listenerPlus :: Plus (Listener k) where
  empty = Listener \_ -> empty

instance listenerSemigroup :: Semigroup (Listener k a) where
  append = alt

instance listenerMonoid :: Monoid (Listener k a) where
  mempty = empty

instance listenerProfunctor :: Profunctor Listener where
  dimap f g (Listener h) = Listener (map g <<< h <<< f)

runListener :: forall a k. Listener k a -> k -> Maybe a
runListener (Listener h) = h

execListener :: forall a k f. (Applicative f) => Listener k (f a) -> k -> f Unit
execListener (Listener h) k = maybe (pure unit) void (h k)

listen :: forall a k. (k -> Boolean) -> (k -> a) -> Listener k a
listen q f = Listener \k -> if q k then Just (f k) else Nothing

listenerBackend
  :: forall a k eff. ((k -> Eff (ref :: REF | eff) Unit) -> Eff (ref :: REF | eff) Unit)
  -> Eff (ref :: REF | eff) (Backend (ref :: REF | eff) (Listener k))
listenerBackend run = do
  driveRef <- newRef empty
  run \k -> do
    drive <- readRef driveRef
    execListener drive k
  pure \drive -> writeRef driveRef drive
