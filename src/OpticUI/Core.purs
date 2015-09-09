module OpticUI.Core
  ( UI ()
  , Handler (..)
  , runHandler
  , runUI
  , ui
  , with
  , withView
  , traversal
  , foreach
  ) where
--------------------------------------------------------------------------------
import           Prelude
import           Optic.Core
import           Optic.Extended
import           Data.Profunctor            (Profunctor, dimap)
import           Data.Profunctor.Choice     (Choice, left, right)
import           Data.Profunctor.Strong     (Strong, first, second)
import           Control.Monad.State        (evalState)
import           Control.Monad.State.Class  (state)
import           Control.Monad.Eff          (Eff ())
import           Data.Monoid                (Monoid, mempty)
import           Data.Traversable           (Traversable, traverse, sequence)
import           Data.Tuple                 (Tuple (..))
import           Data.Either                (Either (..), either)
import           Data.Functor.Contravariant (Contravariant, cmap)
--------------------------------------------------------------------------------

newtype UI eff v s t = UI
  ( s -> Handler eff t -> Eff eff v )

instance uiProfunctor :: Profunctor (UI eff v) where
  dimap f g (UI u) = UI \s h -> u (f s) (cmap g h)

instance uiChoice :: (Monoid v) => Choice (UI eff v) where
  left (UI u) = UI \s h -> either (flip u $ cmap Left h) (\_ -> pure mempty) s
  right (UI u) = UI \s h -> either (\_ -> pure mempty) (flip u $ cmap Right h) s

instance uiStrong :: Strong (UI eff v) where
  first (UI u) = UI \(Tuple a b) h  -> u a $ cmap (\r -> Tuple r b) h
  second (UI u) = UI \(Tuple a b) h -> u b $ cmap (\r -> Tuple a r) h

instance uiSemigroup :: (Semigroup v) => Semigroup (UI eff v s t) where
  append (UI u) (UI v) = UI \s h -> append <$> u s h <*> v s h

instance uiMonoid :: (Monoid v) => Monoid (UI eff v s t) where
  mempty = UI \_ _ -> pure mempty

runUI :: forall eff v s t. UI eff v s t -> s -> Handler eff t -> Eff eff v
runUI (UI u) = u

--------------------------------------------------------------------------------

newtype Handler eff s = Handler (s -> Eff eff Unit)

instance handlerContravariant :: Contravariant (Handler eff) where
  cmap f (Handler h) = Handler (h <<< f)

runHandler :: forall eff s. Handler eff s -> s -> Eff eff Unit
runHandler (Handler h) = h

--------------------------------------------------------------------------------

ui :: forall eff v s t. v -> UI eff v s t
ui v = UI \_ _ -> pure v

with :: forall eff v s t. (s -> Handler eff t -> UI eff v s t) -> UI eff v s t
with f = UI \s h -> case f s h of UI u -> u s h

withView :: forall eff v w s t. (v -> w) -> UI eff v s t -> UI eff w s t
withView f (UI u) = UI \s h -> map f (u s h)

traversal :: forall eff v s t. (Monoid v) => TraversalP s t -> UI eff v t t -> UI eff v s s
traversal t = fromSink <<< t <<< toSink

foreach
  :: forall eff v s t. (Monoid v, Traversable t)
  => (Int -> UI eff v s s) -> UI eff v (t s) (t s)
foreach f = fromSink $ sequence <<< indices (toSink <<< f) where
  indices g t = evalState (traverse (\x -> state \i -> Tuple (g i x) (i + 1)) t) 0

--------------------------------------------------------------------------------

data Sink eff v a = Sink a (Handler eff a -> Eff eff v)

instance sinkFunctor :: Functor (Sink eff v) where
  map f (Sink x h) = Sink (f x) (h <<< cmap f)

instance sinkApply :: (Semigroup v) => Apply (Sink eff v) where
  apply (Sink f fh) (Sink x xh) = Sink (f x) \h -> append
    <$> fh (cmap ($ x) h) <*> xh (cmap (f $) h)

instance sinkApplicative :: (Monoid v) => Applicative (Sink eff v) where
  pure x = Sink x \_ -> pure mempty

runSink :: forall eff v a. Sink eff v a -> Handler eff a -> Eff eff v
runSink (Sink _ f) = f

fromSink :: forall eff v s. (s -> Sink eff v s) -> UI eff v s s
fromSink f = UI (runSink <<< f)

toSink :: forall eff v s. UI eff v s s -> s -> Sink eff v s
toSink (UI u) s = Sink s (u s)
