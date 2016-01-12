module OpticUI.Core
  ( UI ()
  , UI_ ()
  , Handler (..)
  , update
  , updatePure
  , withEffects
  , runUI
  , ui
  , with
  , withView
  , traversal
  , foreach
  , unsafeInline
  ) where
--------------------------------------------------------------------------------
import           Prelude
import           Data.Profunctor            (Profunctor)
import           Data.Profunctor.Choice     (Choice)
import           Data.Profunctor.Strong     (Strong)
import           Control.Monad.State
import           Control.Monad.Eff          (Eff ())
import           Data.Monoid                (Monoid, mempty)
import           Data.Lens
import           Data.Profunctor.Star
import           Data.Traversable           (Traversable, traverse, sequence)
import           Data.Tuple                 (Tuple (..))
import           Data.Either                (Either (..), either)
import           Data.Functor.Contravariant (Contravariant, cmap)
--------------------------------------------------------------------------------

newtype UI eff m v s t = UI
  ( s -> Handler eff m t -> Eff eff v )

type UI_ eff m v s = UI eff m v s s

instance uiProfunctor :: (Functor m) => Profunctor (UI eff m v) where
  dimap f g (UI u) = UI \s h -> u (f s) (cmap g h)

instance uiChoice :: (Monoid v, Functor m) => Choice (UI eff m v) where
  left (UI u) = UI \s h -> either (flip u $ cmap Left h) (\_ -> pure mempty) s
  right (UI u) = UI \s h -> either (\_ -> pure mempty) (flip u $ cmap Right h) s

instance uiStrong :: (Functor m) => Strong (UI eff m v) where
  first (UI u) = UI \(Tuple a b) h  -> u a $ cmap (\r -> Tuple r b) h
  second (UI u) = UI \(Tuple a b) h -> u b $ cmap (\r -> Tuple a r) h

instance uiSemigroup :: (Semigroup v) => Semigroup (UI eff m v s t) where
  append (UI u) (UI v) = UI \s h -> append <$> u s h <*> v s h

instance uiMonoid :: (Monoid v) => Monoid (UI eff m v s t) where
  mempty = UI \_ _ -> pure mempty

runUI :: forall eff v s t. UI eff (Eff eff) v s t -> s -> Handler eff (Eff eff) t -> Eff eff v
runUI (UI u) = u

--------------------------------------------------------------------------------

newtype Handler eff m s = Handler (m s -> Eff eff Unit)

instance handlerContravariant :: (Functor m) => Contravariant (Handler eff m) where
  cmap f (Handler h) = Handler (h <<< map f)

update :: forall eff m s. Handler eff m s -> m s -> Eff eff Unit
update (Handler h) = h

updatePure :: forall eff m s. (Applicative m) => Handler eff m s -> s -> Eff eff Unit
updatePure (Handler h) = h <<< pure

--------------------------------------------------------------------------------

-- | Create a static `UI` component from a view.
ui :: forall eff m v s t. v -> UI eff m v s t
ui v = UI \_ _ -> pure v

-- | Access the state and the handler for an `UI` component.
with :: forall eff m v s t. (s -> Handler eff m t -> UI eff m v s t) -> UI eff m v s t
with f = UI \s h -> case f s h of UI u -> u s h

-- | Manipulate the view of an `UI` component.
withView :: forall eff m v w s t. (v -> w) -> UI eff m v s t -> UI eff m w s t
withView f (UI u) = UI \s h -> map f (u s h)

-- | Interpret the effects of an `UI` component in a different monad.
withEffects :: forall eff m w v s t k. (m t -> w k) -> UI eff m v s t -> UI eff w v s k
withEffects f (UI u) = UI \s h -> u s (Handler $ update h <<< f)

-- | Display a `UI` component for each focus of a `Traversal`.
traversal
  :: forall eff m v s t. (Monoid v, Functor m)
  => Traversal s s t t -> UI eff m v t t -> UI eff m v s s
traversal t = fromSink <<< runStar <<< t <<< Star <<< toSink

-- | Display a `UI` component for each element of a `Traversable` container,
-- | with access to the index into the container.
foreach
  :: forall eff m v s t. (Monoid v, Traversable t, Functor m)
  => (Int -> UI eff m v s s) -> UI eff m v (t s) (t s)
foreach f = fromSink $ sequence <<< indices (toSink <<< f) where
  indices g t = evalState (traverse (\x -> state \i -> Tuple (g i x) (i + 1)) t) 0

-- | Create a `UI` component that executes an action while build.
unsafeInline :: forall eff m v s t. Eff eff v -> UI eff m v s t
unsafeInline go = UI \_ _ -> go

--------------------------------------------------------------------------------

data Sink eff m v a = Sink a (Handler eff m a -> Eff eff v)

instance sinkFunctor :: (Functor m) => Functor (Sink eff m v) where
  map f (Sink x h) = Sink (f x) (h <<< cmap f)

instance sinkApply :: (Semigroup v, Functor m) => Apply (Sink eff m v) where
  apply (Sink f fh) (Sink x xh) = Sink (f x) \h -> append
    <$> fh (cmap ($ x) h) <*> xh (cmap (f $) h)

instance sinkApplicative :: (Monoid v, Functor m) => Applicative (Sink eff m v) where
  pure x = Sink x \_ -> pure mempty

runSink :: forall eff m v a. Sink eff m v a -> Handler eff m a -> Eff eff v
runSink (Sink _ f) = f

fromSink :: forall eff m v s. (s -> Sink eff m v s) -> UI eff m v s s
fromSink f = UI (runSink <<< f)

toSink :: forall eff m v s. UI eff m v s s -> s -> Sink eff m v s
toSink (UI u) s = Sink s (u s)
