module OpticUI.Core
  ( UI ()
  , UI_ ()
  , Handler (..)
  , Result (..)
  , update
  , updatePure
  , raise
  , withEffects
  , runUI
  , ui
  , with
  , withView
  , withSignal
  , listen
  , traversal
  , foreach
  ) where
--------------------------------------------------------------------------------
import Prelude (class Applicative, class Functor, class Apply, class Semigroup,
                Unit, (<<<), ($), (<>), (+), pure, map, (>>=), append, flip)
import Control.Apply              ((*>))
import Control.Monad.State        (state, evalState)
import Control.Monad.Eff          (Eff ())
import Data.Monoid                (class Monoid, mempty)
import Data.Maybe                 (maybe)
import Data.Traversable           (class Traversable, traverse, sequence)
import Data.Tuple                 (Tuple (..))
import Data.Either                (Either (..), either)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Profunctor            (class Profunctor)
import Data.Profunctor.Choice     (class Choice)
import Data.Profunctor.Strong     (class Strong)
import Data.Profunctor.Star       (Star(Star), runStar)
import Data.Lens                  (Traversal, APrismP, clonePrism, preview, review)
--------------------------------------------------------------------------------

-- | The type of user interface components in `OpticUI`.
-- |
-- | A user interface component has an associated state of type `s`. Using `with`
-- | it can read this state and get a handler function to write the state on
-- | an event. The type of the written state is `t` to allow `UI` to be a
-- | `Profunctor`; for almost all practical purposes, `s` and `t` coincide.
-- |
-- | With the state and the handler functions, components generate a view of
-- | type `v`. If `v` is a monoid, multiple components can be appended in
-- | sequence for a composite view.
-- |
-- | The handler can execute effects in a monad `m`. Using `withEffects` the
-- | effects in an inner component can be interpreted in the monad of the
-- | outer component.
-- |
-- | Using `listen`, an `UI` component can subscribe to signals of type `k`.
-- | Signals are raised from the outside of the user interface and are
-- | propagated to all listeners until one listener accepts the signal.
-- | With `raise` signals can be issued from inside the application.
newtype UI eff m k v s t = UI (s -> Handler eff m k t -> Result eff k v)

-- | Shorthand for user interfaces with the same input and output state.
type UI_ eff m k v s = UI eff m k v s s

instance uiProfunctor :: (Functor m) => Profunctor (UI eff m k v) where
  dimap f g (UI u) = UI \s h -> u (f s) (cmap g h)

instance uiChoice :: (Monoid v, Functor m) => Choice (UI eff m k v) where
  left (UI u) = UI \s h -> either (flip u $ cmap Left h) (\_ -> mempty) s
  right (UI u) = UI \s h -> either (\_ -> mempty) (flip u $ cmap Right h) s

instance uiStrong :: (Functor m) => Strong (UI eff m k v) where
  first (UI u) = UI \(Tuple a b) h  -> u a $ cmap (\r -> Tuple r b) h
  second (UI u) = UI \(Tuple a b) h -> u b $ cmap (\r -> Tuple a r) h

instance uiSemigroup :: (Semigroup v) => Semigroup (UI eff m k v s t) where
  append (UI u) (UI v) = UI \s h -> append (u s h) (v s h)

instance uiMonoid :: (Monoid v) => Monoid (UI eff m k v s t) where
  mempty = UI \_ _ -> mempty

runUI :: forall eff m k v s t. UI eff m k v s t -> s -> Handler eff m k t -> Result eff k v
runUI (UI u) = u

--------------------------------------------------------------------------------

data Result eff k v = Result (k -> Eff eff Boolean) v

instance resultSemigroup :: (Semigroup v) => Semigroup (Result eff k v) where
  append (Result shL vL) (Result shR vR) = Result sh (vL <> vR) where
    sh k = shL k >>= \r -> if r then pure true else shR k

instance resultMonoid :: (Monoid v) => Monoid (Result eff k v) where
  mempty = Result (\_ -> pure false) mempty

--------------------------------------------------------------------------------

data Handler eff m k s = Handler (m s -> Eff eff Unit) (k -> Eff eff Unit)

instance handlerContravariant :: (Functor m) => Contravariant (Handler eff m k) where
  cmap f (Handler h hs) = Handler (h <<< map f) hs

update :: forall eff m k s. Handler eff m k s -> m s -> Eff eff Unit
update (Handler h _) = h

updatePure :: forall eff m k s. (Applicative m) => Handler eff m k s -> s -> Eff eff Unit
updatePure (Handler h _) = h <<< pure

raise :: forall eff m k s. Handler eff m k s -> k -> Eff eff Unit
raise (Handler _ hs) = hs

--------------------------------------------------------------------------------

-- | Create a static `UI` component from a view.
ui :: forall eff m k v s t. v -> UI eff m k v s t
ui v = UI \_ _ -> Result (\_ -> pure false) v

-- | Access the state and the handler for an `UI` component.
with :: forall eff m k v s t. (s -> Handler eff m k t -> UI eff m k v s t) -> UI eff m k v s t
with f = UI \s h -> case f s h of UI u -> u s h

-- | Manipulate the view of an `UI` component.
withView :: forall eff m k v w s t. (v -> w) -> UI eff m k v s t -> UI eff m k w s t
withView f (UI u) = UI \s h -> case u s h of
  Result sh v -> Result sh (f v)

-- | Interpret the effects of an `UI` component in a different monad.
withEffects :: forall eff m w k v s t l. (m t -> w l) -> UI eff m k v s t -> UI eff w k v s l
withEffects f (UI u) = UI \s (Handler h hs) -> u s $ Handler (h <<< f) hs

-- | Translate and route signals.
withSignal :: forall eff m k l v s t. APrismP l k -> UI eff m k v s t -> UI eff m l v s t
withSignal p (UI u) = UI \s (Handler h hs) -> case u s (Handler h $ hs <<< review (clonePrism p)) of
  Result sh v -> Result (maybe (pure false) sh <<< preview (clonePrism p)) v

-- | Listen to a signal matching a predicate and execute an action on match.
listen :: forall eff m k v s t. (Monoid v) => (k -> Boolean) -> (k -> Eff eff Unit) -> UI eff m k v s t
listen p go = UI \_ _ -> Result (\k -> if p k then go k *> pure true else pure false) mempty

-- | Display a `UI` component for each focus of a `Traversal`.
traversal
  :: forall eff m k v s t. (Monoid v, Functor m)
  => Traversal s s t t -> UI eff m k v t t -> UI eff m k v s s
traversal t = fromSink <<< runStar <<< t <<< Star <<< toSink

-- | Display a `UI` component for each element of a `Traversable` container,
-- | with access to the index into the container.
foreach
  :: forall eff m k v s t. (Monoid v, Traversable t, Functor m)
  => (Int -> UI eff m k v s s) -> UI eff m k v (t s) (t s)
foreach f = fromSink $ sequence <<< indices (toSink <<< f) where
  indices g t = evalState (traverse (\x -> state \i -> Tuple (g i x) (i + 1)) t) 0

--------------------------------------------------------------------------

data Sink eff m k v a = Sink a (Handler eff m k a -> Result eff k v)

instance sinkFunctor :: (Functor m) => Functor (Sink eff m k v) where
  map f (Sink x h) = Sink (f x) (h <<< cmap f)

instance sinkApply :: (Semigroup v, Functor m) => Apply (Sink eff m k v) where
  apply (Sink f fh) (Sink x xh) = Sink (f x) \h ->
    fh (cmap ($ x) h) <> xh (cmap (f $) h)

instance sinkApplicative :: (Monoid v, Functor m) => Applicative (Sink eff m k v) where
  pure x = Sink x \_ -> mempty

runSink :: forall eff m k v a. Sink eff m k v a -> Handler eff m k a -> Result eff k v
runSink (Sink _ f) = f

fromSink :: forall eff m k v s. (s -> Sink eff m k v s) -> UI eff m k v s s
fromSink f = UI (runSink <<< f)

toSink :: forall eff m k v s. UI eff m k v s s -> s -> Sink eff m k v s
toSink (UI u) s = Sink s (u s)
