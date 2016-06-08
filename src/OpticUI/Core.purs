module OpticUI.Core
  ( UI (), UI_ ()
  , runUI, mkUI
  , mkView, overView, mkSub, overSub
  , withState, effect, ixui
  ) where
--------------------------------------------------------------------------------
import Prelude
import Control.Plus
import Control.Alt
import Control.Bind
import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice
import Data.Lens
import Data.Lens.Lens.Product as PL
import Data.Lens.Internal.Indexed
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Monoid
import Data.Functor.Compose
import OpticUI.Internal.Recombine
import OpticUI.Component

-- | User interface component over a view `f`: for any type `e`, `f e`
-- | should represent a renderable view that can raise events of type `e`.
-- | Views should be composable using a `Plus` instance.
-- |
-- | A user interface component has an associated input state type `s` which can
-- | be read in the construction of a component. In addition an `UI` component
-- | has an output state `t`, the new state of the component after an event has
-- | been raised. While `s` and `t` are allowed to differ (for technical reasons),
-- | most operations expect `s` and `t` to be the same. On top of a new state,
-- | events raised by the view of an `UI` component are allowed to have effects
-- | in a monad `m`.
-- |
-- | Further, a user interface manages subscriptions using the type argument `k`,
-- | where (as with the view), `k e` represents a subscription for events of type `e`.
-- |
-- | `UI m f k` is an instance of `Profunctor`, `Strong`, `Choice` and `Wander`.
-- | Hence `Iso`s, `Lens`es, `Prism`s and `Traversal`s can be used to focus
-- | parts on the state. Where there are multiple foci, the respective views
-- | are appended with `alt`; where there is none, the view will be `empty`.
newtype UI_ m f k s t = UI (forall a b. Traversal a b s t -> a -> Component k f (m b))

instance profunctorUI :: Profunctor (UI_ m f k) where
  dimap f g (UI h) = UI \tr -> h \t -> tr (dimap f g t)

instance strongUI :: Strong (UI_ m f k) where
  first = dimap swap swap <<< traversed
  second = traversed

instance choiceUI :: Choice (UI_ m f k) where
  left = dimap swapE swapE <<< traversed
  right = traversed

instance wanderUI :: Wander (UI_ m f k) where
  wander tr (UI h) = UI \lr a -> h (lr <<< wander tr) a

instance semigroupUI :: (Plus f, Plus k, Functor m) => Semigroup (UI_ m f k s s) where
  append a b = mkUI \s -> runUI a s <|> runUI b s

instance monoidUI :: (Plus f, Plus k, Functor m) => Monoid (UI_ m f k s s) where
  mempty = mkUI \_ -> empty

-- | Type alias for `UI_` with the same input and output type.
type UI m f k s = UI_ m f k s s

-- | Converts an `UI` component into a map from state to view.
runUI :: forall f m s t k. UI_ m f k s t -> s -> Component k f (m t)
runUI (UI u) = u id

-- | Converts a map from state to view into an `UI` component.
mkUI :: forall f m s k. (Plus f, Plus k, Functor m) => (s -> Component k f (m s)) -> UI m f k s
mkUI f = UI (\tr -> recombine f tr)

-- | Create a static `UI` component from a view.
mkView :: forall f m s k. (Plus f, Plus k, Functor m) => f (m s) -> UI m f k s
mkView = mkUI <<< const <<< Component empty

-- | Apply a function to the view of an `UI` component.
overView :: forall f g m s k. (Plus g, Plus k, Functor m) => (f (m s) -> g (m s)) -> UI m f k s -> UI m g k s
overView f = mkUI <<< map (\(Component s v) -> Component s (f v)) <<< runUI

-- | Create a `UI` component from a subscription.
mkSub :: forall f m s k. (Plus f, Plus k, Functor m) => k (m s) -> UI m f k s
mkSub = mkUI <<< const <<< flip Component empty

-- | Apply a function to the subscriptions of an `UI` component.
overSub :: forall f m s k l. (Plus f, Plus l, Functor m) => (k (m s) -> l (m s)) -> UI m f k s -> UI m f l s
overSub f = mkUI <<< map (\(Component s v) -> Component (f s) v) <<< runUI

-- | Access the state of an `UI` component.
withState :: forall f m s k. (Plus f, Plus k, Functor m) => (s -> UI m f k s) -> UI m f k s
withState = mkUI <<< (runUI =<<)

-- | Interpret the effects of an `UI` component in another monad.
effect :: forall s f m w k. (Plus f, Plus k, Functor w) => (m s -> w s) -> UI m f k s -> UI w f k s
effect f = mkUI <<< (map <<< map) f <<< runUI

-- | Wrap an `UI` component such that it can be used with indexed optics.
ixui :: forall i m f k a. (Plus f, Plus k, Functor m) => (i -> UI m f k a) -> Indexed (UI_ m f k) i a a
ixui u = Indexed $ rmap snd (mkUI \(Tuple i a) -> (map <<< map) (Tuple i) $ runUI (u i) a)

-- | Helper function that lifts `a -> f (m a)` to the cofree wander used in `UI`.
recombine :: forall f m a s t. (Plus f, Functor m) => (a -> f (m a)) -> Traversal s t a a -> s -> f (m t)
recombine f tr s = decompose (fromRecombine (traverseOf tr (toRecombine (Compose <<< f))) s)

-- | Swaps the components of an `Either`.
swapE :: forall a b. Either a b -> Either b a
swapE = either Right Left
