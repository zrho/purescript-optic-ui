module Main where
--------------------------------------------------------------------------------
import Prelude
import Control.Monad.State.Trans
import Control.Monad.Free.Trans
import Control.Monad.Rec.Class
import Control.Apply
import Control.Bind
import Data.Tuple
import Data.Either
import Data.Lens
import Data.Foldable
import Data.Profunctor
import OpticUI
import OpticUI.Components
import OpticUI.Components.State
import OpticUI.Markup.HTML as H
--------------------------------------------------------------------------------

main = animate (Tuple "" "") $ withState $ withEffects storeState component

--------------------------------------------------------------------------------

-- | Component with a single text field that can get and put the state in
-- | an ambient store. It is independent of the type of store used: the get
-- | and put actions of the free monad can be interpreted in arbitrary ways. E.g.:
-- |
-- | - Set another part of the UI state.
-- | - Run in a state monad.
-- | - Issue AJAX requests to a REST server.
-- | - etc.
component :: forall m eff. (Monad m) => UI_ eff (Store String m) Markup String
component = with \s h -> mconcat
  [ ui $ H.p_ $ text "Press 'Put' or 'Get' to manipulate the hidden store."
  , textField []
  , ui $ H.p_ $ mconcat
    [ H.button [ H.onClick \_ -> update h (putStore s *> pure s) ] (text "Put")
    , H.button [ H.onClick \_ -> update h getStore] (text "Get")
    ]
  ]

--------------------------------------------------------------------------------

-- | Free monad transformer with get and put operations.
type Store s m = FreeT (StoreF s) m

data StoreF s a
  = Get (s -> a)
  | Put s a

instance storeFunctor :: Functor (StoreF s) where
  map f (Get r)   = Get (map f r)
  map f (Put s a) = Put s (f a)

getStore :: forall m s. (Monad m) => Store s m s
getStore = liftFreeT (Get id)

putStore :: forall m s. (Monad m) => s -> Store s m Unit
putStore s = liftFreeT (Put s unit)

-- | Interpret the Store monad transformer in `StateT`.
storeState :: forall m s a. (MonadRec m) => Store s m a -> StateT s m a
storeState = evalFreeT lift \r -> case r of
  Get t   -> map t get
  Put s a -> put s *> pure a

--------------------------------------------------------------------------------

-- | Run a `FreeT` computation to completion, given functions that interpret
-- | the transformed monad and the functor in the target monad.
evalFreeT
  :: forall m w f a. (Functor f, MonadRec w, MonadRec m)
  => (forall b. m b -> w b)
  -> (forall b. f b -> w b)
  -> FreeT f m a -> w a
evalFreeT mw fw = tailRecM $ resume >>> mw >=> either (pure <<< Right) (map Left <<< fw)
