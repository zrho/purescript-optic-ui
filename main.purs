module OpticUI where
import Optic.Core
import Optic.Extended
import Control.Monad.Eff
import Control.Monad.Reader.Trans (ReaderT, withReaderT)
import Data.Tuple (Tuple)
import Data.Traversable
import VirtualDOM
import VirtualDOM.VTree
import Data.Foreign.Undefined
import DOM
--------------------------------------------------------------------------------

newtype UI s m a = UI { fromUI :: ReaderT (Tuple s (m ())) m a }

instance uiFunctor :: (Monad m) => Functor (UI s eff m) where
  (<$>) f m = m >>= \x -> pure (f x)

instance uiApplicative :: (Monad m) => Applicative (UI s eff m) where
  pure = UI <<< pure x

instance uiApply :: (Monad m) => Apply (UI s eff m) where
  (<*>) ff fx = do
    f <- ff
    x <- fx
    return (f x)

instance uiBind :: (Monad m) => Bind (UI s eff m) where
  (>>=) m f = UI (fromUI m >>= fromUI <<< f x)

instance uiMonad :: (Monad m) => Monad (UI s eff m)

--------------------------------------------------------------------------------

embed :: forall m s t a. (Monad m) => LensP s t -> UI t m a -> UI s m a
embed l m = UI $ withReaderT
  (\Tuple s fs -> Tuple (s ^. l) (fs <<< flip (set l) s))
  (fromUI m)
