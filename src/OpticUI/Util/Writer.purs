module OpticUI.Util.Writer
  ( zoomW
  , zoomIxedW
  , collect
  , module Control.Monad.Writer.Trans
  , module Control.Monad.Writer.Class
  , module Control.Monad.Trans
  ) where
--------------------------------------------------------------------------------
import Prelude
import Control.Monad.Writer.Trans (WriterT (..), execWriterT)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Trans        (lift)
import Data.Monoid                (Monoid)
import Optic.Extended
import OpticUI.Core
--------------------------------------------------------------------------------

-- | Zoom in on a dynamic number of components using a traversal and write
-- | the result to a writer transformer around the UI monad. Together with
-- | `execWriterT` this allows to compose markup from multiple components
-- | using do notation.
zoomW
  :: forall eff a s t. (Monoid a)
  => TraversalP s t -> UI t eff a -> WriterT a (UI s eff) Unit
zoomW tr ui = lift (zoom tr ui) >>= tell

-- | Zoom in on a dynamic number of components using a traversal and write
-- | the result to a writer transformer around the UI monad, keeping track of
-- | the index of the zoomed component.
zoomIxedW
  :: forall eff a s t. (Monoid a)
  => TraversalP s t -> (Int -> UI t eff a) -> WriterT a (UI s eff) Unit
zoomIxedW tr ui = lift (zoomIxed tr ui) >>= tell

collect :: forall a b c m. (Monoid a, Monad m) => (a -> b) -> WriterT a m c -> m b
collect f w = f <$> execWriterT w
