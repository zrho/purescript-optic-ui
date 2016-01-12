module OpticUI.Components.Async where
--------------------------------------------------------------------------------
import Prelude
import OpticUI.Core
import Control.Monad.Eff
import Control.Monad.Eff.Ref (REF (), Ref (), newRef, readRef, writeRef)
import Control.Monad.Aff
import Control.Monad.Eff.Exception (Error ())
import Data.Either
import Data.Monoid
--------------------------------------------------------------------------------

data Async eff a = Async (Ref (Either Error a -> Eff eff Unit))

onResult
  :: forall eff m v t a. (Monoid v, Functor m)
  => (a -> Eff (ref :: REF | eff) Unit)
  -> (Error -> Eff (ref :: REF | eff) Unit)
  -> UI (ref :: REF | eff) m v (Async (ref :: REF | eff) a) t
onResult s f = with \(Async r) _ -> unsafeInline do
  writeRef r (either f s)
  pure mempty

async
  :: forall eff a. Aff (ref :: REF | eff) a
  -> Eff (ref :: REF | eff) (Async (ref :: REF | eff) a)
async go = do
  r <- newRef (const $ pure unit)
  let s a = readRef r >>= ($ (Right a))
  let f e = readRef r >>= ($ (Left e))
  runAff f s go
  pure (Async r)
