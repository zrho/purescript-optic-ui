module OpticUI.Components.Async
  ( Async ()
  , AsyncHandle ()
  , request
  , async
  , _Waiting
  , _Request
  , _Success
  , _Failure
  ) where
--------------------------------------------------------------------------------
import Prelude
import Optic.Core
import Optic.Extended
import OpticUI.Core
import Data.Maybe
import Data.Const
import Data.Monoid (Monoid)
import Data.Either
import Data.Functor ((<$))
import Control.Monad.Eff
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Eff.Console (CONSOLE (), print)
import Control.Monad.Eff.Ref (REF (), Ref (), newRef, readRef, writeRef)
import Control.Monad.Eff.Exception(Error(), EXCEPTION(), catchException, throwException, error)
--------------------------------------------------------------------------------

-- | State for an asynchronous computation.
data Async eff a
  = Request (Aff eff a)
  | Waiting (AsyncHandle eff a)
  | Success a
  | Failure Error

newtype AsyncHandle eff a = AsyncHandle (Ref (Either Error a -> Eff eff Unit))

-- | Create a new request.
request :: forall eff a. Aff eff a -> Async eff a
request = Request

-- | Manage the asynchronous request that is in the focus of the UI.
async :: forall a b eff. UI (Async (ref :: REF | eff) a) (ref :: REF | eff) Unit
async = do
  s <- uiState
  h <- handler $ \es _ -> pure $ either Failure Success es
  case s of
    Request go -> execute $ const $ do
      r <- newRef h
      let onError e   = readRef r >>= \g -> g (Left e)
      let onSuccess x = readRef r >>= \g -> g (Right x)
      Waiting (AsyncHandle r) <$ runAff onError onSuccess go
    Waiting (AsyncHandle r) -> inline $ const (writeRef r h)
    _ -> pure unit

--------------------------------------------------------------------------------

_Waiting :: forall a eff. PrismP (Async eff a) (AsyncHandle eff a)
_Waiting = prism' Waiting $ \x -> case x of
  Waiting y -> Just y
  _         -> Nothing

_Request :: forall a eff. PrismP (Async eff a) (Aff eff a)
_Request = prism' Request $ \x -> case x of
  Request y -> Just y
  _         -> Nothing

_Success :: forall a eff. PrismP (Async eff a) a
_Success = prism' Success $ \x -> case x of
  Success y -> Just y
  _         -> Nothing

_Failure :: forall a eff. PrismP (Async eff a) Error
_Failure = prism' Failure $ \x -> case x of
  Failure y -> Just y
  _         -> Nothing
