module OpticUI.Internal.Bazaar where
--------------------------------------------------------------------------------
import           Prelude
import           Data.List (List (..))
import qualified Data.List as L
import           Data.Tuple
import           Data.Const                (Const (..), getConst)
import           Control.Monad.State       (evalState)
import           Control.Monad.State.Class (state)
import           Unsafe.Coerce             (unsafeCoerce)
--------------------------------------------------------------------------------

newtype Bazaar a b t = Bazaar (forall f. (Applicative f) => (a -> f b) -> f t)

instance bazaarFunctor :: Functor (Bazaar a b) where
  map f (Bazaar k) = Bazaar ((f <$>) <<< k)

instance bazaarApply :: Apply (Bazaar a b) where
  apply (Bazaar mf) (Bazaar ma) = Bazaar (\afb -> mf afb <*> ma afb)

instance bazaarApplicative :: Applicative (Bazaar a b) where
  pure a = Bazaar (\_ -> pure a)

sell :: forall a b. a -> Bazaar a b b
sell a = Bazaar (\k -> k a)

pins :: forall a b t. Bazaar a b t -> L.List a
pins (Bazaar k) = getConst (k (\ra -> Const (L.singleton ra)))

unsafeOuts :: forall a b t. Bazaar a b t -> L.List b -> t
unsafeOuts (Bazaar k) = evalState (k (\_ -> state unconsUnsafe)) where
  unconsUnsafe L.Nil         = Tuple (unsafeCoerce unit) L.Nil
  unconsUnsafe (L.Cons x xs) = Tuple x xs
