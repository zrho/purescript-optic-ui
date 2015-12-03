module OpticUI.Markup where
--------------------------------------------------------------------------------
import Prelude
import Data.Maybe  (Maybe (..))
import Data.Exists (Exists (), mkExists, runExists)
import qualified OpticUI.Internal.VirtualDOM as VD
import Control.Monad.Eff (Eff())
import Data.Monoid (Monoid, mempty)
import Data.Foldable (foldMap)
import Data.Nullable     (Nullable(), toNullable)
import Data.Function     (Fn2 (), runFn2)
import DOM.HTML.Types (HTMLElement ())
import Control.Monad.Writer.Trans (WriterT (..), execWriterT)
import Unsafe.Coerce   (unsafeCoerce)
--------------------------------------------------------------------------------

type TagName   = String
type AttrName  = String
type PropName  = String
type EventName = String
type NS        = String
type UniqueStr = String

data Markup = Markup (Array Node)

data Node
  = Text String
  | Element (Maybe NS) TagName (Array Prop) Markup

element :: forall eff. Maybe NS -> TagName -> Array Prop -> Markup -> Markup
element ns tag props childs = Markup [ Element ns tag props childs ]

text :: forall eff. String -> Markup
text t = Markup [ Text t ]

instance markupSemigroup :: Semigroup Markup where
  append (Markup xs) (Markup ys) = Markup (xs ++ ys)

instance markupMonoid :: Monoid Markup where
  mempty = Markup []

--------------------------------------------------------------------------------

data Prop
  = AttrP        AttrName String
  | PropP        PropName (Exists PropE)
  | HandlerP     EventName EventHandler
  | InitializerP UniqueStr Initializer
  | FinalizerP   UniqueStr Finalizer

data PropE e = PropE e

attr :: forall eff. AttrName -> String -> Prop
attr n v = AttrP n v

prop :: forall eff a. PropName -> a -> Prop
prop n v = PropP n (mkExists (PropE v))

handle :: forall e eff. EventName -> (e -> Eff eff Unit) -> Prop
handle n f = HandlerP n (mkEventHandler f)

--------------------------------------------------------------------------------

-- | An event handler to be attached to a DOM node. The type of event and the
-- | row of effects are hidden in an existential package.
data EventHandler

-- | Build a handler from a handler function.
mkEventHandler :: forall e eff. (e -> Eff eff Unit) -> EventHandler
mkEventHandler = unsafeCoerce

-- | Use the handler function for a handler.
runEventHandler :: forall r. (forall e eff. (e -> Eff eff Unit) -> r) -> EventHandler -> r
runEventHandler f h = f (unsafeCoerce h)

--------------------------------------------------------------------------------

data Initializer
data Finalizer

mkInitializer :: forall eff. (HTMLElement -> Eff eff Unit) -> Initializer
mkInitializer = unsafeCoerce

runInitializer :: forall r. ((forall eff. HTMLElement -> Eff eff Unit) -> r) -> Initializer -> r
runInitializer f init = f (unsafeCoerce init)

initializer s f = InitializerP s (mkInitializer f)

mkFinalizer   :: forall eff. (HTMLElement -> Eff eff Unit) -> Finalizer
mkFinalizer   = unsafeCoerce

runFinalizer :: forall r. ((forall eff. HTMLElement -> Eff eff Unit) -> r) -> Finalizer -> r
runFinalizer f i = f (unsafeCoerce i)

finalizer s f = FinalizerP s (mkFinalizer f)

--------------------------------------------------------------------------------

type Event r =
  { bubbles :: Boolean
  , cancelable :: Boolean
  , currentTarget :: HTMLElement
  , target :: HTMLElement
  , timeStamp :: Number
  , "type" :: String
  | r
  }

type KeyboardEvent r = Event ( keyCode :: Int | r )
--------------------------------------------------------------------------------
