module OpticUI.HTML where
--------------------------------------------------------------------------------
import Prelude
import Data.Maybe  (Maybe (..))
import Data.Exists (Exists (..), mkExists, runExists)
import qualified OpticUI.Internal.VirtualDOM as VD
import Control.Monad.Eff (Eff())
import Data.Foldable (foldMap)
import Data.Nullable     (Nullable(), toNullable)
import Data.Function     (Fn2 (), runFn2)
import DOM.HTML.Types (HTMLElement ())
--------------------------------------------------------------------------------

type TagName   = String
type AttrName  = String
type PropName  = String
type EventName = String
type NS        = String

data HTML eff
  = Text String
  | Element (Maybe NS) TagName (Array (Prop eff)) (Array (HTML eff))

data HandlerE eff e = HandlerE EventName (e -> Eff eff Unit)
data PropE e = PropE String e

data Prop eff
  = Attr    AttrName String
  | Prop    (Exists PropE)
  | Handler (Exists (HandlerE eff))

attr :: forall eff. AttrName -> String -> Prop eff
attr n v = Attr n v

prop :: forall eff a. PropName -> a -> Prop eff
prop n v = Prop (mkExists (PropE n v))

handle :: forall e eff. EventName -> (e -> Eff eff Unit) -> Prop eff
handle n f = Handler (mkExists (HandlerE n f))

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

--------------------------------------------------------------------------------
