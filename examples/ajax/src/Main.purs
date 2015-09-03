module Main where
--------------------------------------------------------------------------------
import           Prelude
import           Optic.Core
import           Optic.Extended
import           OpticUI
import           OpticUI.Util.Writer
import           OpticUI.Components
import           OpticUI.Components.Async
import qualified OpticUI.Markup.HTML        as H
import qualified Data.List                  as L
import           Data.Monoid                (mempty)
import           Data.Either                (Either (..))
import           Data.Maybe                 (Maybe (..), maybe)
import           Data.Foldable              (Foldable, mconcat)
import           Data.Traversable           (traverse)
import qualified Data.JSON                  as JS
import qualified Data.Map                   as M
import qualified Network.HTTP.Affjax        as AJ
import           Control.Bind
import           Control.Monad              (when)
import           Control.Monad.Writer.Trans (execWriterT)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Writer.Class (censor)
--------------------------------------------------------------------------------

main = animate { name: "", repos: Nothing } $ do
  h <- handler $ \_ s ->
    let url = "https://api.github.com/users/" ++ s.name ++ "/repos"
    in pure $ s # repos .~ Just (request (JS.decode <<< _.response <$> AJ.get url))
  execWriterT $ do
    tell $
      H.h1_ (text "GitHub Repository List") ++
      H.p_ (text "Enter the name of a GitHub user:")
    zoomW name $ textField [ H.placeholderA "Enter a user name"]
    tell $ H.button [ H.onClick h ] $ text "Load"
    zoomW (repos <<< _Just) $ execWriterT $ do
      lift async
      zoomW _Waiting $ pure $ H.p_ $ text "Fetching repositories..."
      zoomW _Failure $ text <<< show <$> uiState
      zoomW (_Success <<< _Nothing) $ pure $ H.p_ $ text "Parse error :("
      zoomW (_Success <<< _Just) repoList

repoList = uiState >>= \s -> execWriterT $ do
  tell $ H.h2_ (text "Repositories")
  when (L.null (s ^.. _JArray .. traverse)) $ tell $ text "There are no repositories."
  censor H.ul_
    $ zoomW (_JArray <<< traverse <<< _JObject <<< ixMap "name" <<< _JString)
    $ H.li_ <<< text <$> uiState

--------------------------------------------------------------------------------
-- A huge list of lenses and prisms. Having to define this in user code is
-- obviously annoying; it might be worthwhile to revive refractor some time.

_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

_Nothing :: forall a. Prism (Maybe a) (Maybe a) Unit Unit
_Nothing = prism (const Nothing) $ maybe (Right unit) (Left <<< Just)

_JArray = prism' JS.JArray $ \x -> case x of
  JS.JArray y -> Just y
  _ -> Nothing

_JObject = prism' JS.JObject $ \x -> case x of
  JS.JObject y -> Just y
  _ -> Nothing

_JString = prism' JS.JString $ \x -> case x of
  JS.JString y -> Just y
  _ -> Nothing

_JNumber = prism' JS.JNumber $ \x -> case x of
  JS.JNumber y -> Just y
  _ -> Nothing

name = lens _.name (\r -> r { name = _ })
repos = lens _.repos (\r -> r { repos = _ })

-- taken from purescript-index, which messes up all my dependencies :(
ixMap :: forall k v. (Ord k) => k -> TraversalP (M.Map k v) v
ixMap k v2fv m = M.lookup k m # maybe (pure m) \v ->
      (\v' -> M.insert k v' m) <$> v2fv v
