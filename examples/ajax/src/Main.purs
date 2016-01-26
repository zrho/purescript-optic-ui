module Main where
--------------------------------------------------------------------------------
import Prelude
import Data.Lens
import Data.Lens.Index            (ix)
import OpticUI
import OpticUI.Components
import OpticUI.Util.Remote
import OpticUI.Util.Async
import OpticUI.Markup.HTML        as H
import Data.Array                 as A
import Data.Either                (Either (..))
import Data.Maybe                 (Maybe (..), maybe)
import Data.Foldable              (mconcat)
import Data.Traversable           (traverse)
import Data.JSON                  as JS
import Network.HTTP.Affjax        as AJ
import Control.Monad.Eff
--------------------------------------------------------------------------------

type App = { name :: String, repos :: Remote Unit JS.JValue }

main = animate { name: "", repos: Init } repoApp

-- | Application that allows a user to view the list of repositories of a
-- | GitHub user. On submission, the application sends an AJAX request to
-- | GitHub and receives the result as a signal.
repoApp :: forall eff. UI_
  (ajax :: AJ.AJAX | eff) (Eff (ajax :: AJ.AJAX | eff))
  (Maybe JS.JValue) Markup App
repoApp = withAsync $ with \s h -> let
  submitted _ = update h $ do
    let url = "https://api.github.com/users/" ++ s.name ++ "/repos"
    async (JS.decode <<< _.response <$> AJ.get url) (const Nothing)
    pure $ s # repos .~ Pending
  loaded ma = updatePure h $ case ma of
    Just a  -> s # repos .~ Success a
    Nothing -> s # repos .~ Failed unit
  in mconcat
    [ ui $ H.h1_ $ text "GitHub Repository List"
    , ui $ H.p_ $ text "Enter the name of a GitHub user:"
    , name $ textField [ H.placeholderA "Enter a user name" ]
    , ui $ H.button [ H.onClick submitted ] $ text "Load"
    , repos $ mconcat
      [ _Failed $ ui $ H.p_ $ text "An error occured :("
      , _Pending $ ui $ H.p_ $ text "Fetching repositories..."
      , _Success repoList
      ]
    , listen (const true) loaded
    ]

-- | User interface component that displays the result of the GitHub API call.
repoList :: forall eff m k. (Functor m) => UI_ eff m k Markup JS.JValue
repoList = with \s h -> mconcat
  [ ui $ H.h2_ $ text "Repositories"
  , withView H.ul_ $ traversal
    (_JArray <<< traversed <<< _JObject <<< ix "name" <<< _JString)
    $ with \t _ -> ui $ H.li_ $ text t
  , _JArray <<< filtered A.null $ ui $ H.p_ $ text "There do not seem to be any repos."
  ]

--------------------------------------------------------------------------------
-- A huge list of lenses and prisms. Having to define this in user code is
-- obviously annoying; it might be worthwhile to revive refractor some time.

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

name = lens _.name (_ { name = _ })
repos = lens _.repos (_ { repos = _ })
