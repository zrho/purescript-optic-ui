module Main where
--------------------------------------------------------------------------------
import           Prelude
import           Data.Lens
import           Data.Lens.Index            (ix)
import           OpticUI
import           OpticUI.Components
import           OpticUI.Components.Async
import qualified OpticUI.Markup.HTML        as H
import qualified Data.Array                 as A
import           Data.Either                (Either (..))
import           Data.Maybe                 (Maybe (..))
import           Data.Foldable              (mconcat)
import           Data.Traversable           (traverse)
import qualified Data.JSON                  as JS
import qualified Network.HTTP.Affjax        as AJ
--------------------------------------------------------------------------------

main = animate { name: "", repos: Nothing } $ with \s h -> let
  url = "https://api.github.com/users/" ++ s.name ++ "/repos"
  submitted _ = do
    r <- async $ JS.decode <<< _.response <$> AJ.get url
    runHandler h (s # repos ?~ Left r)
  loaded a  = runHandler h $ s # repos ?~ Right a
  failure _ = runHandler h $ s # repos ?~ Right Nothing
  in mconcat
    [ ui $ H.h1_ $ text "GitHub Repository List"
    , ui $ H.p_ $ text "Enter the name of a GitHub user:"
    , name $ textField [ H.placeholderA "Enter a user name" ]
    , ui $ H.button [ H.onClick submitted ] $ text "Load"
    , repos <<< _Just $ mconcat
      [ _Left $ mconcat
        [ ui $ H.p_ $ text "Fetching repositories..."
        , onResult loaded failure
        ]
      , _Right <<< _Just $ repoList
      , _Right <<< _Nothing $ ui $ H.p_ $ text "An error occured :("
      ]
    ]

repoList = with \s h -> mconcat
  [ ui $ H.h2_ $ text "Repositories"
  , withView H.ul_ $ traversal
    (_JArray <<< traversed <<< _JObject <<< ix "name" <<< _JString)
    $ with \s _ -> ui $ H.li_ $ text s
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