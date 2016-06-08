module Main where
--------------------------------------------------------------------------------
import Prelude
import Data.Maybe
import Data.List
import Data.Lens
import Data.Lens.Index
import OpticUI
import OpticUI.Util.Remote
import Web.Markup
import Web.Markup.Event
import Web.Markup.IncDOM
import Web.Markup.HTML as H
import Web.Markup.HTML.Attributes as A
import Web.Markup.HTML.Event as E
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff
import DOM (DOM ())
import Data.JSON                  as JS
import Network.HTTP.Affjax        as AJ
import Signal
import Signal.Channel

-- | State of the application.
type App = { name :: String, repos :: Response }

-- | Effects of the application.
type AppE = (ajax :: AJ.AJAX, ref :: REF, channel :: CHANNEL, dom :: DOM, err :: EXCEPTION)

-- | Type of the (parsed) response of the GitHub servers.
type Response = Remote Error (List String)

-- | Runs the `repoApp` component.
main = do
  response <- channel Init
  backend <- listenerBackend \go -> runSignal (map go (subscribe response))
  animate renderToBody backend { name: "", repos: Init } (repoApp response)

-- | UI component that allows to query and view the repositories of a GitHub user.
repoApp :: Channel Response -> UI (Eff AppE) Markup (Listener Response) App
repoApp ch = withState \s ->
  let
    -- listener for form submission
    submitted = do
      let url     = "https://api.github.com/users/" ++ s.name ++ "/repos"
      let fail    = send ch <<< Failed
      let success = send ch <<< Success
      runAff fail success do
        r <- JS.decode <<< _.response <$> AJ.get url
        liftEff $ maybe
          (throwException $ error "Failed to decode response.")
          (pure <<< toListOf repoNames) r
      pure (s # repos .~ Pending)

    -- listener for AJAX responses
    listener = mkSub (listen (const true) \k -> pure $ s # repos .~ k)

    -- traversal into the names of GitHub repositories
    repoNames = _JArray <<< traversed <<< _JObject <<< ix "name" <<< _JString

    -- UI for repo list
    listUI = mconcat
      [ _Failed  $ mkView $ H.p [] (text "An error occured :(")
      , _Pending $ mkView $ H.p [] (text "Fetching repos...")
      , _Success $ mconcat
        [ mkView $ tag "h2" Nothing [] (text "Repositories")
        , overView (H.ul []) $ traversed $ withState \s -> mkView $ H.li [] (text s)
        ]
      ]
  in
    mconcat
      [ listener
      , mkView $ H.h1 [] (text "GitHub Repository List 2")
      , mkView $ H.p  [] (text "Enter the name of a GitHub user:")
      , name   $ textField [ A.placeholder "Enter a user name"]
      , mkView $ H.button [ on_ E.Click submitted ] (text "Load")
      , repos listUI
      ]

textField :: forall m k. (Plus k, Applicative m) => Array (Prop (m String)) -> UI m Markup k String
textField ps = withState \s -> mkView $ H.input (ps ++ [A.value s, on E.Input pure]) mempty

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
