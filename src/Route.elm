module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Article.Slug as Slug exposing (Slug)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Profile exposing (Profile)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)
import Username exposing (Username)


type Route
    = Home
    | Root
    | Login
    | Logout
    | Register
    | Settings
    | Article Slug
    | Profile Username
    | NewArticle
    | EditArticle Slug


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Settings (s "settings")
        , Parser.map Profile (s "profile" </> Username.urlParser)
        , Parser.map Register (s "register")
        , Parser.map Article (s "article" </> Slug.urlParser)
        , Parser.map NewArticle (s "editor")
        , Parser.map EditArticle (s "editor" </> Slug.urlParser)
        ]


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


routeToString : Route -> String
routeToString page =
    "#/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Root ->
            []

        Login ->
            [ "login" ]

        Logout ->
            [ "logout" ]

        Register ->
            [ "register" ]

        Settings ->
            [ "settings" ]

        Article slug ->
            [ "article", Slug.toString slug ]

        Profile username ->
            [ "profile", Username.toString username ]

        NewArticle ->
            [ "editor" ]

        EditArticle slug ->
            [ "editor", Slug.toString slug ]
