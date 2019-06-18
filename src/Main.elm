module Main exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type FetchingStatus
    = None
    | Loading
    | Success
    | Error


type alias Model =
    { address : String
    , status : FetchingStatus
    , response : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" None "", Cmd.none )



-- UPDATE


type Msg
    = Name String
    | FetchHistory (Result Http.Error String)
    | SubmitForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name address ->
            ( { model | address = address }, Cmd.none )

        SubmitForm ->
            ( { model | status = Loading }
            , Http.get
                { expect = Http.expectString FetchHistory
                , url = "http://api.etherscan.io/api?module=account&action=txlist&address=" ++ model.address ++ "&startblock=0&endblock=99999999&sort=asc"
                }
            )

        FetchHistory result ->
            case result of
                Ok response ->
                    ( { model | status = Success, response = response }, Cmd.none )

                Err _ ->
                    ( { model | status = Error }, Cmd.none )



-- ( model
-- , Http.get
--     { url = "https://elm-lang.org/assets/public-opinion.txt"
--     , expect = Http.expectString
--     }
-- )
-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ []
        [ h1 [] [ text "ΞStat" ]
        , Html.form [ onSubmit SubmitForm ]
            [ viewInput "text" "0x…" model.address Name
            , button [ type_ "submit" ] [ text "Get data!" ]
            ]
        , pre []
            [ text
                (case model.status of
                    Success ->
                        model.response

                    Error ->
                        "Саня, хуй соси"

                    None ->
                        ""

                    Loading ->
                        "..."
                )
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    label [] [ span [] [ text "Enter address" ], input [ type_ t, placeholder p, value v, onInput toMsg ] [] ]
