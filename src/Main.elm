module Main exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias TxData =
    { blockNumber : Int
    , timeStamp : Int
    , hash : String
    , nonce : Int
    , blockHash : String
    , transactionIndex : Int
    , from : String
    , to : String
    , value : String
    , gas : String
    , gasPrice : String
    , isError : Int
    , txreceipt_status : Int
    , input : String -- Hexed number
    , contractAddress : String
    , cumulativeGasUsed : String
    , gasUsed : String
    , confirmations : Int
    }


type alias ApiResponse =
    { status : Int
    , message : String

    -- , result: List String
    }


initApiResponse : ApiResponse
initApiResponse =
    ApiResponse 99 ""


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


makeApiUrl : String -> String
makeApiUrl address =
    "http://api.etherscan.io/api?module=account&action=txlist&address=" ++ address ++ "&startblock=0&endblock=99999999&sort=asc"


responseDecode : D.Decoder String
responseDecode =
    -- D.map2 ApiResponse
    -- (D.field "status" D.int)
    D.field "message" D.string



-- (D.at ["result"] (D.List D.String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name address ->
            ( { model | address = address }, Cmd.none )

        SubmitForm ->
            ( { model | status = Loading }
            , Http.get
                { expect = Http.expectJson FetchHistory responseDecode
                , url = makeApiUrl model.address
                }
            )

        FetchHistory result ->
            case result of
                Ok response ->
                    ( { model | status = Success, response = response }, Cmd.none )

                Err _ ->
                    ( { model | status = Error }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.main_ []
        [ h1 [] [ text "ΞStat" ]
        , Html.form [ onSubmit SubmitForm ]
            [ viewInput "text" "0x…" model.address Name
            , button [ type_ "submit" ] [ text "Get data!" ]
            ]
        , div []
            [ case model.status of
                Success ->
                    Html.pre [] [ text model.response ]

                Error ->
                    text "Саня, хуй соси"

                None ->
                    text ""

                Loading ->
                    text "..."
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    label [] [ span [] [ text "Enter address" ], input [ type_ t, placeholder p, value v, onInput toMsg ] [] ]
