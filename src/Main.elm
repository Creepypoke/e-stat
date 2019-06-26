module Main exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Decode.Extra as DE



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
    { blockNumber : String
    , timeStamp : String
    , hash : String
    , nonce : String
    , blockHash : String
    , transactionIndex : String
    , from : String
    , to : String
    , value : String
    , gas : String
    , gasPrice : String
    , isError : String
    , txreceipt_status : String
    , input : String -- Hexed number
    , contractAddress : String
    , cumulativeGasUsed : String
    , gasUsed : String
    , confirmations : String
    }


type alias ApiResponse =
    { status : String
    , message : String
    , result : List TxData
    }


initApiResponse : ApiResponse
initApiResponse =
    ApiResponse "99" "" []


type FetchingStatus
    = None
    | Loading
    | Success
    | Error


type alias Model =
    { address : String
    , status : FetchingStatus
    , error : String
    , response : ApiResponse
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" None "" initApiResponse, Cmd.none )



-- UPDATE


type Msg
    = Name String
    | FetchHistory (Result Http.Error ApiResponse)
    | SubmitForm


makeApiUrl : String -> String
makeApiUrl address =
    "https://api.etherscan.io/api?module=account&action=txlist&address=" ++ address ++ "&startblock=0&endblock=99999999&sort=asc"


txDecoder : D.Decoder TxData
txDecoder =
    D.succeed TxData
        |> DE.andMap (D.field "blockNumber" D.string)
        |> DE.andMap (D.field "timeStamp" D.string)
        |> DE.andMap (D.field "hash" D.string)
        |> DE.andMap (D.field "nonce" D.string)
        |> DE.andMap (D.field "blockHash" D.string)
        |> DE.andMap (D.field "transactionIndex" D.string)
        |> DE.andMap (D.field "from" D.string)
        |> DE.andMap (D.field "to" D.string)
        |> DE.andMap (D.field "value" D.string)
        |> DE.andMap (D.field "gas" D.string)
        |> DE.andMap (D.field "gasPrice" D.string)
        |> DE.andMap (D.field "isError" D.string)
        |> DE.andMap (D.field "txreceipt_status" D.string)
        |> DE.andMap (D.field "input" D.string)
        |> DE.andMap (D.field "contractAddress" D.string)
        |> DE.andMap (D.field "cumulativeGasUsed" D.string)
        |> DE.andMap (D.field "gasUsed" D.string)
        |> DE.andMap (D.field "confirmations" D.string)


responseDecode : D.Decoder ApiResponse
responseDecode =
    D.map3 ApiResponse
        (D.field "status" D.string)
        (D.field "message" D.string)
        (D.field "result" (D.list txDecoder))


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
            log "Result"
                (case result of
                    Ok apiResponse ->
                        ( { model | status = Success, response = apiResponse }, Cmd.none )

                    Err error ->
                        ( { model | status = Error, error = Debug.toString error }, Cmd.none )
                )


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
                    Html.pre [] [ text model.response.message ]

                Error ->
                    text model.error

                None ->
                    text ""

                Loading ->
                    text "..."
            ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    label [] [ span [] [ text "Enter address" ], input [ type_ t, placeholder p, value v, onInput toMsg ] [] ]
