module Main exposing (Model, Msg(..), init, main, update, view, viewInput)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Decode.Extra exposing (andMap)
import Time exposing (..)



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
        |> andMap (D.field "blockNumber" D.string)
        |> andMap (D.field "timeStamp" D.string)
        |> andMap (D.field "hash" D.string)
        |> andMap (D.field "nonce" D.string)
        |> andMap (D.field "blockHash" D.string)
        |> andMap (D.field "transactionIndex" D.string)
        |> andMap (D.field "from" D.string)
        |> andMap (D.field "to" D.string)
        |> andMap (D.field "value" D.string)
        |> andMap (D.field "gas" D.string)
        |> andMap (D.field "gasPrice" D.string)
        |> andMap (D.field "isError" D.string)
        |> andMap (D.field "txreceipt_status" D.string)
        |> andMap (D.field "input" D.string)
        |> andMap (D.field "contractAddress" D.string)
        |> andMap (D.field "cumulativeGasUsed" D.string)
        |> andMap (D.field "gasUsed" D.string)
        |> andMap (D.field "confirmations" D.string)


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
        , Html.form [ class "main-form", onSubmit SubmitForm ]
            [ viewInput (PropsInput "text" "0x…" model.address) Name
            , button [ class "main-button", type_ "submit" ] [ text "Get data!" ]
            ]
        , div []
            [ case model.status of
                Success ->
                    Html.ul [] (List.map viewTransactionItem model.response.result)

                Error ->
                    text model.error

                None ->
                    text ""

                Loading ->
                    text "..."
            ]
        ]


type alias PropsInput =
    { text : String
    , placeholder : String
    , value : String
    }


viewInput : PropsInput -> (String -> msg) -> Html msg
viewInput props toMsg =
    label [] [ span [] [ text "Enter address" ], input [ class "main-input", type_ props.text, placeholder props.placeholder, value props.value, onInput toMsg ] [] ]


type alias PropsTxItem =
    { blockHash : String
    , hash : String
    , timestamp : String
    , from : String
    , to : String
    , input : String
    }


timestampToDate : String -> Posix
timestampToDate timestamp =
    let
        t =
            timestamp
                |> (++) "000"
                |> String.trim
                |> String.toInt
    in
    case t of
        Nothing ->
            millisToPosix 0

        Just time ->
            millisToPosix time


viewTransactionItem : TxData -> Html msg
viewTransactionItem props =
    li [ class "tx-item" ]
        [ a [ class "tx-item--link", href ("https://etherscan.io/tx/" ++ props.hash) ]
            [ div [ class "tx-item-address" ]
                [ div [ class "tx-item-address-from" ] [ text props.from ]
                , div [ class "tx-item-address-delimiter" ] [ text "➡️" ]
                , div [ class "tx-item-address-to" ] [ text props.to ]
                ]
            , div [ class "tx-item-timestamp" ] [ text (props.timeStamp ++ "000") ]
            ]
        ]
