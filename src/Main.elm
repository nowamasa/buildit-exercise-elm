module Main exposing (..)

import Config exposing (..)
import Decoders exposing (..)
import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (decodeValue, Value)
import Task
import Http
import Time
import List.Extra


main : Program Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = FetchWeather (Result Http.Error Weather)


decodePayload : Value -> Maybe Payload
decodePayload payload =
    case decodeValue payloadDecoder payload of
        Err _ ->
            Nothing

        Ok data ->
            Just data


init : Value -> ( Model, Cmd Msg )
init payload =
    case decodePayload payload of
        Nothing ->
            ( Model True (Just "Parsing payload failed") Nothing, Cmd.none )

        Just decodedPayload ->
            let
                url =
                    getUrl apiUrl apiKey decodedPayload.cityName decodedPayload.countryCode
            in
                ( Model True Nothing Nothing, send url )


getErrorMessage : Http.Error -> String
getErrorMessage err =
    case err of
        Http.BadPayload _ _ ->
            "Parsing response failed"

        _ ->
            toString err


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchWeather (Ok weather) ->
            ( { model | loading = False, weather = Just weather }, Cmd.none )

        FetchWeather (Err err) ->
            ( { model | loading = False, errorMessage = Just (getErrorMessage err) }, Cmd.none )


getGroupTitle : List Day -> String
getGroupTitle groupedDay =
    case List.head groupedDay of
        Nothing ->
            ""

        Just day ->
            getDateFromDaytime day.dt_txt


getDiv : String -> Html Msg -> Html Msg
getDiv className content =
    div [ class className ] [ content ]


getItemSection : String -> Item -> Html Msg
getItemSection time item =
    div []
        [ div [] [ text time ]
        , div [] [ text item.description ]
        , img [ src <| getIconUrl item.icon, alt item.description ] []
        ]


getDaySection : Day -> Html Msg
getDaySection day =
    div [ class "day-weather-box" ]
        [ div [ class "box-content" ] (List.map (getItemSection <| getTimeFromDaytime day.dt_txt) day.items)
        ]


getDayGroupSection : String -> List Day -> Html Msg
getDayGroupSection heading groupedDay =
    div [ class "day" ]
        [ h3 [ class "date-heading" ] [ text heading ]
        , div [ class "clearfix" ] (List.map getDaySection groupedDay)
        ]


getWeatherSection : String -> List (List Day) -> Html Msg
getWeatherSection city groupedDays =
    div []
        [ h2 [ class "city-heading" ] [ text city ]
        , section [] (List.map (\groupedDay -> getDayGroupSection (getGroupTitle groupedDay) groupedDay) groupedDays)
        ]


groupDays : List Day -> List (List Day)
groupDays days =
    List.Extra.groupWhile (\dayA dayB -> getDateFromDaytime dayA.dt_txt == getDateFromDaytime dayB.dt_txt) days


view : Model -> Html Msg
view model =
    let
        layout =
            getDiv "weather-viewer-component"
    in
        case model.errorMessage of
            Just errorMessage ->
                layout <| getDiv "error-msg" <| text errorMessage

            Nothing ->
                case model.loading of
                    True ->
                        layout <| getDiv "loading-msg" <| text "Loading weather..."

                    False ->
                        case model.weather of
                            Nothing ->
                                layout <| getDiv "error-msg" <| text "No weather to show"

                            Just weather ->
                                layout <| getWeatherSection weather.city <| groupDays weather.days


getWeather : String -> Http.Request Weather
getWeather url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "accept" "application/json" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson weatherDecoder
        , timeout = Just (Time.second * timeout)
        , withCredentials = False
        }


send : String -> Cmd Msg
send url =
    Http.send FetchWeather <| getWeather url


getUrl : String -> String -> String -> String -> String
getUrl apiUrl apiKey cityName countryCode =
    apiUrl ++ "?q=" ++ cityName ++ "," ++ countryCode ++ "&APPID=" ++ apiKey


getIconUrl : String -> String
getIconUrl icon =
    "http://openweathermap.org/img/w/" ++ icon ++ ".png"


getDateFromDaytime : String -> String
getDateFromDaytime daytime =
    String.slice 0 10 daytime


getTimeFromDaytime : String -> String
getTimeFromDaytime daytime =
    String.slice 11 16 daytime
