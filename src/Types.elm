module Types exposing (..)


type alias Payload =
    { cityName : String
    , countryCode : String
    }


type alias Item =
    { description : String
    , icon : String
    }


type alias Day =
    { dt : Int
    , dt_txt : String
    , items : List Item
    }


type alias Weather =
    { city : String
    , days : List Day
    }


type alias Model =
    { loading : Bool
    , errorMessage : Maybe String
    , weather : Maybe Weather
    }
