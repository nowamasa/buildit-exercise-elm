module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)
import Main exposing (..)
import Types exposing (..)


day1 =
    Day 1498856400 "2017-06-30 21:00:00" []


day2 =
    Day 1498856400 "2017-06-30 21:00:00" []


day3 =
    Day 1498856400 "2017-06-31 21:00:00" []


days =
    [ day1, day2, day3 ]


group1 =
    [ day1, day2 ]


group2 =
    [ day3 ]


groupedDays =
    [ group1, group2 ]


suite : Test
suite =
    describe "The Main module"
        [ test "getTimeFromDaytime" <|
            \_ ->
                Expect.equal "21:00" (getTimeFromDaytime "2017-06-30 21:00:00")
        , test "getDateFromDaytime" <|
            \_ ->
                Expect.equal "2017-06-30" (getDateFromDaytime "2017-06-30 21:00:00")
        , test "getIconUrl" <|
            \_ ->
                Expect.equal "http://openweathermap.org/img/w/foo.png" (getIconUrl "foo")
        , test "getUrl" <|
            \_ ->
                Expect.equal "AA?q=CC,DD&APPID=BB" (getUrl "AA" "BB" "CC" "DD")
        , test "groupDays" <|
            \_ ->
                Expect.equal groupedDays (groupDays days)
        , describe "getGroupTitle"
            [ test "when list is not empty" <|
                \_ ->
                    Expect.equal "2017-06-30" (getGroupTitle group1)
            , test "when list is empty" <|
                \_ ->
                    Expect.equal "" (getGroupTitle [])
            ]
        ]
