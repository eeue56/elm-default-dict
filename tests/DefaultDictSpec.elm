module DefaultDictSpec (tests) where

import Basics exposing (..)
import DefaultDict as Dict
import List
import Maybe exposing (..)

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

animals : Dict.DefaultDict String String
animals =
    Dict.fromList
        "animal"
        [ ("Tom", "cat")
        , ("Jerry", "mouse") ]

tests : Test
tests =
    let
        buildTests =
            let
                defaultKV = Dict.fromList "v" [("k", "v")]
            in
                suite "build Tests"
                    [ test "empty"
                        <| assert
                        <| Dict.isEmpty
                        <| Dict.fromList "v" []

                    , test "singleton"
                        <| assertEqual defaultKV
                        <| Dict.singleton "k" "v"

                    , test "insert"
                        <| assertEqual defaultKV
                        <| Dict.insert "k" "v"
                        <| Dict.empty "v"

                    , test "insert replace"
                        <| assertEqual "vv"
                        <| Dict.get "k"
                        <| Dict.insert "k" "vv"
                        <| Dict.singleton "k" "v"

                    , test "update"
                        <| assertEqual "vv"
                        <| Dict.get "k"
                        <| Dict.update "k" (\v-> Just "vv")
                        <| Dict.singleton "k" "v"

                    , test "update Nothing"
                        <| assert
                        <| Dict.isEmpty
                        <| Dict.update "k" (\v -> Nothing)
                        <| Dict.singleton "k" "v"

                    , test "remove"
                        <| assert
                        <| Dict.isEmpty
                        <| Dict.remove "k"
                        <| Dict.singleton "k" "v"

                    , test "remove not found"
                        <| assertEqual (Dict.singleton "k" "v")
                        <| Dict.remove "kk"
                        <| Dict.singleton "k" "v"
                    ]

        queryTests =
            suite "query Tests"
                [ test "member 1" <| assertEqual True (Dict.member "Tom" animals)
                , test "member 2" <| assertEqual False (Dict.member "Spike" animals)
                , test "get 1" <| assertEqual ("cat") (Dict.get "Tom" animals)
                , test "get 2" <| assertEqual "animal" (Dict.get "Spike" animals)
                ]
        combineTests =
            suite "combine Tests"
                [ test "union" <| assertEqual animals (Dict.union (Dict.singleton "Jerry" "mouse") (Dict.singleton "Tom" "cat"))
                , test "union collison" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.union (Dict.singleton "Tom" "cat") (Dict.singleton "Tom" "mouse"))
                , test "intersect" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.intersect animals (Dict.singleton "Tom" "cat"))
                , test "diff" <| assertEqual (Dict.singleton "Jerry" "mouse") (Dict.diff animals (Dict.singleton "Tom" "cat"))
                ]
        transformTests =
            suite "transform Tests"
                [ test "filter" <| assertEqual (Dict.singleton "Tom" "cat") (Dict.filter (\k v -> k == "Tom") animals)
                , test "partition" <| assertEqual (Dict.singleton "Tom" "cat", Dict.singleton "Jerry" "mouse") (Dict.partition (\k v -> k == "Tom") animals)
                ]
  in
    suite "Dict Tests"
    [ buildTests
    , queryTests
    , combineTests
    , transformTests
    ]
