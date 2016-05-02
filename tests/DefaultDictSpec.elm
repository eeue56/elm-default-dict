module DefaultDictSpec exposing (tests)

import DefaultDict as Dict
import Maybe exposing (..)

import ElmTest exposing (..)

animals : Dict.DefaultDict String String
animals =
    Dict.fromList
        "animal"
        [ ("Tom", "cat")
        , ("Jerry", "mouse") ]

ages : Dict.DefaultDict String Int
ages =
    Dict.fromList
        100
        [ ("Mike", 5)
        , ("David", 0)
        , ("Tommy", 19)]

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
                [ test "element equality" <| assert (Dict.eq animals animals)
                , test "false element equality"
                    <| assert
                    <| not
                    <| Dict.eq animals
                    <| Dict.singleton "k" "v"

                , test "full equality" <| assert (Dict.fullEq animals animals)
                , test "false full equality"
                    <| assert
                    <| not
                    <| Dict.fullEq animals
                    <| Dict.fromList
                        "bobby"
                        [ ("Tom", "cat")
                        , ("Jerry", "mouse") ]

                , test "size" <| assertEqual 2 <| Dict.size animals
                , test "get base" <| assertEqual "animal" <| Dict.getDefault animals

                , test "member 1" <| assertEqual True (Dict.member "Tom" animals)
                , test "member 2" <| assertEqual False (Dict.member "Spike" animals)
                , test "get 1" <| assertEqual ("cat") (Dict.get "Tom" animals)
                , test "get 2" <| assertEqual "animal" (Dict.get "Spike" animals)
                ]
        combineTests =
            suite "combine Tests"
                [ test "union"
                    <| assert
                    <| Dict.eq animals
                    <| Dict.union (Dict.singleton "Jerry" "mouse")
                    <| Dict.singleton "Tom" "cat"

                , test "union collison"
                    <| assert
                    <| Dict.eq (Dict.singleton "Tom" "cat")
                    <| Dict.union (Dict.singleton "Tom" "cat")
                    <| Dict.singleton "Tom" "mouse"

                , test "intersect"
                    <| assert
                    <| Dict.eq (Dict.singleton "Tom" "cat")
                    <| (Dict.intersect animals (Dict.singleton "Tom" "cat"))
                , test "diff"
                    <| assert
                    <| Dict.eq (Dict.singleton "Jerry" "mouse")
                    <| (Dict.diff animals (Dict.singleton "Tom" "cat"))
                ]
        transformTests =
            suite "transform Tests"
                [ test "filter"
                    <| assert
                    <| Dict.eq (Dict.singleton "Tom" "cat")
                    <| Dict.filter (\k v -> k == "Tom") animals
                , test "partition"
                    <| assertEqual (True, True)
                    <| (\(l, r) ->
                        ( Dict.eq (Dict.singleton "Tom" "cat") l
                        , Dict.eq (Dict.singleton "Jerry" "mouse") r)
                       )
                    <| Dict.partition (\k v -> k == "Tom") animals

                , test "map"
                    <| assert
                    <| Dict.fullEq (
                        Dict.fromList
                            100
                            [ ("Mike", 6)
                            , ("David", 1)
                            , ("Tommy", 20)]
                        )
                    <| Dict.map (\name age -> age + 1)
                    <| ages

                , test "mapWithDefault"
                    <| assert
                    <| Dict.fullEq (
                        Dict.fromList
                            "name"
                            [ ("Mike", "Mike")
                            , ("David", "David")
                            , ("Tommy", "Tommy")]
                        )
                    <| Dict.mapWithDefault "name" (\name _ -> name)
                    <| ages
                ]
  in
    suite "Dict Tests"
    [ buildTests
    , queryTests
    , combineTests
    , transformTests
    ]
