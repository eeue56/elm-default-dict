module DefaultDictSpec exposing (tests)

import DefaultDict as Dict
import Maybe exposing (..)
import Test exposing (..)
import Expect


animals : Dict.DefaultDict String String
animals =
    Dict.fromList
        "animal"
        [ ( "Tom", "cat" )
        , ( "Jerry", "mouse" )
        ]


ages : Dict.DefaultDict String Int
ages =
    Dict.fromList
        100
        [ ( "Mike", 5 )
        , ( "David", 0 )
        , ( "Tommy", 19 )
        ]


tests : Test
tests =
    let
        buildTests =
            let
                defaultKV =
                    Dict.fromList "v" [ ( "k", "v" ) ]
            in
                describe "build Tests"
                    [ test "empty" <|
                        \() ->
                            Dict.fromList "v" []
                                |> Dict.isEmpty
                                |> Expect.true "is empty"
                    , test "singleton" <|
                        \() ->
                            Dict.singleton "k" "v"
                                |> Expect.equal defaultKV
                    , test "insert" <|
                        \() ->
                            Dict.empty "v"
                                |> Dict.insert "k" "v"
                                |> Expect.equal defaultKV
                    , test "insert replace" <|
                        \() ->
                            Dict.singleton "k" "v"
                                |> Dict.insert "k" "vv"
                                |> Dict.get "k"
                                |> Expect.equal "vv"
                    , test "update" <|
                        \() ->
                            Dict.singleton "k" "v"
                                |> Dict.update "k" (\v -> Just "vv")
                                |> Dict.get "k"
                                |> Expect.equal "vv"
                    , test "update Nothing" <|
                        \() ->
                            Dict.singleton "k" "v"
                                |> Dict.update "k" (\v -> Nothing)
                                |> Dict.isEmpty
                                |> Expect.true "dict is empty"
                    , test "remove" <|
                        \() ->
                            Dict.singleton "k" "v"
                                |> Dict.remove "k"
                                |> Dict.isEmpty
                                |> Expect.true "dict is empty"
                    , test "remove not found" <|
                        \() ->
                            Dict.singleton "k" "v"
                                |> Dict.remove "kk"
                                |> Expect.equal (Dict.singleton "k" "v")
                    ]

        queryTests =
            describe "query Tests"
                [ test "element equality" <|
                    \() ->
                        (Dict.eq animals animals)
                            |> Expect.true "dicts with same elements are equal"
                , test "false element equality" <|
                    \() ->
                        Dict.singleton "k" "v"
                            |> Dict.eq animals
                            |> Expect.false "dicts with different elements are not equal"
                , test "full equality" <|
                    \() ->
                        Dict.fullEq animals animals
                            |> Expect.true "dicts with same elements and base are equal"
                , test "false full equality" <|
                    \() ->
                        Dict.fromList
                            "bobby"
                            [ ( "Tom", "cat" )
                            , ( "Jerry", "mouse" )
                            ]
                            |> Dict.fullEq animals
                            |> Expect.false "dicts with same elements but different base are not equal"
                , test "size" <|
                    \() ->
                        Dict.size animals
                            |> Expect.equal 2
                , test "get base" <|
                    \() ->
                        Dict.getDefault animals
                            |> Expect.equal "animal"
                , test "member 1" <|
                    \() ->
                        Dict.member "Tom" animals
                            |> Expect.equal True
                , test "member 2" <|
                    \() ->
                        Dict.member "Spike" animals
                            |> Expect.equal False
                , test "get 1" <|
                    \() ->
                        Dict.get "Tom" animals
                            |> Expect.equal ("cat")
                , test "get 2" <|
                    \() ->
                        Dict.get "Spike" animals
                            |> Expect.equal "animal"
                ]

        combineTests =
            describe "combine Tests"
                [ test "union" <|
                    \() ->
                        Dict.singleton "Tom" "cat"
                            |> Dict.union (Dict.singleton "Jerry" "mouse")
                            |> Dict.eq animals
                            |> Expect.true "includes all elements from both dicts"
                , test "union collison" <|
                    \() ->
                        Dict.singleton "Tom" "mouse"
                            |> Dict.union (Dict.singleton "Tom" "cat")
                            |> Dict.eq (Dict.singleton "Tom" "cat")
                            |> Expect.true "precedence given to left dict"
                , test "intersect" <|
                    \() ->
                        Dict.intersect animals (Dict.singleton "Tom" "cat")
                            |> Dict.eq (Dict.singleton "Tom" "cat")
                            |> Expect.true "only includes elements present in both dicts"
                , test "diff" <|
                    \() ->
                        Dict.diff animals (Dict.singleton "Tom" "cat")
                            |> Dict.eq (Dict.singleton "Jerry" "mouse")
                            |> Expect.true "only includes elements not present in right dict"
                ]

        transformTests =
            describe "transform Tests"
                [ test "filter" <|
                    \() ->
                        Dict.filter (\k v -> k == "Tom") animals
                            |> Dict.eq (Dict.singleton "Tom" "cat")
                            |> Expect.true "retains elements matching predicate"
                , test "partition" <|
                    \() ->
                        Dict.partition (\k v -> k == "Tom") animals
                            |> (\( l, r ) ->
                                    ( Dict.eq (Dict.singleton "Tom" "cat") l
                                    , Dict.eq (Dict.singleton "Jerry" "mouse") r
                                    )
                               )
                            |> Expect.equal ( True, True )
                , test "map" <|
                    \() ->
                        Dict.fullEq
                            (Dict.map (\name age -> age + 1) ages)
                            (Dict.fromList
                                100
                                [ ( "Mike", 6 )
                                , ( "David", 1 )
                                , ( "Tommy", 20 )
                                ]
                            )
                            |> Expect.true "applies a function to each element in dict"
                , test "mapWithDefault" <|
                    \() ->
                        Dict.fullEq
                            (Dict.mapWithDefault "name" (\name _ -> name) ages)
                            (Dict.fromList
                                "name"
                                [ ( "Mike", "Mike" )
                                , ( "David", "David" )
                                , ( "Tommy", "Tommy" )
                                ]
                            )
                            |> Expect.true "boom"
                ]
    in
        describe "Dict Tests"
            [ buildTests
            , queryTests
            , combineTests
            , transformTests
            ]
