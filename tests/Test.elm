module Test exposing (..)

import ElmTest exposing (suite, runSuite, Test)

import DefaultDictSpec as Dict

tests : Test
tests =
    suite "Default dict Library Tests"
    [ Dict.tests
    ]


main = runSuite tests

