module Test where

import Graphics.Element exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.Element as ElementRunner
import ElmTest.Runner.String  as StringRunner
import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)

import DefaultDictSpec as Dict

tests : Test
tests =
    suite "Default dict Library Tests"
    [ Dict.tests
    ]


results : String
results = StringRunner.runDisplay tests


main = ElementRunner.runDisplay tests

