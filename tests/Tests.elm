module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import DefaultDictSpec as Dict


all : Test
all =
    describe "Default dict Library Tests"
        [ Dict.tests
        ]
