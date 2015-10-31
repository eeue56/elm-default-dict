# elm-default-dict


Use default dictionaries in Elm!

```elm
-- create a default dictionary with the value 0
ages : DefaultDict String Int
ages =
    DefaultDict.empty 0

-- equal to 5
mikesAge =
    DefaultDict.insert "Mike" 5 ages
        |> DefaultDict.get "Mike"

-- equal to 0
someoneElse =
    DefaultDict.get "David" ages

everyonesAges : DefaultDict String Int
everyonesAges =
    DefaultDict.fromList
        100
        [ ("Mike", 5)
        , ("David", 0)
        , ("Tommy", 19)]

-- equal to 100
leesAge =
    DefaultDict.get "Lee" everyonesAges
```
