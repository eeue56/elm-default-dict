import DefaultDict exposing (DefaultDict)

import Graphics.Element exposing (show)

default : DefaultDict Int Int
default = DefaultDict.empty 5

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
        0
        [ ("Mike", 5)
        , ("David", 0)
        , ("Tommy", 19)]

magic = DefaultDict.get 1 default

--main = show <| DefaultDict.toList <| DefaultDict.update "Mike" (\age -> Just (age - 1)) everyonesAges

main = show <| show
