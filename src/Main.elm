import DefaultDict exposing (DefaultDict)

import Graphics.Element exposing (show)


default : DefaultDict Int Int
default = DefaultDict.empty 5

magic = DefaultDict.get 1 default

main = show <| magic
