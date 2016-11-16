module DefaultDict
    exposing
        ( DefaultDict
        , empty
        , singleton
        , insert
        , update
        , get
        , remove
        , member
        , getDefault
        , eq
        , fullEq
        , size
        , isEmpty
        , filter
        , partition
        , foldl
        , foldr
        , map
        , mapWithDefault
        , union
        , intersect
        , diff
        , keys
        , values
        , toList
        , fromList
        )

{-| A dictionary mapping unique keys to values. A provided default is used for when
keys are missing. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take *O(log n)* time. DefaultDictionary
equality with `(==)` is unreliable and should not be used.

# Types
@docs DefaultDict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size, getDefault, eq, fullEq

# Combine
@docs union, intersect, diff

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, mapWithDefault, foldl, foldr, filter, partition

-}

-- where
-- This is a hack
-- As DefaultDict uses types with the same names as those used
-- internally by Elm-Core's Dict, when the runtime tries to
-- shortcut and pretty-print the Dict (as the ctor of the
-- objects match), it will cause an error if Dict has not been
-- imported at least somewhere in the program.
-- So, we import it here - and get prettyprinting for free!

import Dict as DeadDict
import Maybe exposing (..)
import List exposing (..)
import Debug
import String


-- BBlack and NBlack should only be used during the deletion
-- algorithm. Any other occurrence is a bug and should fail an assert.


type NColor
    = Red
    | Black
    | BBlack
      -- Double Black, counts as 2 blacks for the invariant
    | NBlack



-- Negative Black, counts as -1 blacks for the invariant


showNColor : NColor -> String
showNColor c =
    case c of
        Red ->
            "Red"

        Black ->
            "Black"

        BBlack ->
            "BBlack"

        NBlack ->
            "NBlack"


type LeafColor
    = LBlack
    | LBBlack



-- Double Black, counts as 2


showLColor : LeafColor -> String
showLColor color =
    case color of
        LBlack ->
            "LBlack"

        LBBlack ->
            "LBBlack"


{-| A default dict which lifts the type from Core's Dict
-}
type DefaultDict k v
    = RBNode_elm_builtin NColor k v (DefaultDict k v) (DefaultDict k v)
    | RBEmpty_elm_builtin LeafColor v


{-| Create an empty dictionary with a given default value
-}
empty : v -> DefaultDict comparable v
empty default =
    RBEmpty_elm_builtin LBlack default


{-| Element equality. Does not check equality of base
-}
eq : DefaultDict comparable v -> DefaultDict comparable v -> Bool
eq first second =
    (toList first) == (toList second)


{-| Base + element equality
-}
fullEq : DefaultDict comparable v -> DefaultDict comparable v -> Bool
fullEq first second =
    (toList first == toList second) && (getDefault first == getDefault second)


min : DefaultDict k v -> ( k, v )
min dict =
    case dict of
        RBNode_elm_builtin _ key value (RBEmpty_elm_builtin LBlack _) _ ->
            ( key, value )

        RBNode_elm_builtin _ _ _ left _ ->
            min left

        RBEmpty_elm_builtin _ v ->
            Debug.crash "(min Empty) is not defined"


max : DefaultDict k v -> ( k, v )
max dict =
    case dict of
        RBNode_elm_builtin _ key value _ (RBEmpty_elm_builtin _ _) ->
            ( key, value )

        RBNode_elm_builtin _ _ _ _ right ->
            max right

        RBEmpty_elm_builtin _ _ ->
            Debug.crash "(max Empty) is not defined"


get_ : comparable -> DefaultDict comparable v -> Maybe v
get_ targetKey dict =
    case dict of
        RBEmpty_elm_builtin _ _ ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case compare targetKey key of
                LT ->
                    get_ targetKey left

                EQ ->
                    Just value

                GT ->
                    get_ targetKey right


{-| Helper function for grabbing the default value used in the dict
-}
getDefault : DefaultDict comparable v -> v
getDefault dict =
    case dict of
        RBEmpty_elm_builtin _ v ->
            v

        RBNode_elm_builtin _ _ _ left _ ->
            getDefault left


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Mouse" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable -> DefaultDict comparable v -> v
get targetKey dict =
    case get_ targetKey dict of
        Just v ->
            v

        Nothing ->
            getDefault dict


{-| Determine if a key is in a dictionary.
-}
member : comparable -> DefaultDict comparable v -> Bool
member key dict =
    case get_ key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine if a dictionary is empty.

    isEmpty empty == True
-}
isEmpty : DefaultDict comparable v -> Bool
isEmpty dict =
    case dict of
        RBEmpty_elm_builtin _ _ ->
            True

        _ ->
            False


{-| Get the number of key-value pairs in a dict
-}
size : DefaultDict k v -> Int
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> DefaultDict k v -> Int
sizeHelp n dict =
    case dict of
        RBEmpty_elm_builtin _ _ ->
            n

        RBNode_elm_builtin _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


ensureBlackRoot : DefaultDict k v -> DefaultDict k v
ensureBlackRoot dict =
    case dict of
        RBNode_elm_builtin Red key value left right ->
            RBNode_elm_builtin Black key value left right

        RBNode_elm_builtin Black _ _ _ _ ->
            dict

        _ ->
            dict


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : comparable -> v -> DefaultDict comparable v -> DefaultDict comparable v
insert key value dict =
    update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> DefaultDict comparable v -> DefaultDict comparable v
remove key dict =
    update key (always Nothing) dict


type Flag
    = Insert
    | Remove
    | Same


showFlag : Flag -> String
showFlag f =
    case f of
        Insert ->
            "Insert"

        Remove ->
            "Remove"

        Same ->
            "Same"


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : comparable -> (v -> Maybe v) -> DefaultDict comparable v -> DefaultDict comparable v
update k alter dict =
    let
        base =
            getDefault dict

        empty_ =
            empty base

        up dict =
            case dict of
                RBEmpty_elm_builtin _ v ->
                    case alter base of
                        Nothing ->
                            ( Same, empty_ )

                        Just v ->
                            ( Insert, RBNode_elm_builtin Red k v empty_ empty_ )

                RBNode_elm_builtin clr key value left right ->
                    case compare k key of
                        EQ ->
                            case alter value of
                                Nothing ->
                                    ( Remove, rem clr left right )

                                Just newValue ->
                                    ( Same, RBNode_elm_builtin clr key newValue left right )

                        LT ->
                            let
                                ( flag, newLeft ) =
                                    up left
                            in
                                case flag of
                                    Same ->
                                        ( Same, RBNode_elm_builtin clr key value newLeft right )

                                    Insert ->
                                        ( Insert, balance clr key value newLeft right )

                                    Remove ->
                                        ( Remove, bubble clr key value newLeft right )

                        GT ->
                            let
                                ( flag, newRight ) =
                                    up right
                            in
                                case flag of
                                    Same ->
                                        ( Same, RBNode_elm_builtin clr key value left newRight )

                                    Insert ->
                                        ( Insert, balance clr key value left newRight )

                                    Remove ->
                                        ( Remove, bubble clr key value left newRight )

        ( flag, updatedDict ) =
            up dict
    in
        case flag of
            Same ->
                updatedDict

            Insert ->
                ensureBlackRoot updatedDict

            Remove ->
                blacken updatedDict


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> v -> DefaultDict comparable v
singleton key value =
    insert key value (empty value)


isBBlack : DefaultDict k v -> Bool
isBBlack dict =
    case dict of
        RBNode_elm_builtin BBlack _ _ _ _ ->
            True

        RBEmpty_elm_builtin LBBlack v ->
            True

        _ ->
            False


moreBlack : NColor -> NColor
moreBlack color =
    case color of
        Black ->
            BBlack

        Red ->
            Black

        NBlack ->
            Red

        BBlack ->
            Debug.crash "Can't make a double black node more black!"


lessBlack : NColor -> NColor
lessBlack color =
    case color of
        BBlack ->
            Black

        Black ->
            Red

        Red ->
            NBlack

        NBlack ->
            Debug.crash "Can't make a negative black node less black!"


lessBlackTree : DefaultDict k v -> DefaultDict k v
lessBlackTree dict =
    case dict of
        RBNode_elm_builtin c k v l r ->
            RBNode_elm_builtin (lessBlack c) k v l r

        RBEmpty_elm_builtin LBBlack v ->
            RBEmpty_elm_builtin LBlack v

        _ ->
            dict


reportRemBug : String -> NColor -> String -> String -> a
reportRemBug msg c lgot rgot =
    Debug.crash <|
        String.concat
            [ "Internal red-black tree invariant violated, expected "
            , msg
            , " and got "
            , showNColor c
            , "/"
            , lgot
            , "/"
            , rgot
            , "\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"
            ]



-- Remove the top node from the tree, may leave behind BBlacks


rem : NColor -> DefaultDict k v -> DefaultDict k v -> DefaultDict k v
rem c l r =
    case ( l, r ) of
        ( RBEmpty_elm_builtin _ v, RBEmpty_elm_builtin _ _ ) ->
            case c of
                Red ->
                    RBEmpty_elm_builtin LBlack v

                Black ->
                    RBEmpty_elm_builtin LBBlack v

                _ ->
                    Native.Debug.crash "cannot have bblack or nblack nodes at this point"

        ( RBEmpty_elm_builtin cl v, RBNode_elm_builtin cr k_ v_ l_ r_ ) ->
            case ( c, cl, cr ) of
                ( Black, LBlack, Red ) ->
                    RBNode_elm_builtin Black k_ v_ l_ r_

                _ ->
                    reportRemBug "Black/LBlack/Red" c (showLColor cl) (showNColor cr)

        ( RBNode_elm_builtin cl k_ v_ l_ r_, RBEmpty_elm_builtin cr v ) ->
            case ( c, cl, cr ) of
                ( Black, Red, LBlack ) ->
                    RBNode_elm_builtin Black k_ v_ l_ r_

                _ ->
                    reportRemBug "Black/Red/LBlack" c (showNColor cl) (showLColor cr)

        -- l and r are both RBNode_elm_builtins
        ( RBNode_elm_builtin cl kl vl ll rl, RBNode_elm_builtin cr kr vr lr rr ) ->
            let
                l =
                    RBNode_elm_builtin cl kl vl ll rl

                r =
                    RBNode_elm_builtin cr kr vr lr rr

                ( k, v ) =
                    max l

                l_ =
                    remove_max cl kl vl ll rl
            in
                bubble c k v l_ r



-- Kills a BBlack or moves it upward, may leave behind NBlack


bubble : NColor -> k -> v -> DefaultDict k v -> DefaultDict k v -> DefaultDict k v
bubble c k v l r =
    if isBBlack l || isBBlack r then
        balance (moreBlack c) k v (lessBlackTree l) (lessBlackTree r)
    else
        RBNode_elm_builtin c k v l r



-- Removes rightmost node, may leave root as BBlack


remove_max : NColor -> k -> v -> DefaultDict k v -> DefaultDict k v -> DefaultDict k v
remove_max c k v l r =
    case r of
        RBEmpty_elm_builtin _ v ->
            rem c l r

        RBNode_elm_builtin cr kr vr lr rr ->
            bubble c k v l (remove_max cr kr vr lr rr)



-- generalized tree balancing act


balance : NColor -> k -> v -> DefaultDict k v -> DefaultDict k v -> DefaultDict k v
balance c k v l r =
    balance_node (RBNode_elm_builtin c k v l r)


blackish : DefaultDict k v -> Bool
blackish t =
    case t of
        RBNode_elm_builtin c _ _ _ _ ->
            c == Black || c == BBlack

        RBEmpty_elm_builtin _ v ->
            True


balance_node : DefaultDict k v -> DefaultDict k v
balance_node t =
    let
        assemble col xk xv yk yv zk zv a b c d =
            RBNode_elm_builtin (lessBlack col) yk yv (RBNode_elm_builtin Black xk xv a b) (RBNode_elm_builtin Black zk zv c d)
    in
        if blackish t then
            case t of
                RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red yk yv (RBNode_elm_builtin Red xk xv a b) c) d ->
                    assemble col xk xv yk yv zk zv a b c d

                RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red xk xv a (RBNode_elm_builtin Red yk yv b c)) d ->
                    assemble col xk xv yk yv zk zv a b c d

                RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red zk zv (RBNode_elm_builtin Red yk yv b c) d) ->
                    assemble col xk xv yk yv zk zv a b c d

                RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red yk yv b (RBNode_elm_builtin Red zk zv c d)) ->
                    assemble col xk xv yk yv zk zv a b c d

                RBNode_elm_builtin BBlack xk xv a (RBNode_elm_builtin NBlack zk zv (RBNode_elm_builtin Black yk yv b c) d) ->
                    case d of
                        RBNode_elm_builtin Black _ _ _ _ ->
                            RBNode_elm_builtin Black yk yv (RBNode_elm_builtin Black xk xv a b) (balance Black zk zv c (redden d))

                        _ ->
                            t

                RBNode_elm_builtin BBlack zk zv (RBNode_elm_builtin NBlack xk xv a (RBNode_elm_builtin Black yk yv b c)) d ->
                    case a of
                        RBNode_elm_builtin Black _ _ _ _ ->
                            RBNode_elm_builtin Black yk yv (balance Black xk xv (redden a) b) (RBNode_elm_builtin Black zk zv c d)

                        _ ->
                            t

                _ ->
                    t
        else
            t



-- make the top node black


blacken : DefaultDict k v -> DefaultDict k v
blacken t =
    case t of
        RBEmpty_elm_builtin _ v ->
            RBEmpty_elm_builtin LBlack v

        RBNode_elm_builtin _ k v l r ->
            RBNode_elm_builtin Black k v l r



-- make the top node red


redden : DefaultDict k v -> DefaultDict k v
redden t =
    case t of
        RBEmpty_elm_builtin _ v ->
            Debug.crash "can't make a Leaf red"

        RBNode_elm_builtin _ k v l r ->
            RBNode_elm_builtin Red k v l r


{-| Apply a function to all values in a dictionary.
Notice that this function takes a function of type `comparable -> a -> a`,
rather than Dict's `comparable a -> b`. If you want to provide a new default,
see mapWithDefault
-}
map : (comparable -> a -> a) -> DefaultDict comparable a -> DefaultDict comparable a
map f dict =
    case dict of
        RBEmpty_elm_builtin _ v ->
            dict

        RBNode_elm_builtin clr key value left right ->
            RBNode_elm_builtin clr key (f key value) (map f left) (map f right)


{-| Like map, but allows you to provide a default value too.
When mapping from type `a` to type `b`, the old default of
type `a` won't work as it's not possible to have `DefaultDict compareable (a|b)`
If this is what you want, use an `Either` type instead.
-}
mapWithDefault : b -> (comparable -> a -> b) -> DefaultDict comparable a -> DefaultDict comparable b
mapWithDefault default f dict =
    case dict of
        RBEmpty_elm_builtin _ v ->
            RBEmpty_elm_builtin LBlack default

        RBNode_elm_builtin clr key value left right ->
            RBNode_elm_builtin clr key (f key value) (mapWithDefault default f left) (mapWithDefault default f right)


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (comparable -> v -> b -> b) -> b -> DefaultDict comparable v -> b
foldl f acc dict =
    case dict of
        RBEmpty_elm_builtin _ v ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldl f (f key value (foldl f acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (comparable -> v -> b -> b) -> b -> DefaultDict comparable v -> b
foldr f acc t =
    case t of
        RBEmpty_elm_builtin _ v ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldr f (f key value (foldr f acc right)) left


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : DefaultDict comparable v -> DefaultDict comparable v -> DefaultDict comparable v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : DefaultDict comparable v -> DefaultDict comparable v -> DefaultDict comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : DefaultDict comparable v -> DefaultDict comparable v -> DefaultDict comparable v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| Get all of the keys in a dictionary.
-}
keys : DefaultDict comparable v -> List comparable
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary.
-}
values : DefaultDict comparable v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs.
-}
toList : DefaultDict comparable v -> List ( comparable, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
Takes a default value, and a list of key-pair tuples
-}
fromList : v -> List ( comparable, v ) -> DefaultDict comparable v
fromList default assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) (empty default) assocs


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> DefaultDict comparable v -> DefaultDict comparable v
filter predicate dictionary =
    let
        add key value dict =
            if predicate key value then
                insert key value dict
            else
                dict
    in
        foldl add (empty (getDefault dictionary)) dictionary


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (comparable -> v -> Bool) -> DefaultDict comparable v -> ( DefaultDict comparable v, DefaultDict comparable v )
partition predicate dict =
    let
        add key value ( t1, t2 ) =
            if predicate key value then
                ( insert key value t1, t2 )
            else
                ( t1, insert key value t2 )

        base =
            getDefault dict
    in
        foldl add ( empty base, empty base ) dict
