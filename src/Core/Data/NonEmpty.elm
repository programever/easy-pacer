module Core.Data.NonEmpty exposing
    ( NonEmpty
    , filterToList
    , fromList
    , head
    , length
    , map
    , singleton
    , sortBy
    , tail
    , toList
    )

{-| A list that is guaranteed to hold at least one element. Used for route
points and checkpoints so that "the first point" is a value, not a `Maybe`.
-}


type NonEmpty a
    = NonEmpty a (List a)


singleton : a -> NonEmpty a
singleton value =
    NonEmpty value []


fromList : List a -> Maybe (NonEmpty a)
fromList list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Just (NonEmpty first rest)


head : NonEmpty a -> a
head (NonEmpty first _) =
    first


tail : NonEmpty a -> List a
tail (NonEmpty _ rest) =
    rest


toList : NonEmpty a -> List a
toList (NonEmpty first rest) =
    first :: rest


length : NonEmpty a -> Int
length nonEmpty =
    List.length (toList nonEmpty)


map : (a -> b) -> NonEmpty a -> NonEmpty b
map f (NonEmpty first rest) =
    NonEmpty (f first) (List.map f rest)


filterToList : (a -> Bool) -> NonEmpty a -> List a
filterToList predicate nonEmpty =
    List.filter predicate (toList nonEmpty)


sortBy : (a -> comparable) -> NonEmpty a -> NonEmpty a
sortBy key nonEmpty =
    case List.sortBy key (toList nonEmpty) of
        first :: rest ->
            NonEmpty first rest

        [] ->
            nonEmpty
