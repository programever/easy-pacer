module Core.Data.NonEmpty exposing
    ( NonEmpty
    , fromList, singleton, cons
    , head, tail, toList, length, map, filterToList, foldl, minimumBy, sortBy
    )

{-| A list that is guaranteed to hold at least one element. Used for route
points and checkpoints so that "the first point" is a value, not a `Maybe`.
-}


type NonEmpty a
    = NonEmpty a (List a)


singleton : a -> NonEmpty a
singleton value =
    NonEmpty value []


cons : a -> NonEmpty a -> NonEmpty a
cons value (NonEmpty first rest) =
    NonEmpty value (first :: rest)


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


foldl : (a -> b -> b) -> b -> NonEmpty a -> b
foldl step initial nonEmpty =
    List.foldl step initial (toList nonEmpty)


minimumBy : (a -> Float) -> NonEmpty a -> a
minimumBy score (NonEmpty first rest) =
    List.foldl
        (\candidate best ->
            if score candidate < score best then
                candidate

            else
                best
        )
        first
        rest


sortBy : (a -> comparable) -> NonEmpty a -> NonEmpty a
sortBy key nonEmpty =
    case List.sortBy key (toList nonEmpty) of
        first :: rest ->
            NonEmpty first rest

        [] ->
            nonEmpty
