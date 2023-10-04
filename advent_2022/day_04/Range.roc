interface Range exposes [Range, new, min, max, intersection, isContained] imports []

Range a : { first : Num a, last : Num a } where a implements Bool.Eq

new : Num a, Num a -> Range a
new = \a, b -> { first: a, last: b }

min : Range a -> Num a
min = \{ first, last } -> if first <= last then first else last

max : Range a -> Num a
max = \{ first, last } -> if first <= last then last else first

intersection : Range a, Range a -> [Disjoint, Intersecting (Range a)]
intersection = \rangeA, rangeB ->
    if min rangeA > max rangeB || min rangeB > max rangeA then
        Disjoint
    else
        first = Num.max (min rangeA) (min rangeB)
        last = Num.min (max rangeA) (max rangeB)
        Intersecting (new first last)

expect intersection (new 0 5) (new 1 3) == Intersecting (new 1 3)

isContained : Range a, Range a -> Bool
isContained = \inner, outer ->
    intersection inner outer == Intersecting inner
