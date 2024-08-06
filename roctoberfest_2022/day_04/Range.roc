module [Range, new, min, max, intersection, isContained]

Range := { first : U64, last : U64 } implements [Eq { isEq: isEq }]

new : U64, U64 -> Range
new = \a, b -> @Range { first: a, last: b }

min : Range -> U64
min = \@Range { first, last } -> if first <= last then first else last

max : Range -> U64
max = \@Range { first, last } -> if first <= last then last else first

isEq : Range, Range -> Bool
isEq = \rangeA, rangeB ->
    min rangeA == min rangeB && max rangeA == max rangeB

intersection : Range, Range -> [Disjoint, Intersecting Range]
intersection = \rangeA, rangeB ->
    if min rangeA > max rangeB || min rangeB > max rangeA then
        Disjoint
    else
        first = Num.max (min rangeA) (min rangeB)
        last = Num.min (max rangeA) (max rangeB)
        Intersecting (new first last)

expect intersection (new 0 5) (new 1 3) == Intersecting (new 1 3)

isContained : Range, Range -> Bool
isContained = \inner, outer ->
    intersection inner outer == Intersecting inner
