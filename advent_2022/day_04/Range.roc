interface Range exposes [Range, new, min, max, intersection, isContained] imports []

Range : { first : Nat, last : Nat }

new : Nat, Nat -> Range
new = \a, b -> { first: a, last: b }

min : Range -> Nat
min = \{ first, last } -> if first <= last then first else last

max : Range -> Nat
max = \{ first, last } -> if first <= last then last else first

isEqual : Range, Range -> Bool
isEqual = \rangeA, rangeB ->
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
    when intersection inner outer is
        Disjoint -> Bool.false
        Intersecting range -> isEqual range inner
