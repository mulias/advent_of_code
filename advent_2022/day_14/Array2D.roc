interface Array2D exposes [
        Shape,
        Index,
        init,
        initWithList,
        initWithLists,
        repeat,
        fromList,
        fromLists,
        fromExactList,
        fromExactLists,
        shape,
        size,
        isRowStart,
        isRowEnd,
        isColStart,
        isColEnd,
        lastRow,
        lastCol,
        set,
        get,
        update,
        map,
        mapWithIndex,
        walk,
        walkUntil,
        toList,
        toLists,
        reshape,
        transpose,
        rotateClockwise,
        rotateCounterClockwise,
        countIf,
        findFirstIndex,
        findLastIndex,
    ] imports []

Shape : { dimX : Nat, dimY : Nat }

Index : { x : Nat, y : Nat }

Array2D a := { data : List a, shape : Shape } implements [Eq { isEq: isEq }]

init : Shape, (Index -> a) -> Array2D a
init = \arrayShape, fn ->
    mapWithIndex (repeat Empty arrayShape) \_elem, index -> fn index

expect
    init { dimX: 4, dimY: 2 } \{ x, y } -> (x, y)
    == @Array2D { data: [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1), (3, 0), (3, 1)], shape: { dimX: 4, dimY: 2 } }

initWithList : List a, Shape, (Result a [NotEnoughElements], Index -> b) -> Array2D b
initWithList = \list, arrayShape, fn ->
    list
    |> List.map Ok
    |> fromList (Fill (Err NotEnoughElements) arrayShape)
    |> mapWithIndex fn

expect
    initWithList [1, 2, 3] { dimX: 2, dimY: 3 } \elem, _ -> elem |> Result.map Some |> Result.withDefault Empty
    == @Array2D { data: [Some 1, Some 2, Some 3, Empty, Empty, Empty], shape: { dimX: 2, dimY: 3 } }

initWithLists : List (List a), Shape, (Result a [NotEnoughElements], Index -> b) -> Array2D b
initWithLists = \lists, arrayShape, fn ->
    lists
    |> List.map \list -> List.map list Ok
    |> fromLists (Fill (Err NotEnoughElements) arrayShape)
    |> mapWithIndex fn

expect
    initWithLists [[1, 2, 3, 4], [1, 2]] { dimX: 2, dimY: 3 } \elem, _ -> elem |> Result.map Some |> Result.withDefault Empty
    == @Array2D { data: [Some 1, Some 2, Some 3, Some 1, Some 2, Empty], shape: { dimX: 2, dimY: 3 } }

repeat : a, Shape -> Array2D a
repeat = \elem, arrayShape ->
    @Array2D { data: List.repeat elem (shapeSize arrayShape), shape: arrayShape }

expect repeat Empty { dimX: 3, dimY: 2 } == @Array2D { data: [Empty, Empty, Empty, Empty, Empty, Empty], shape: { dimX: 3, dimY: 2 } }

fromList : List a, [Fit, Fill a Shape] -> Array2D a
fromList = \list, stratagy ->
    array = @Array2D { data: list, shape: { dimX: 1, dimY: List.len list } }
    when stratagy is
        Fit -> array
        Fill defaultElem arrayShape -> reshape array defaultElem arrayShape

fromLists : List (List a), [FitShortest, FitLongest a, Fill a Shape] -> Array2D a
fromLists = \lists, stratagy ->
    when stratagy is
        FitShortest ->
            arrayShape = {
                dimX: List.len lists,
                dimY: lists |> List.map List.len |> List.min |> Result.withDefault 0,
            }
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk lists startData \acc, list ->
                List.concat acc (List.takeFirst list arrayShape.dimY)

            @Array2D { data, shape: arrayShape }

        FitLongest defaultElem ->
            arrayShape = {
                dimX: List.len lists,
                dimY: lists |> List.map List.len |> List.max |> Result.withDefault 0,
            }
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk lists startData \acc, list ->
                List.concat acc (resize list defaultElem arrayShape.dimY)

            @Array2D { data, shape: arrayShape }

        Fill defaultElem arrayShape ->
            paddedLists = resize lists [] arrayShape.dimX
            startData = List.withCapacity (shapeSize arrayShape)
            data = List.walk paddedLists startData \acc, list ->
                List.concat acc (resize list defaultElem arrayShape.dimY)

            @Array2D { data, shape: arrayShape }

expect
    fromLists [[1, 2, 3], [4]] FitShortest
    == @Array2D { data: [1, 4], shape: { dimX: 2, dimY: 1 } }

expect
    fromLists [[1, 2, 3], [4]] (FitLongest 0)
    == @Array2D { data: [1, 2, 3, 4, 0, 0], shape: { dimX: 2, dimY: 3 } }

expect
    fromLists [[1, 2, 3], [4]] (Fill -1 { dimX: 4, dimY: 2 })
    == @Array2D { data: [1, 2, 4, -1, -1, -1, -1, -1], shape: { dimX: 4, dimY: 2 } }

fromExactList : List a, Shape -> Result (Array2D a) [NotEnoughElements, TooManyElements]
fromExactList = \list, arrayShape ->
    if List.len list == shapeSize arrayShape then
        Ok (@Array2D { data: list, shape: arrayShape })
    else if List.len list < shapeSize arrayShape then
        Err NotEnoughElements
    else
        Err TooManyElements

expect
    fromExactList [1, 2, 3, 4, 5, 6] { dimX: 2, dimY: 3 }
    == Ok (@Array2D { data: [1, 2, 3, 4, 5, 6], shape: { dimX: 2, dimY: 3 } })

expect fromExactList [1, 2, 3, 4] { dimX: 2, dimY: 3 } == Err NotEnoughElements

expect fromExactList [1, 2, 3, 4, 5, 6, 7] { dimX: 2, dimY: 3 } == Err TooManyElements

fromExactLists : List (List a) -> Result (Array2D a) [InconsistentRowLengths]
fromExactLists = \lists ->
    first = lists |> List.first |> Result.withDefault []

    if List.all lists \list -> List.len list == List.len first then
        Ok
            (
                @Array2D {
                    data: List.join lists,
                    shape: { dimX: List.len lists, dimY: List.len first },
                }
            )
    else
        Err InconsistentRowLengths

expect
    fromExactLists [[1, 2], [3, 4], [5, 6]]
    == Ok (@Array2D { data: [1, 2, 3, 4, 5, 6], shape: { dimX: 3, dimY: 2 } })

expect fromExactLists [[1, 2, 3], [3, 4], [5, 6]] == Err InconsistentRowLengths

expect fromExactLists [[1, 2], [3, 4], [5, 6, 7]] == Err InconsistentRowLengths

shape : Array2D * -> Shape
shape = \@Array2D array -> array.shape

size : Array2D * -> Nat
size = \@Array2D array -> shapeSize array.shape

isRowStart : Index -> Bool
isRowStart = \{ y } -> y == 0

isRowEnd : Array2D *, Index -> Bool
isRowEnd = \@Array2D { shape: { dimY } }, { y } -> y >= (dimY - 1)

isColStart : Index -> Bool
isColStart = \{ x } -> x == 0

isColEnd : Array2D *, Index -> Bool
isColEnd = \@Array2D { shape: { dimX } }, { x } -> x >= (dimX - 1)

lastRow : Array2D * -> Nat
lastRow = \@Array2D { shape: { dimX } } -> dimX - 1

lastCol : Array2D * -> Nat
lastCol = \@Array2D { shape: { dimY } } -> dimY - 1

shapeSize : Shape -> Nat
shapeSize = \{ dimX, dimY } -> dimX * dimY

set : Array2D a, Index, a -> Array2D a
set = \@Array2D array, index, elem ->
    if index.x >= array.shape.dimX || index.y >= array.shape.dimY then
        @Array2D array
    else
        @Array2D { array & data: List.set array.data (listIndexOf array.shape index) elem }

get : Array2D a, Index -> Result a [OutOfBounds]
get = \@Array2D array, index ->
    if index.x >= array.shape.dimX || index.y >= array.shape.dimY then
        Err OutOfBounds
    else
        List.get array.data (listIndexOf array.shape index)

update : Array2D a, Index, (a -> a) -> Array2D a
update = \array, index, updateFn ->
    array
    |> get index
    |> Result.map \elem -> set array index (updateFn elem)
    |> Result.withDefault array

map : Array2D a, (a -> b) -> Array2D b
map = \@Array2D array, fn -> @Array2D { data: List.map array.data fn, shape: array.shape }

mapWithIndex : Array2D a, (a, Index -> b) -> Array2D b
mapWithIndex = \@Array2D array, fn -> @Array2D {
        data: List.mapWithIndex array.data \elem, listIndex ->
            fn elem (arrayIndexOf array.shape listIndex),
        shape: array.shape,
    }

WalkOptions : {
    direction ? [Forward, Backwards, ForwardFrom Index, BackwardsFrom Index],
    orientation ? [Rows, Cols],
    # start ? Index,
    # bounds ? [All, Bounded {start: Index, shape: Shape} ],
}

walk : Array2D a, state, WalkOptions, (state, a, Index -> state) -> state
walk = \array, startState, options, fn ->
    { direction ? Forward, orientation ? Rows } = options

    if direction == Forward && orientation == Rows then
        (@Array2D { data, shape: arrayShape }) = array
        List.walkWithIndex data startState \state, elem, listIndex ->
            fn state elem (arrayIndexOf arrayShape listIndex)
    else
        walkUntil array startState options \state, elem, index ->
            Continue (fn state elem index)

walkUntil : Array2D a, state, WalkOptions, (state, a, Index -> [Continue state, Break state]) -> state
walkUntil = \array, startState, options, fn ->
    (@Array2D { shape: arrayShape }) = array
    { direction ? Forward, orientation ? Rows } = options

    firstIndex = { x: 0, y: 0 }
    lastIndex = lastArrayIndex arrayShape

    when (direction, orientation) is
        (Forward, Rows) -> walkRowsUntil array firstIndex startState fn
        (ForwardFrom startIndex, Rows) -> walkRowsUntil array startIndex startState fn
        (Backwards, Rows) -> walkRowsBackwardsUntil array lastIndex startState fn
        (BackwardsFrom startIndex, Rows) -> walkRowsBackwardsUntil array startIndex startState fn
        (Forward, Cols) -> walkColsUntil array firstIndex startState fn
        (ForwardFrom startIndex, Cols) -> walkColsUntil array startIndex startState fn
        (Backwards, Cols) -> walkColsBackwardsUntil array lastIndex startState fn
        (BackwardsFrom startIndex, Cols) -> walkColsBackwardsUntil array startIndex startState fn

walkRowsUntil : Array2D a, Index, state, (state, a, Index -> [Continue state, Break state]) -> state
walkRowsUntil = \array, startIndex, state, fn ->
    iterate array startIndex state incY fn

walkRowsBackwardsUntil : Array2D a, Index, state, (state, a, Index -> [Continue state, Break state]) -> state
walkRowsBackwardsUntil = \array, startIndex, state, fn ->
    iterate array startIndex state decY fn

walkColsUntil : Array2D a, Index, state, (state, a, Index -> [Continue state, Break state]) -> state
walkColsUntil = \array, startIndex, state, fn ->
    iterate array startIndex state incX fn

walkColsBackwardsUntil : Array2D a, Index, state, (state, a, Index -> [Continue state, Break state]) -> state
walkColsBackwardsUntil = \array, startIndex, state, fn ->
    iterate array startIndex state decX fn

iterate : Array2D a, Index, state, (Array2D a, Index -> Result Index [OutOfBounds]), (state, a, Index -> [Continue state, Break state]) -> state
iterate = \array, index, state, nextIndexFn, nextStateFn ->
    elem =
        when get array index is
            Ok e -> e
            Err OutOfBounds -> crash "Unexpected error iterating over Array2D"

    when (nextIndexFn array index, nextStateFn state elem index) is
        (Ok nextIndex, Continue nextState) -> iterate array nextIndex nextState nextIndexFn nextStateFn
        (_, Continue nextState) -> nextState
        (_, Break nextState) -> nextState

incY : Array2D *, Index -> Result Index [OutOfBounds]
incY = \@Array2D array, { x, y } ->
    if y >= array.shape.dimY - 1 then
        if x >= array.shape.dimX - 1 then
            Err OutOfBounds
        else
            Ok { x: x + 1, y: 0 }
    else
        Ok { x: x, y: y + 1 }

decY : Array2D *, Index -> Result Index [OutOfBounds]
decY = \@Array2D array, { x, y } ->
    if y == 0 then
        if x == 0 then
            Err OutOfBounds
        else
            Ok { x: x - 1, y: array.shape.dimY - 1 }
    else
        Ok { x: x, y: y - 1 }

incX : Array2D *, Index -> Result Index [OutOfBounds]
incX = \@Array2D array, { x, y } ->
    if x >= array.shape.dimX - 1 then
        if y >= array.shape.dimY - 1 then
            Err OutOfBounds
        else
            Ok { x: 0, y: y + 1 }
    else
        Ok { x: x + 1, y: y }

decX : Array2D *, Index -> Result Index [OutOfBounds]
decX = \@Array2D array, { x, y } ->
    if x == 0 then
        if y == 0 then
            Err OutOfBounds
        else
            Ok { x: array.shape.dimX - 1, y: y - 1 }
    else
        Ok { x: x - 1, y: y }

toList : Array2D a -> List a
toList = \@Array2D { data } -> data

expect toList (repeat Empty { dimX: 2, dimY: 2 }) == [Empty, Empty, Empty, Empty]

toLists : Array2D a -> List (List a)
toLists = \array ->
    (@Array2D { data, shape: { dimX, dimY } }) = array
    xIndicies = List.range { start: At 0, end: Before dimX }
    startState = List.withCapacity dimX

    List.walk xIndicies startState \state, xIndex ->
        startIndex = listIndexOf { dimX, dimY } { x: xIndex, y: 0 }
        row = List.sublist data { start: startIndex, len: dimY }
        List.append state row

expect toLists (repeat 0 { dimX: 2, dimY: 2 }) == [[0, 0], [0, 0]]

expect
    toLists (fromList [1, 2, 3, 4, 5] (Fill 0 { dimX: 2, dimY: 3 }))
    == [[1, 2, 3], [4, 5, 0]]

reshape : Array2D a, a, Shape -> Array2D a
reshape = \@Array2D { data }, defaultValue, newShape ->
    @Array2D { data: resize data defaultValue (shapeSize newShape), shape: newShape }

expect repeat 1 { dimX: 3, dimY: 3 } |> reshape 9 { dimX: 1, dimY: 1 } |> toLists == [[1]]

expect
    repeat 1 { dimX: 3, dimY: 3 }
    |> reshape 9 { dimX: 4, dimY: 3 }
    |> toLists
    == [[1, 1, 1], [1, 1, 1], [1, 1, 1], [9, 9, 9]]

resize : List a, a, Nat -> List a
resize = \list, defaultValue, newLen ->
    oldLen = List.len list
    if newLen < oldLen then
        List.takeFirst list newLen
    else if newLen > oldLen then
        List.concat list (List.repeat defaultValue (newLen - oldLen))
    else
        list

transpose : Array2D a -> Array2D a
transpose = \@Array2D array ->
    startAcc = @Array2D { array & shape: { dimX: array.shape.dimY, dimY: array.shape.dimX } }
    List.walkWithIndex array.data startAcc \acc, elem, listIndex ->
        { x, y } = arrayIndexOf array.shape listIndex
        set acc { x: y, y: x } elem

expect
    [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]
    |> fromExactLists
    |> Result.map transpose
    |> Result.map toLists
    == Ok [[1, 5, 9], [2, 6, 10], [3, 7, 11], [4, 8, 12]]

rotateClockwise : Array2D a -> Array2D a

rotateCounterClockwise : Array2D a -> Array2D a

countIf : Array2D a, (a -> Bool) -> Nat
countIf = \@Array2D array, fn -> List.countIf array.data fn

findFirstIndex : Array2D a, (a -> Bool) -> Result Index [NotFound]
findFirstIndex = \@Array2D array, fn ->
    array.data
    |> List.findFirstIndex fn
    |> Result.map \listIndex -> arrayIndexOf array.shape listIndex

findLastIndex : Array2D a, (a -> Bool) -> Result Index [NotFound]
findLastIndex = \@Array2D array, fn ->
    array.data
    |> List.findLastIndex fn
    |> Result.map \listIndex -> arrayIndexOf array.shape listIndex

listIndexOf : Shape, Index -> Nat
listIndexOf = \{ dimY }, { x, y } -> (x * dimY) + y

arrayIndexOf : Shape, Nat -> Index
arrayIndexOf = \{ dimY }, index ->
    x = index // dimY
    y = index % dimY
    { x, y }

lastArrayIndex : Shape -> Index
lastArrayIndex = \{ dimX, dimY } -> { x: dimX - 1, y: dimY - 1 }

isEq : Array2D a, Array2D a -> Bool where a implements Eq
isEq = \@Array2D a, @Array2D b -> a == b
