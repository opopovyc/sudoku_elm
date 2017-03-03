module Main exposing (..)

import Html.Attributes exposing(..)
import Html exposing(..)
import Html.Events exposing (onInput)
import String


--MAIN

main : Program Never Model Msg
main = Html.beginnerProgram
    { model = init
    , update = update
    , view = view
    }
{-main = Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = (\s -> Sub.none)
    }
-}


--MODEL

type alias Model = 
    { grid : Matrix
    , textMsg : String 
    }

-- Cell of a sudoku grid can be empty, filled with a default value or own value (that can be invalid)
type Cell
    = Empty
    | Fixed Int
    | Value Int
    | Invalid Int

-- Row is a list of cells
type alias Row =
    List Cell


type alias Matrix =
    List Row


type alias IndexedCell =
    ( Int, Cell )


type alias IndexedRow =
    List IndexedCell


type alias InputValue =
    Int


type alias CellOrd =
    Int


type alias CellId =
    Int


--INIT

--ideally use map and similar for init (with input values as a couple value-position)
init : Model
init = 
    { grid = initMatrix
    , textMsg = "Let's play sudoku!"
    }

initMatrix : Matrix
initMatrix =
    [ [ Empty, Empty, Empty,        Fixed 1, Empty, Fixed 5,    Empty, Fixed 6, Fixed 8 ]
    , [ Empty, Empty, Empty,        Empty, Empty, Empty,        Fixed 7, Empty, Fixed 1 ]
    , [ Fixed 9, Empty, Fixed 1,    Empty, Empty, Empty,        Empty, Fixed 3, Empty ]

    , [ Empty, Empty, Fixed 7,      Empty, Fixed 2, Fixed 6,    Empty, Empty, Empty ]
    , [ Fixed 5, Empty, Value 4,    Empty, Empty, Empty,        Empty, Empty, Fixed 3 ]
    , [ Empty, Empty, Empty,        Fixed 8, Fixed 7, Empty,    Fixed 4, Empty, Empty ]

    , [ Empty, Fixed 3, Empty,      Empty, Empty, Empty,        Fixed 8, Empty, Fixed 5 ]
    , [ Fixed 1, Empty, Fixed 5,    Empty, Empty, Empty,        Empty, Empty, Empty ]
    , [ Fixed 7, Fixed 9, Empty,    Fixed 4, Empty, Fixed 1,    Empty, Empty, Empty ]
    ]


--UPDATE 

type Msg
    = Input CellId String


--write my own
update : Msg -> Model -> Model
update msg model =
    case msg of
        Input id inputValue ->
            let
                parsedInput =
                    Result.withDefault 1 (String.toInt (String.right 1 inputValue))

                _ =
                    Debug.log "Update" ( inputValue, parsedInput, id )
            in
                { model | grid = applyInput model.grid id parsedInput }


applyInput : Matrix -> CellId -> InputValue -> Matrix
applyInput grid id val =
    List.concat grid |> List.indexedMap (mapCell val id) |> group 9


mapCell : InputValue -> CellId -> CellOrd -> Cell -> Cell
mapCell val id cellOrd cell =
    if cellOrd == id then
        if val > 0 then
            Value val
        else
            Empty
    else
        cell


-- VIEW


{-| We are flattening the list and converting it into a list of numbered cells [ (0, Empty), (1, Value x)...]
    in order to supply the id needed to map the right input back to the update function.
-}
view : Model -> Html Msg
view model =
    let
        indexedMatrix =
            -- flatten so we can number the cells
            List.concat model.grid
                |> -- number the cells from 0 .. 81
                   List.indexedMap (,)
                |> -- re-assemble the original matrix, now numbered
                   group 9
                |> -- group by three rows for visual representation of groups
                   group 3
                |> -- apply tbody to each group
                   List.map rowgrp
    in
        div [] 
            [ button [id "message"] [text model.textMsg]
            , table [ class "grid" ]
                (colgroups ++ indexedMatrix)
            ]


{-| Wrap each three-row group with a tbody tag
-}
rowgrp : List IndexedRow -> Html Msg
rowgrp rows =
    tbody [] (List.map renderRow rows)


renderRow : IndexedRow -> Html Msg
renderRow row =
    tr [] (List.map renderCell row)


renderCell : IndexedCell -> Html Msg
renderCell ( id, cell ) =
    td [] [ input (cellattribs cell id) [] ]


cellattribs cell id =
    case cell of
        Empty ->
            [ value (""), onInput (Input id) ]

        Fixed v ->
            [ value (toString v), disabled True, class "fixed" ]

        Value v ->
            [ value (toString v), onInput (Input id) ]

        Invalid v ->
            [ value (toString v), class "invalid", onInput (Input id) ]


{-| Generate the column groups we need for formatting
-}
colgroups =
    let
        m3 a =
            List.map (\_ -> a) <| List.range 1 3
    in
        m3 (colgroup [] (m3 (col [] [])))

{-| Group list elements into sub-lists of length n
-}
group : Int -> List a -> List (List a)
group n xs =
    case xs of
        [] ->
            []

        _ ->
            List.take n xs :: group n (List.drop n xs)


{-
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (toInt, right)
import List exposing (map, indexedMap, concat, take, drop)


-- HELPERS


{-| Group list elements into sub-lists of length n
-}
group : Int -> List a -> List (List a)
group n xs =
    case xs of
        [] ->
            []

        _ ->
            List.take n xs :: group n (List.drop n xs)



-- TYPES


type Cell
    = Empty
    | Fixed Int
    | Value Int
    | Invalid Int


type alias Row =
    List Cell


type alias Matrix =
    List Row


type alias IndexedCell =
    ( Int, Cell )


type alias IndexedRow =
    List IndexedCell


type alias InputValue =
    Int


type alias CellOrd =
    Int


type alias CellId =
    Int



-- APP


main : Program Never Model Msg
main = Html.beginnerProgram
    { model = init
    , update = update
    , view = view
    }

-- MODEL
type alias Model = 
    { grid : Matrix
    , textMsg : String 
    }

type alias IndexedMatrix =
    List (List { rowI : Int, colI : Int, boxI : Int, value : Cell })

init : Model
init = 
    { grid = empty --initMatrix
    , textMsg = "Let's play sudoku!"
    }
-- Matrix is a list of rows.. 
--start with empty matrix -> convert each cell to (row, col, cell)

empty : Matrix
empty = List.repeat 9 (List.repeat 9 Empty)

indexedEmpty : IndexedMatrix
indexedEmpty = 
    List.map (\row -> indexedMap (,) row) empty
        |> indexedMap (,) 
        |> List.map (\(r, row) -> List.map (\(c, cell) -> {rowI = r+1, colI = c+1, boxI = findBoxIndex (r+1) (c+1), value = cell}) row ) 

findBoxIndex : Int -> Int -> Int 
findBoxIndex rowIndex colIndex =
    (rowIndex * colIndex) // 3

        
initializeByIndex : IndexedMatrix -> List (Int, Int, Cell) -> IndexedMatrix
initializeByIndex matrix ls =
    -- need to make 
    matrix

initMatrix : Matrix
initMatrix =
    [ [ Empty, Empty, Empty,        Fixed 1, Empty, Fixed 5,    Empty, Fixed 6, Fixed 8 ]
    , [ Empty, Empty, Empty,        Empty, Empty, Empty,        Fixed 7, Empty, Fixed 1 ]
    , [ Fixed 9, Empty, Fixed 1,    Empty, Empty, Empty,        Empty, Fixed 3, Empty ]

    , [ Empty, Empty, Fixed 7,      Empty, Fixed 2, Fixed 6,    Empty, Empty, Empty ]
    , [ Fixed 5, Empty, Value 4,    Empty, Empty, Empty,        Empty, Empty, Fixed 3 ]
    , [ Empty, Empty, Empty,        Fixed 8, Fixed 7, Empty,    Fixed 4, Empty, Empty ]

    , [ Empty, Fixed 3, Empty,      Empty, Empty, Empty,        Fixed 8, Empty, Fixed 5 ]
    , [ Fixed 1, Empty, Fixed 5,    Empty, Empty, Empty,        Empty, Empty, Empty ]
    , [ Fixed 7, Fixed 9, Empty,    Fixed 4, Empty, Fixed 1,    Empty, Empty, Empty ]
    ]



-- UPDATE


type Msg
    = Input CellId String
    


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input id inputValue ->
            let
                parsedInput =
                    Result.withDefault 0 (String.toInt (String.right 1 inputValue))

                _ =
                    Debug.log "Update" ( inputValue, parsedInput, id )
            in
                 { model | grid = applyInput model.grid id parsedInput }
 --check in box, row and column IDEA: need to index cells in model with (row#, col#, box#)


applyInput : Matrix -> CellId -> InputValue -> Matrix
applyInput grid id val =
    List.concat grid |> List.indexedMap (mapCell val id) |> group 9


mapCell : InputValue -> CellId -> CellOrd -> Cell -> Cell
mapCell val id cellOrd cell =
    if cellOrd == id then
        if val > 0 then
            Value val
        else
            Empty
    else
        cell



-- VIEW


{-| We are flattening the list and converting it into a list of numbered cells [ (0, Empty), (1, Value x)...]
    in order to supply the id needed to map the right input back to the update function.
-}
view : Model -> Html Msg
view model =
    let
        indexedMatrix =
            -- flatten so we can number the cells
            List.concat model.grid
                |> -- number the cells from 0 .. 81
                   List.indexedMap (,)
                |> -- re-assemble the original matrix, now numbered
                   group 9
                |> -- group by three rows for visual representation of groups
                   group 3
                |> -- apply tbody to each group
                   List.map rowgrp
    in
        div [] 
            [ div [id "message"] [text model.textMsg]
            , table [ class "grid" ]
                (colgroups ++ indexedMatrix)
            ]


{-| Wrap each three-row group with a tbody tag
-}
rowgrp : List IndexedRow -> Html Msg
rowgrp rows =
    tbody [] (List.map renderRow rows)


renderRow : IndexedRow -> Html Msg
renderRow row =
    tr [] (List.map renderCell row)


renderCell : IndexedCell -> Html Msg
renderCell ( id, cell ) =
    td [] [ input (cellattribs cell id) [] ]


cellattribs cell id =
    case cell of
        Empty ->
            [ value (""), onInput (Input id)]

        Fixed v ->
            [ value (toString v), disabled True, class "fixed" ]

        Value v ->
            [ value (toString v), onInput (Input id) ]

        Invalid v ->
            [ value (toString v), class "invalid", onInput (Input id) ]


{-| Generate the column groups we need for formatting
-}
colgroups =
    let
        m3 a =
            List.map (\_ -> a) <| List.range 1 3
    in
        m3 (colgroup [] (m3 (col [] [])))
-}