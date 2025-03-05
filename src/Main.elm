module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit, onInput, on, keyCode)
import Json.Decode as Json

type alias Todo =
    { text : String
    , completed : Bool
    , editing : Bool
    }

type alias Model =
    { todos : List Todo
    , inputText : String
    }

type Message
    = AddTodo
    | RemoveTodo Int
    | ToggleTodo Int
    | ChangeInput String
    | ChangeTodoText Int String
    | ToggleEditTodo Int
    | EditTodoCompleted Int
    | NoOp
    | KeyDown Int Int

update : Message -> Model -> Model
update message model =
    case message of
        AddTodo ->
            if String.isEmpty (String.trim model.inputText) then
                model
            else
                { model
                    | todos = addToList { text = model.inputText, completed = False, editing = False } model.todos
                    , inputText = ""
                }
        RemoveTodo index ->
            { model | todos = removeFromList index model.todos }
        ToggleTodo index ->
            { model | todos = toggleAtIndex index model.todos }
        ChangeInput input ->
            { model | inputText = input }
        ChangeTodoText index newText ->
            { model | todos = updateTodoText index newText model.todos }
        ToggleEditTodo index ->
            { model | todos = toggleEditTodo index model.todos }
        EditTodoCompleted index ->
            { model | todos = completeEditTodo index model.todos }
        NoOp ->
              model
        KeyDown index key ->
              if key == 13 then
                { model | todos = completeEditTodo index model.todos}
              else
                model

updateTodoText : Int -> String -> List Todo -> List Todo
updateTodoText index newText todos =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == index then { todo | text = newText } else todo
        )
        todos

toggleEditTodo : Int -> List Todo -> List Todo
toggleEditTodo index todos =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == index then { todo | editing = not todo.editing } else todo
        )
        todos

completeEditTodo : Int -> List Todo -> List Todo
completeEditTodo index todos =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == index then { todo | editing = False } else todo
        )
        todos

addToList : Todo -> List Todo -> List Todo
addToList todo todos =
    todos ++ [todo]

removeFromList : Int -> List Todo -> List Todo
removeFromList index list =
    List.take index list ++ List.drop (index + 1) list

toggleAtIndex : Int -> List Todo -> List Todo
toggleAtIndex indexToToggle list =
    List.indexedMap
        (\currentIndex todo ->
            if currentIndex == indexToToggle then
                { todo | completed = not todo.completed }
            else
                todo
        )
        list

view : Model -> Html Message
view model =
    div [ class "app-container" ]
        [ Html.form [ onSubmit AddTodo ]
            [ h1 [ class "app-title" ] [ text "The Todo Site!" ]
            , div [ class "input-container" ]
                [ input [ class "input-field", value model.inputText, onInput ChangeInput, placeholder "What do you want to do?" ] []
                , button [ class "submit-button", type_ "submit" ] [ text "Add" ]
                ]
            , if List.isEmpty model.todos then
                p [ class "empty-message" ] [ text "The list is clean" ]
              else
                div [ class "todos-container" ]
                    [ h2 [ class "todos-heading" ] [ text "Tasks" ]
                    , ul [ class "todos-list" ]
                        (List.indexedMap (\index todo -> if not todo.completed then viewTodo index todo else text "") model.todos)
                    , h2 [ class "todos-heading" ] [ text "Tasks done" ]
                    , ul [ class "todos-list", style "text-decoration" "line-through" , style "text-decoration-thickness" "0.5px"]
                        (List.indexedMap (\index todo -> if todo.completed then viewTodo index todo else text "") model.todos)
                    ]
            ]
        ]
viewTodo : Int -> Todo -> Html Message
viewTodo index todo =
    li [ class "todo-item" ]
        [ div [ class "todo-content" ]
            [ input
                [ type_ "checkbox"
                , checked todo.completed
                , onInput (\_ -> ToggleTodo index)
                , class "todo-checkbox"
                ] []
            , if not todo.completed then
                if todo.editing then
                    div [ class "editing-container" ]
                        [ input
                            [ type_ "text"
                            , value todo.text
                            , onInput (\newText ->  ChangeTodoText index newText )
                            , onKeyDown (KeyDown index)
                            , class "edit-input"
                            ] 
                            []
                        , img [ src "images/done.png", alt "Done", onClick (EditTodoCompleted index), class "done-button" ] []
                    ]
                else
                    span
                        [ class "todo-text"
                        ]
                        [ text todo.text
                        , img [ src "images/edit.png", alt "Edit", onClick (ToggleEditTodo index), class "edit-button" ] []
                    ]
              else
                span [ class "todo-text completed" ] [ text todo.text ]
            ]
        , img
            [ src "images/trash.png"
            , alt "Delete"
            , class "delete-icon"
            , onClick (RemoveTodo index)
            ] []
        ]

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

init =
    { todos = []
    , inputText = ""
    }

main =
    Browser.sandbox { init = init, update = update, view = view }
