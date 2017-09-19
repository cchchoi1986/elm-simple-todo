import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)

main =
  Html.program
    { 
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
    }

--MODEL
type alias Model = 
  {
    task : String,
    tasks : List TodoTask
  }

type alias TodoTask =
  { 
    task : String,
    status : Bool
  }

init : (Model, Cmd Msg)
init =
  (Model "" [{ task = "Do laundry", status = False }], Cmd.none)

--UPDATE
type Msg
  = Input String
  | AddTask
  | ToggleStatus TodoTask
  | Remove TodoTask

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input input ->
      ({ model | task = input }, Cmd.none)
    AddTask ->
      if (model.task == "") then (model, Cmd.none)
      else
      ({ model 
        | tasks = model.tasks ++ [{ task = model.task, status = False }]
        , task = "" }
        , Cmd.none)
    ToggleStatus todo ->
      let
        updateTask t =
          if (t.task == todo.task) then { t | status = not t.status }
          else t
      in
        ({ model | tasks = List.map updateTask model.tasks }, Cmd.none)
    Remove todo ->
        ({ model | tasks = List.filter (.task >> (/=) todo.task) model.tasks }, Cmd.none)

--VIEW
view : Model -> Html Msg
view model =
  (div []
    [
      h1 [ style [("text-align", "center")] ] [ text "Simple TODO" ],
      div [ style [("background", "#982492")
        , ("padding", "20px")] ] 
        [
          div [ style [("width", "100%")] ]
            [
              input [ type_ "text"
                , placeholder "Write a Task"
                , onInput Input
                , value model.task 
                , style [("margin", "auto")
                  , ("height", "30px")
                  , ("width", "calc(100% - 6px)")
                  , ("text-align", "center")]
              ] []
            ]
          , div [ style [("width", "100%")] ] 
            [
              button [ style [("width", "100%")
                , ("height", "35px")
                , ("text-align", "center")
                , ("font-weight", "bolder")]
                , onClick AddTask ] [ text "Add to List" ]
            ]
        ]
      , div [ style [] ] 
        (List.map renderList model.tasks)
    ])

subscriptions : Model -> Sub Msg
subscriptions model =
   Sub.none

renderList : TodoTask -> Html Msg
renderList todo =
  div [ style [("text-align", "center")
    , ("border", "1px solid #ccc")
    , ("height", "40px")]
    ] [ input [ type_ "checkbox"
      , checked todo.status
      , style [("text-align", "center")
      , ("z-index", "1000000")
      , ("width", "100%")
      , ("height", "34px")]
      , onClick (ToggleStatus todo)
      ] []
      , span [ style [("z-index", "-10")
        , ("position", "relative")
        , ("top", "-25px")] ] 
        [ text todo.task ]
      , span [ style [("z-index", "100")
        , ("position", "absolute")
        , ("margin-top", "-25px")
        , ("right", "30px")
        --, ("border", "1px solid #ccc")
        , ("cursor", "hand")
        , ("border-radius", "50px")]
        , onClick (Remove todo)]
        [ text "X" ]
      ]
