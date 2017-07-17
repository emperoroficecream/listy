import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

main = 
  Html.beginnerProgram
    { model = model
    , view = view 
    , update = update
    }

-- Model

type alias Log = 
  { id : Int
  , content : String
  , complete : Bool
  }

type alias Model = 
  { logs : List Log
  , field : String
  , uid : Int
  }

newLog : String -> Int -> Log
newLog content id =
    { id = id
    , content = content
    , complete = False
    }

model : Model
model = { logs = [ newLog "" 1 ], field = "", uid = 0 }



-- Update

type Msg
    = UpdateLog Int String
    | DeleteLog Int
    | UpdateField String
    | Add
  
update : Msg -> Model -> Model
update msg model = 
  case msg of 
    Add ->
      { model 
        | uid = model.uid + 1
        , field = ""
        , logs = 
            if String.isEmpty model.field then
              model.logs
            else
              model.logs ++ [ newLog model.field model.uid ]
      }
      
    UpdateLog id content ->
      let
          updateLog l =
            if l.id == id then
              { l | content = content }
            else
              l
      in
        { model | logs = List.map updateLog model.logs }

    UpdateField str ->
      { model | field = str }

    DeleteLog id ->
      { model | logs = List.filter (\log -> log.id /= id) model.logs }

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "todomvc-wrapper" ]
    [ section
      [ class "todoapp" ]
      [ viewInput model.field
      , viewLogs model.logs
      ]
    ]

viewInput : String -> Html Msg
viewInput log =
  header
    [ class "header" ]
    [ h1 [] [ text "logs" ]
    , input
      [ class "new-log"
      , placeholder "What's on your mind? "
      , autofocus True
      , value log
      , name "newLog"
      , onInput UpdateField
      , onEnter Add
      ]
      []
    ]

viewLogs : List Log -> Html Msg 
viewLogs logs =
  section
    [ class "main" ]
    [ ul [] <| List.map viewLog logs ]

viewLog : Log -> Html Msg 
viewLog log =
  li
    []
    [ p [] [ text log.content ]
    ]


onEnter : Msg -> Attribute Msg
onEnter msg = 
  let
      isEnter code =
        if code == 13 then
          Json.succeed msg 
        else
          Json.fail "not ENTER"     
  in
    on "keydown" (Json.andThen isEnter keyCode)



