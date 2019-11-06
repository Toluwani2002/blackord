import Grid.Bordered as Grid
import Color exposing (rgb255)
import Grid.Position as Position exposing (Position, Coord)
import Grid.Direction exposing (Direction(..))
import PixelEngine.Options as Options exposing (Options)
import Time
import PixelEngine
    exposing
        ( Area
        , Input(..)
        , PixelEngine
        , game
        , colorBackground
        )
import PixelEngine.Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)
import PixelEngine.Image as Image

type alias Player =
  { position : Position
  , coinCount : Int
  }

type Entity =
    Platform
  | Coin
  | Marker

type alias Platform = Grid.Grid Entity

type alias GameState =
  { player : Player
  , platform : Platform
  , finishLine : Position
  }

type alias Model = Maybe GameState

type Msg = MovePlayer Direction | Restart

main : PixelEngine () Model Msg
main = game
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  , controls = controls
  , width = width
  }

boardSize : Int
boardSize = 30

tileSize : Int
tileSize = 30

width : Float
width = toFloat <| boardSize * tileSize

initPlayer : Player
initPlayer = { position = (0, 0) , coinCount = 0}

initPlatform : Platform
initPlatform =
  Grid.fill
    (\(x,y) ->
      if y == 15 && x > 10 && x < 20 then
        Just Platform
      else if y == 14 && x > 10 && x < 20 then
        Just Coin
      else if y == 19 && x >= 0 && x < 9 then
        Just Platform
      else if y == 23 && x > 21 && x < 29 then
        Just Platform
      else
        Nothing
    )
    { rows    = boardSize
    , columns = boardSize
    }


initGameState : GameState
initGameState =
  { player = initPlayer
  , platform = initPlatform
  , finishLine = (29,29)
  }

initModel : Model
initModel = Just initGameState

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none)

controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| MovePlayer Up

        InputDown ->
            Just <| MovePlayer Down

        InputLeft ->
            Just <| MovePlayer Left

        InputRight ->
            Just <| MovePlayer Right

        InputA ->
            Just <| MovePlayer Up

        _ ->
            Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg x =
  case x of
    Nothing ->
        case msg of
          Restart -> (initModel, Cmd.none)
          _       -> (x, Cmd.none)
    Just model ->
      case msg of
        MovePlayer direction ->
          let
            (g, p) =
              checkAndMove model.platform direction model.player
              |> coinPickup model.platform
            newGameState =
              { model
                | player = p
                , platform = g
              }
          in
            ( checkToWin p model.finishLine newGameState
            , Cmd.none
            )
        _ -> (x, Cmd.none)

checkAndMove : Grid.Grid Entity -> Direction -> Player -> Player
checkAndMove grid direction player =
  player.position
    |> Position.add (direction |> Position.fromDirection)
    |> (\newPos ->
        case (Grid.get newPos grid) of
          Err _       -> player
          Ok (entity) ->
            case entity of
              (Just Platform) -> player
              _               -> { player | position = newPos }
      )

coinPickup : Grid.Grid Entity -> Player -> (Grid.Grid Entity, Player)
coinPickup grid player =
  Grid.get (player.position) grid
    |> Result.map (\entity ->
        case entity of
          (Just Coin) ->
            ( Grid.remove player.position grid |> Result.withDefault grid
            , { player | coinCount = player.coinCount + 1}
            )
          _ -> (grid, player)
      )
    |> Result.withDefault (grid, player)

checkToWin : Player -> Position -> GameState -> Model
checkToWin player finishLinePosition gstate =
  if player.position == finishLinePosition && player.coinCount == 9 then
    Nothing
  else
    Just gstate

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
   -- Time.every 20 (MovePlayer Down |> always)

options : Options Msg
options =
    Options.default
        |> Options.withMovementSpeed 0.8

view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "BlackOrd"
    , options = Just options
    , body = areas model
    }

areas : Model -> List (Area Msg)
areas model =
  case model of
    Nothing -> [gameWonArea]
    Just gstate ->
      [ scoreArea gstate
      , gameArea gstate
      ]

gameWonArea : Area Msg
gameWonArea =
  PixelEngine.imageArea
    { height = 30
    , background = colorBackground (rgb255 255 255 255)
    }
    [ ((300, 200), Image.clickable Restart <| Image.fromText "You Win!!! (click to restart)" font)
    ]

scoreArea : GameState -> Area Msg
scoreArea model =
  PixelEngine.imageArea
    { height = 30
    , background = colorBackground (rgb255 255 255 255)
    }
    [ ((0,0), Image.fromText ((coinCountStr model)) font)
    ]

coinCountStr : GameState -> String
coinCountStr = .player >> .coinCount >> String.fromInt >> String.append "Coins - "

-- hasWonStr : GameState -> String
-- hasWonStr model =
--   case model.finished of
--     True -> "Won"
--     False -> "Not Won"

font : Tile.Tileset
font = Tile.tileset
  { source       = "img/font.png"
  , spriteWidth  = 16
  , spriteHeight = 16
  }

gameArea : GameState -> Area Msg
gameArea { platform, player, finishLine } = -- TODO: add ", finishLine" to this inputs of the function
  PixelEngine.tiledArea
      { rows = boardSize
      , tileset =
          { source       = "img/man.png"
          , spriteWidth  = tileSize
          , spriteHeight = tileSize
          }
      , background = colorBackground (rgb255 255 255 255)
      }
      ( platform
          |> Grid.toList
          |> List.map
              (\(pos, entity) ->
                ( pos
                , case entity of
                    Platform -> platformTile
                    Coin     -> coinTile
                    _        -> emptyTile
                )
              )
          |> (::) (player.position, playerTile)
          |> (::) (finishLine, finishLineTile)-- TODO: add a duplicate of this line below this line and change the pair to have the finishLine (which is a position) and the finishLineTile
      )

playerTile : Tile Msg
playerTile =
  Tile.fromPosition (0,1)
    |> Tile.movable "player"
    |> Tile.jumping

platformTile : Tile Msg
platformTile = Tile.fromPosition (1,0)

emptyTile : Tile Msg
emptyTile = Tile.fromPosition (0, 0)

coinTile : Tile Msg
coinTile = Tile.fromPosition (0,0)

finishLineTile : Tile Msg
finishLineTile = Tile.fromPosition (1,1)
