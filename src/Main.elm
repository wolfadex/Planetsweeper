module Main exposing (main)


import Browser exposing (Document)
-- import Browser.Events exposing (onKeyDown, onKeyPress)
import Browser.Events exposing (onAnimationFrameDelta)
import Debug exposing (log)
-- import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Point3d
import Vector3d exposing (Vector3d)
-- import Json.Decode as Decode
import WebGL exposing (Mesh, Shader, Entity)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


---- TYPES ----


type alias Model =
    { t : Float
    , triangles : List (Vector3d, Vector3d, Vector3d)
    }


type Msg
    = NoOp
    | Tick Float
    | Subdivide


---- INIT ----


init : () -> (Model, Cmd Msg)
init flags =
  ( { t = 0
    , triangles =
          List.map
              (\( a, b, c ) ->
                  ( Vector3d.normalize <| Vector3d.fromComponents a
                  , Vector3d.normalize <| Vector3d.fromComponents b
                  , Vector3d.normalize <| Vector3d.fromComponents c
                  )
              )
              initialVerticies
    }
  , Cmd.none
  )


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        ]


-- decodeKeyDown : Decode.Decoder Msg
-- decodeKeyDown =
--     Decode.map2 keyDownToMsg
--         (Decode.field "key" Decode.string)
--         (Decode.field "repeat" Decode.bool)
--
--
-- keyDownToMsg : String -> Bool -> Msg
-- keyDownToMsg key repeat =
--     if repeat then
--         NoOp
--     else
--         case key of
--             "ArrowRight" ->
--                 Move MRight
--             "d" ->
--                 Move MRight
--             "ArrowLeft" ->
--                 Move MLeft
--             "a" ->
--                 Move MLeft
--             "ArrowUp" ->
--                 Move MUp
--             "w" ->
--                 Move MUp
--             "ArrowDown" ->
--                 Move MDown
--             "s" ->
--                 Move MDown
--             _ ->
--                 NoOp



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Tick delta ->
            ( { model | t = model.t + delta }
            , Cmd.none
            )
        Subdivide ->
            ( { model | triangles = List.concat <| List.map subdivide model.triangles }
            , Cmd.none
            )


subdivide : (Vector3d, Vector3d, Vector3d) -> List (Vector3d, Vector3d, Vector3d)
subdivide (p0, p1, p2) =
    let
        mid01 = Vector3d.normalize <| midVector3d p0 p1
        mid12 = Vector3d.normalize <| midVector3d p1 p2
        mid20 = Vector3d.normalize <| midVector3d p2 p0
    in
      [ ( p0, mid01, mid20 )
      , ( mid01, p1, mid12 )
      , ( mid01, mid12, mid20 )
      , ( mid20, mid12, p2 )
      ]


midVector3d : Vector3d -> Vector3d -> Vector3d
midVector3d v1 v2 =
    Vector3d.fromComponents
        <| Point3d.coordinates
        <| Point3d.midpoint
              (Point3d.fromCoordinates <| Vector3d.components v1)
              (Point3d.fromCoordinates <| Vector3d.components v2)





---- VIEW ----


size : Int
size =
    500


scale : Float
scale =
    1.0


factorA : Float
factorA =
    1 / 2


factorB : Float
factorB =
    1 / (2 * phi)


phi : Float
phi =
    (1 + (sqrt 5)) / 2


initialVerticies : List ((Float, Float, Float), (Float, Float, Float), (Float, Float, Float))
initialVerticies =
    [ ((0, factorB, -factorA), (factorB, factorA, 0), (-factorB, factorA, 0))
    , ((0, factorB, factorA), (-factorB, factorA, 0), (factorB, factorA, 0))
    , ((0, factorB, factorA), (0, -factorB, factorA), (-factorA, 0, factorB))
    , ((0, factorB, factorA), (factorA, 0, factorB), (0, -factorB, factorA))
    , ((0, factorB, -factorA), (0, -factorB, -factorA), (factorA, 0, -factorB))
    , ((0, factorB, -factorA), (-factorA, 0, -factorB), (0, -factorB, -factorA))
    , ((0, -factorB, factorA), (factorB, -factorA, 0), (-factorB, -factorA, 0))
    , ((0, -factorB, -factorA), (-factorB, -factorA, 0), (factorB, -factorA, 0))
    , ((-factorB, factorA, 0), (-factorA, 0, factorB), (-factorA, 0, -factorB))
    , ((-factorB, -factorA, 0), (-factorA, 0, -factorB), (-factorA, 0, factorB))
    , ((factorB, factorA, 0), (factorA, 0, -factorB), (factorA, 0, factorB))
    , ((factorB, -factorA, 0), (factorA, 0, factorB), (factorA, 0, -factorB))
    , ((0, factorB, factorA), (-factorA, 0, factorB), (-factorB, factorA, 0))
    , ((0, factorB, factorA), (factorB, factorA, 0), (factorA, 0, factorB))
    , ((0, factorB, -factorA), (-factorB, factorA, 0), (-factorA, 0, -factorB))
    , ((0, factorB, -factorA), (factorA, 0, -factorB), (factorB, factorA, 0))
    , ((0, -factorB, -factorA), (-factorA, 0, -factorB), (-factorB, -factorA, 0))
    , ((0, -factorB, -factorA), (factorB, -factorA, 0), (factorA, 0, -factorB))
    , ((0, -factorB, factorA), (-factorB, -factorA, 0), (-factorA, 0, factorB))
    , ((0, -factorB, factorA), (factorA, 0, factorB), (factorB, -factorA, 0))
    ]


view : Model -> Document Msg
view { t, triangles } =
  { title = "Planetsweeper"
    , body =
          [ Html.button
                [ Events.onClick Subdivide ]
                [ Html.text "Subdivide" ]
          , WebGL.toHtml
                [ Attrs.width size
                , Attrs.height size
                , Attrs.style "display" "block"
                ]
                <| List.map (isoTri t) triangles
                -- [ WebGL.entity
                --       vertexShader
                --       fragmentShader
                --       mesh
                --       { perspective = perspective (t / 1000) }
                -- ]
          ]
    }

isoTri : Float -> (Vector3d, Vector3d, Vector3d) -> Entity
isoTri t tri =
    WebGL.entity
        vertexShader
        fragmentShader
        (isoMesh tri)
        { perspective = perspective (t / 1000) }


isoMesh : (Vector3d, Vector3d, Vector3d) -> Mesh Vertex
isoMesh (p0, p1, p2) =
    let
        (x0, y0, z0) = Vector3d.components p0
        (x1, y1, z1) = Vector3d.components p1
        (x2, y2, z2) = Vector3d.components p2
    in
        WebGL.triangles
            [ ( Vertex (vec3 x0 y0 z0) (vec3 1 0 0)
              , Vertex (vec3 x1 y1 z1) (vec3 0 1 0)
              , Vertex (vec3 x2 y2 z2) (vec3 0 0 1)
              )
            ]


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1  0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
