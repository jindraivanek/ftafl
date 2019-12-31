module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

//open Fable.Import.Browser
importAll "../sass/main.sass"

open Fable.Core
open Fable.Helpers.React
open Fable.Helpers.React.Props

[<Emit("window.innerWidth")>]
let innerWidth : float = jsNative

[<Emit("window.innerHeight")>]
let innerHeight : float = jsNative

module Core = FTafl.Core.Types
type Model<'a> =
    { CoreModel : Core.Model<'a>
      SelectedPos : (Core.BoardId * Core.Pos) option
      Log : string list }

let initModel =
    { CoreModel = FTafl.SCCG.init
      SelectedPos = None
      Log = List.empty }

type Message<'a> =
    | DoMove of 'a
    | SelectPos of Core.BoardId * Core.Pos

let rec autoMove (model : Core.Model<_>) =
    match FTafl.Core.getPlayer model.ActivePlayer model with
    | { Core.AI = Some ai } ->
        let allActions = FTafl.Core.getActivePlayerActions model |> Seq.map snd
        Some <| DoMove(ai model allActions)
    | { Core.AI = None } -> None

let update message model =
    printfn "%A" message
    let nextCmd m =
        autoMove m.CoreModel |?> (fun x ->
        let c = Cmd.ofMsg x
        m, c)
        |?? (m, Cmd.none)
    match message with
    | DoMove msg ->
        { model with
              CoreModel = FTafl.Core.update model.CoreModel [ msg ]
              Log =
                  sprintf "%A : %A" model.CoreModel.ActivePlayer msg
                  :: model.Log }
        |> nextCmd
    | SelectPos(bId, p) as ev ->
        { model with SelectedPos = Some(bId, p) } |> nextCmd

module Svg =
    type Color =
        | Black
        | White
        | Red
        | LightGreen
        | LightGrey

        member x.Name =
            match x with
            | Black -> "black"
            | White -> "white"
            | LightGreen -> "lightgreen"
            | LightGrey -> "lightgrey"
            | Red -> "red"

    type RectOpts = {
        X : float
        Y : float
        Width : float
        Height : float
        Color : Color
        BackgroundColor : Color
        UpperLeftText : string
        UpperRightText : string
        BottomLeftText : string
        BottomRightText : string
        OnClick : React.MouseEvent -> unit
    } with
        static member Default = {
            X = 0.
            Y = 0.
            Width = 100.
            Height = 100.
            Color = Color.Black
            BackgroundColor = Color.White
            UpperLeftText = ""
            UpperRightText = ""
            BottomLeftText = ""
            BottomRightText = ""
            OnClick = ignore
        }

    let svgRect (opts : RectOpts -> RectOpts) =
        let o = opts RectOpts.Default
        let textGap = 5.0
        let textSize = 10.
        // let o = { o with Width = max o.Width <| float (Seq.length o.UpperLeftText + Seq.length o.UpperRightText) * 8. }
        svg [ OnClick o.OnClick ]
            [ rect
                [ SVGAttr.X o.X
                  SVGAttr.Y o.Y
                  SVGAttr.Width o.Width
                  SVGAttr.Height o.Height
                  SVGAttr.Fill o.BackgroundColor.Name
                  SVGAttr.Stroke o.Color.Name
                  SVGAttr.StrokeWidth 3. ] []
              text
                  [ SVGAttr.X(o.X + textGap)
                    SVGAttr.Y(o.Y + textGap + textSize)
                    SVGAttr.TextAnchor "start" ] [ str o.UpperLeftText ]
              text
                  [ SVGAttr.X(o.X + o.Width - textGap)
                    SVGAttr.Y(o.Y + textGap + textSize)
                    SVGAttr.TextAnchor "end" ] [ str o.UpperRightText ]
              text
                  [ SVGAttr.X(o.X + textGap)
                    SVGAttr.Y(o.Y + o.Height - textGap)
                    SVGAttr.TextAnchor "start" ] [ str o.BottomLeftText ]
              text
                  [ SVGAttr.X(o.X + o.Width - textGap)
                    SVGAttr.Y(o.Y + o.Height - textGap)
                    SVGAttr.TextAnchor "end" ] [ str o.BottomRightText ] ]

    let svgRow height maxWidth rects =
        let padding = 5.
        let innerWidth = innerWidth - padding * 2.
        let n = List.length rects
        let width = min maxWidth <| (innerWidth - ((float (n + 1)) * padding)) / float n
        rects |> List.mapi (fun i r -> svgRect (r >> (fun o -> { o with X = padding + float i * (width + padding); Width = width; Height = height })))
        |> svg [ SVGAttr.Width innerWidth; SVGAttr.Height height ]

let view (model : Model<_>) dispatch =
    let m = model.CoreModel

    //printfn "%A" m
    let unitsMap =
        m.Units
        |> Map.toSeq
        |> Seq.map (fun (uId, u) -> (u.Loc, u.Pos), uId)
        |> Map.ofSeq

    let unitIdToPos =
        unitsMap
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> v, k)
        |> Map.ofSeq

    let actions =
        let selectedUnit =
            model.SelectedPos
            |> Option.bind (fun p -> unitsMap |> Map.tryFind p)
        selectedUnit
        |> Option.map (fun u -> FTafl.Core.getUnitActions u m |> Seq.toList)
        |> Option.defaultValue []
        |> Map.ofSeq

    let actionButtons actions =
        actions
        |> Seq.map
            (fun (_, msg) ->
                (fun (o : Svg.RectOpts) ->
                 { o with
                    OnClick = (fun _ -> dispatch (DoMove msg))
                    UpperRightText = sprintf "%A" msg
                 }))
        |> Seq.toList

    let boardActions bId =
        unitsMap
        |> Map.toSeq
        |> Seq.filter (fun ((bId2, _), _) -> bId = bId2)
        |> Seq.collect
            (fun (_, uId) -> FTafl.Core.getUnitActions uId m |> actionButtons)
        |> Seq.toList

    let boards =
        m.Units
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.groupInto (Map.keys m.Boards) (fun u -> u.Loc)
        |> Seq.sortByDescending fst
        |> Seq.map (fun (bId, units) ->
            let board = FTafl.Core.getBoard bId m
            let (Core.Pos(w, h)) = board.Size

            let unitsText =
                units
                |> Seq.map (fun u -> u.Pos, FTafl.Core.unitTextView m u)
                |> Map.ofSeq
            [ 1..h ]
            |> List.map (fun y ->
                div []
                    ([ 1..w ]
                     |> List.collect (fun x ->
                         let pos = bId, Core.Pos(x, y)

                         let action =
                             unitsMap
                             |> Map.tryFind pos
                             |> Option.bind
                                 (fun uId -> actions |> Map.tryFind (Some uId))

                         let actionColor =
                             action |?> (fun _ -> Svg.Color.Red)

                         let readyColor =
                             unitsMap
                             |> Map.tryFind pos
                             |?>= (fun uId ->
                             if FTafl.Core.getUnitActions uId m
                                |> Seq.isEmpty
                                |> not
                             then Some Svg.Color.LightGreen
                             else None)

                         let color = actionColor |> Option.orElse readyColor |?? Svg.RectOpts.Default.Color

                         let backgroundColor =
                            if model.SelectedPos |> Option.exists ((=) pos) then Svg.Color.LightGrey
                            else Svg.Color.White

                         let restActionButtons =
                             actions
                             |> Map.toSeq
                             |> Seq.filter (function
                                 | None, _ -> true
                                 | _ -> false)
                             |> actionButtons

                         let texts = (unitsText
                                         |> Map.tryFind (Core.Pos(x, y))
                                         |> Option.defaultValue [ ""; "empty"; ""; "" ])

                         ([ (fun (o : Svg.RectOpts) ->
                             { o with
                                OnClick = (fun _ ->
                                 dispatch
                                     (action |?> DoMove |?? SelectPos pos))
                                Color = color
                                BackgroundColor = backgroundColor
                                UpperLeftText = texts.[0]
                                UpperRightText = texts.[1]
                                BottomLeftText = texts.[2]
                                BottomRightText = texts.[3]
                             }) ]
                          @ if board.Name.Contains "Avatar" then
                              boardActions bId @ restActionButtons
                            else [])
                            )
                     |> Svg.svgRow 50. (if board.Name.Contains "Avatar" then 200. else 100.) |> fun x -> [ x ]
                    ))
            |> fun t -> div [] t
            |> fun t ->
                div []
                    (str board.Name :: if board.Name.Contains "Deck"
                                          || board.Name.Contains "Graveyard" then
                                           []
                                       else [ t ]))
        |> Seq.toList
        |> span []

    let log =
        model.Log
        |> List.map (fun x ->
            span []
                [ str x
                  br [] ])
        |> span []

    span []
        [ boards
          hr []
          log
        ]

open Elmish.Debug
open Elmish.HMR
open Elmish.React

// App
Program.mkProgram (fun _ -> initModel, Cmd.none) update view
//|> Program.toNavigable (parseHash pageParser) urlUpdate

#if DEBUG
|> Program.withDebugger
//|> Program.withHMR

#endif



|> Program.withReact "elmish-app"
|> Program.run
