module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

//open Fable.Import.Browser
importAll "../sass/main.sass"

open Fable.Helpers.React
open Fable.Helpers.React.Props

// let menuItem label page currentPage =
//     li
//       [ ]
//       [ a
//           [ classList [ "is-active", page = currentPage ]
//             Href (toHash page) ]
//           [ str label ] ]
// let menu currentPage =
//   aside
//     [ ClassName "menu" ]
//     [ p
//         [ ClassName "menu-label" ]
//         [ str "General" ]
//       ul
//         [ ClassName "menu-list" ]
//         [ menuItem "Home" Home currentPage
//           menuItem "Counter sample" Counter currentPage
//           menuItem "About" Page.About currentPage ] ]
// let root model dispatch =
//   let pageHtml =
//     function
//     | Page.About -> Info.View.root
//     | Counter -> Counter.View.root model.counter (CounterMsg >> dispatch)
//     | Home -> Home.View.root model.home (HomeMsg >> dispatch)
//   div
//     []
//     [ div
//         [ ClassName "navbar-bg" ]
//         [ div
//             [ ClassName "container" ]
//             [ Navbar.View.root ] ]
//       div
//         [ ClassName "section" ]
//         [ div
//             [ ClassName "container" ]
//             [ div
//                 [ ClassName "columns" ]
//                 [ div
//                     [ ClassName "column is-3" ]
//                     [ menu model.currentPage ]
//                   div
//                     [ ClassName "column" ]
//                     [ pageHtml model.currentPage ] ] ] ] ]
type Model<'a> =
    { CoreModel : FTafl.Core.Model<'a>
      SelectedPos : (FTafl.Core.BoardId * FTafl.Core.Pos) option
      Log : string list }

let initModel =
    { CoreModel = FTafl.SCCG.init
      SelectedPos = None
      Log = List.empty }

type Message<'a> =
    | DoMove of 'a
    | SelectPos of FTafl.Core.BoardId * FTafl.Core.Pos

let rec autoMove (model : FTafl.Core.Model<_>) =
    match FTafl.Core.getPlayer model.ActivePlayer model with
    | { FTafl.Core.AI = Some ai } ->
        let allActions = FTafl.Core.getActivePlayerActions model |> Seq.map snd
        Some <| DoMove(ai model allActions)
    | { FTafl.Core.AI = None } -> None

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
            button [ OnClick(fun _ -> dispatch (DoMove msg)) ]
                [ str <| sprintf "%A" msg ])
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
        |> Seq.groupBy (fun u -> u.Loc)
        |> Seq.sortByDescending fst
        |> Seq.map (fun (bId, units) ->
            let board = FTafl.Core.getBoard bId m
            let (FTafl.Core.Pos(w, h)) = board.Size

            let unitsText =
                units
                |> Seq.map (fun u -> u.Pos, FTafl.Core.unitTextView m u)
                |> Map.ofSeq
            [ 1..h ]
            |> List.map (fun y ->
                tr []
                    ([ 1..w ]
                     |> List.map (fun x ->
                         let pos = bId, FTafl.Core.Pos(x, y)

                         let action =
                             unitsMap
                             |> Map.tryFind pos
                             |> Option.bind
                                 (fun uId -> actions |> Map.tryFind (Some uId))

                         let actionStyle =
                             action |?> (fun _ -> [ CSSProp.BorderColor "red" ])
                             |?? []
                             |> Seq.cast
                             |> Seq.toList

                         let readyStyle =
                             unitsMap
                             |> Map.tryFind pos
                             |?> (fun uId ->
                             if FTafl.Core.getUnitActions uId m
                                |> Seq.isEmpty
                                |> not
                             then [ CSSProp.BorderColor "lightgreen" ]
                             else [])
                             |?? []
                             |> Seq.cast
                             |> Seq.toList

                         let restActionButtons =
                             actions
                             |> Map.toSeq
                             |> Seq.filter (function
                                 | None, _ -> true
                                 | _ -> false)
                             |> actionButtons

                         td []
                             ([ button
                                 ([ OnClick
                                     (fun _ ->
                                     dispatch
                                         (action |?> DoMove |?? SelectPos pos)) ]
                                  @ [ Style
                                          ((if model.SelectedPos
                                               |> Option.exists ((=) pos) then
                                              [ CSSProp.BackgroundColor
                                                  "lightgrey" ]
                                            else
                                                [ CSSProp.BackgroundColor
                                                    "white" ])
                                           @ actionStyle @ readyStyle) ])
                                    [ str
                                        (unitsText
                                         |> Map.tryFind (FTafl.Core.Pos(x, y))
                                         |> Option.defaultValue "empty") ] ]
                              @ if board.Name.Contains "Avatar" then
                                  boardActions bId @ restActionButtons
                                else []))))
            |> fun t -> table [] t
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

    let svgTest =
        svg []
            [ rect
                [ SVGAttr.Width 100.0
                  SVGAttr.Height 100.0
                  SVGAttr.Fill "white"
                  SVGAttr.Stroke "black" ] []
              text
                  [ SVGAttr.X 95.0
                    SVGAttr.Y 95.0
                    SVGAttr.TextAnchor "end" ] [ str "1/1" ] ]

    span []
        [ boards
          hr []
          log
          svgTest ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram (fun _ -> initModel, Cmd.none) update view
//|> Program.toNavigable (parseHash pageParser) urlUpdate

#if DEBUG
|> Program.withDebugger
//|> Program.withHMR

#endif

|> Program.withReact "elmish-app"
|> Program.run
