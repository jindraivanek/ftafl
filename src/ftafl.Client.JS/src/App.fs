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

type Model<'a> = {
    CoreModel : FTafl.Core.Model<'a>
    SelectedPos : (FTafl.Core.BoardId * FTafl.Core.Pos) option
 }

let initModel = {
    CoreModel = FTafl.SCCG.init
    SelectedPos = None
 }

type Message<'a> =
    | DoMove of 'a
    | SelectPos of FTafl.Core.BoardId * FTafl.Core.Pos

let update message model =
    printfn "%A" message
    match message with
    | DoMove msg -> { model with CoreModel = FTafl.Core.update model.CoreModel [ msg ] }
    | SelectPos(bId, p) as ev -> { model with SelectedPos = Some(bId, p) }

let view (model : Model<_>) dispatch =
    let m = model.CoreModel
    //printfn "%A" m
    let unitsMap = m.Units |> Map.toSeq |> Seq.map (fun (uId, u) -> (u.Loc, u.Pos), uId) |> Map.ofSeq
    let unitIdToPos = unitsMap |> Map.toSeq |> Seq.map (fun (k,v) -> v,k) |> Map.ofSeq
    let actions =
        let selectedUnit = model.SelectedPos |> Option.bind (fun p -> unitsMap |> Map.tryFind p)
        selectedUnit |> Option.map (fun u -> FTafl.Core.getUnitActions u m |> Seq.toList) |> Option.defaultValue []
        |> Map.ofSeq
    let actionButtons =
        actions |> Map.toList
        |> List.map (fun (_,msg) -> button [ OnClick (fun _ -> dispatch (DoMove msg)) ] [ str <| sprintf "%A" msg ])
    let boards =
        m.Units |> Map.toSeq |> Seq.map snd |> Seq.groupBy (fun u -> u.Loc) |> Seq.sortBy fst
        |> Seq.map (fun (bId, units) ->
            let board = FTafl.Core.getBoard bId m
            let (FTafl.Core.Pos(w, h)) = board.Size
            let unitsText = units |> Seq.map (fun u -> u.Pos, FTafl.Core.unitTextView m u) |> Map.ofSeq
            [ 1..h ] |> List.map (fun y -> tr [] ([ 1..w ] |> List.map (fun x ->
                let pos = bId, FTafl.Core.Pos(x, y)
                let action = unitsMap |> Map.tryFind pos |> Option.bind (fun uId -> actions |> Map.tryFind (Some uId))
                let actionStyle = action |?> (fun _ -> [Style [CSSProp.BorderColor "red" ]]) |?? [] |> Seq.cast |> Seq.toList
                td []
                    ([ button
                        ([ OnClick (fun _ -> dispatch (action |?> DoMove |?? SelectPos pos)) ]
                         @ (if model.SelectedPos |> Option.exists ((=) pos) then [ Style [CSSProp.BackgroundColor "lightgreen" ] ] else [ Style [CSSProp.BackgroundColor "white" ] ])
                         @ actionStyle)
                        [ str (unitsText |> Map.tryFind (FTafl.Core.Pos(x, y)) |> Option.defaultValue "empty") ] ]
                    @ if board.Name.Contains "Avatar" then actionButtons else []))))
            |> fun t -> table [] t
            |> fun t -> div [] [ str board.Name; t ]
        ) |> Seq.toList |> span []
    boards


open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkSimple (fun _ -> initModel) update view
//|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
//|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
