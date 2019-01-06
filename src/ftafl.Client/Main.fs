module FTafl.Client.Main

open Bolero
open Bolero.Html
open Elmish


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
    | DoMove msg -> { model with CoreModel = FTafl.Core.update model.CoreModel [msg] }
    | SelectPos(bId, p) as ev -> { model with SelectedPos = Some(bId, p) }

let view (model : Model<_>) dispatch =
    let m = model.CoreModel
    //printfn "%A" m
    let unitsMap = m.Units |> Map.toSeq |> Seq.map (fun (uId, u) -> (u.Loc, u.Pos), uId) |> Map.ofSeq
    let boards =
        m.Units |> Map.toSeq |> Seq.map snd |> Seq.groupBy (fun u -> u.Loc) |> Seq.sortBy fst
        |> Seq.map (fun (bId, units) ->
            let board = FTafl.Core.getBoard bId m
            let (FTafl.Core.Pos(w, h)) = board.Size
            let unitsText = units |> Seq.map (fun u -> u.Pos, FTafl.Core.unitTextView m u) |> Map.ofSeq
            [ 1..h ] |> List.map (fun y -> tr [] ([ 1..w ] |> List.map (fun x ->
                let pos = bId, FTafl.Core.Pos(x, y)
                td []
                    [ button
                        ([ on.click (fun _ -> dispatch (SelectPos pos)) ] 
                         @ (if model.SelectedPos |> Option.exists ((=) pos) then [ attr.style "background-color:lightgreen" ] else [ attr.style "background-color:white" ]))
                        [ text (unitsText |> Map.tryFind (FTafl.Core.Pos(x, y)) |> Option.defaultValue "empty") ] ])))
            |> fun t -> concat [text board.Name; table [] t]
        ) |> Seq.toList |> span []
    let actions =
        let selectedUnit = model.SelectedPos |> Option.bind (fun p -> unitsMap |> Map.tryFind p)
        selectedUnit |> Option.map (fun u -> FTafl.Core.getUnitActions u m |> Seq.toList) |> Option.defaultValue []
        |> List.map (fun msg -> button [ on.click (fun _ -> dispatch (DoMove msg)) ] [ textf "%A" msg ])
    concat (boards :: actions)

type MyApp() =
    inherit ProgramComponent<Model<FTafl.SCCG.Msg>, Message<FTafl.SCCG.Msg>>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
