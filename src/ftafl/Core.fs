module FTafl.Core

[<AutoOpen>]
module rec Types =
    type Rules<'Msg> =
        { GetMoves: UnitId -> Model<'Msg> -> (UnitId option * 'Msg) list
          Intercept: UnitId -> 'Msg -> Model<'Msg> -> 'Msg list
          PostAction: UnitId -> Model<'Msg> -> 'Msg list }
        static member Default: Rules<'Msg> =
            { GetMoves = (fun _ _ -> [])
              Intercept = (fun _ ev _ -> [ ev ])
              PostAction = (fun _ _ -> []) }

    type AttrId = AttrId of int

    type Attr<'Msg> =
        { Name: string
          Rules: Rules<'Msg> }
        static member Default name =
            { Name = name
              Rules = Rules<'Msg>.Default }

    type Pos = Pos of int * int

    type AttrDelta = AttrDelta of AttrId * int

    type Ev =
        | ChangeAttr of UnitId * AttrDelta
        | MoveUnit of UnitId * BoardId * Pos
        | SetActivePlayer of PlayerId
        member ev.UnitId =
            match ev with
            | ChangeAttr(uId, _)
            | MoveUnit(uId, _, _) -> Some uId
            | SetActivePlayer _ -> None

    type Action = Ev list

    type PlayerId = PlayerId of int

    type Player<'Msg> =
        { AI: (Model<'Msg> -> seq<'Msg> -> 'Msg) option }

    type UnitId = UnitId of int

    type Unit =
        { Name: string
          Attrs: Map<AttrId, int>
          Owner: PlayerId
          Loc: BoardId
          Pos: Pos
          InnerBoard: BoardId option }

    type BoardId = BoardId of int

    type Board<'Msg> =
        { Name: string
          Size: Pos
          Rules: Rules<'Msg> }
        static member Default name size =
            { Name = name
              Size = size
              Rules = Rules<_>.Default }

    type Model<'Msg> =
        { Attrs: Map<AttrId, Attr<'Msg>>
          Units: Map<UnitId, Unit>
          Boards: Map<BoardId, Board<'Msg>>
          Players: Map<PlayerId, Player<'Msg>>
          ActivePlayer: PlayerId
          ToCoreEv: Model<'Msg> -> 'Msg -> Ev list
          UnitTextView: (Unit -> string list) option }
        static member Default =
            let d(): Model<'Msg> =
                { Attrs = Map.empty
                  Units = Map.empty
                  Boards = Map.empty
                  Players = Map.empty
                  ActivePlayer = PlayerId -1
                  ToCoreEv = (fun _ _ -> [])
                  UnitTextView = None }
            d()

open Types

let getUnit uId model = model.Units |> Map.find uId //TODO: error handling
let getAttr aId model = model.Attrs |> Map.find aId //TODO: error handling
let getBoard bId model = model.Boards |> Map.find bId //TODO: error handling
let getPlayer pId model = model.Players |> Map.find pId

let getUnitAttrValue uId aId model =
    let u = getUnit uId model
    u.Attrs
    |> Map.tryFind aId
    |> Option.defaultValue 0

let haveUnitAttr uId aId model = getUnitAttrValue uId aId model > 0

let setAttrToValue uId attrId value model =
    let currValue = getUnitAttrValue uId attrId model
    ChangeAttr(uId, AttrDelta(attrId, value - currValue))

let getUnitActions uId model =
    let u = getUnit uId model
    let b = getBoard u.Loc model
    Seq.append (b.Rules.GetMoves uId model)
        (u.Attrs
         |> Map.toSeq
         |> Seq.collect (fun (aId, x) ->
             let a = getAttr aId model
             a.Rules.GetMoves uId model))
    |> Seq.filter (fun (_, msg) -> not (List.isEmpty <| model.ToCoreEv model msg))

let getActivePlayerActions model =
    model.Units
    |> Map.toSeq
    |> Seq.filter (fun (_, u) -> u.Owner = model.ActivePlayer)
    |> Seq.collect (fun (uId, _) -> getUnitActions uId model)

let updateUnit uId f model =
    let u = getUnit uId model |> f
    { model with Units = Map.add uId u model.Units }

let updateSingle model ev =
    //printfn "%A" ev
    match ev with
    | ChangeAttr(uId, AttrDelta(attr, x)) ->
        model
        |> updateUnit uId (fun u ->
               let oldValue =
                   u.Attrs
                   |> Map.tryFind attr
                   |> Option.defaultValue 0

               let newValue = oldValue + x
               { u with
                     Attrs =
                         ((if newValue > 0 then Map.add attr newValue else Map.remove attr) u.Attrs) })
    | MoveUnit(uId, bId, pos) ->
        model
        |> updateUnit uId (fun u ->
               { u with
                     Loc = bId
                     Pos = pos })
    | SetActivePlayer pId -> { model with ActivePlayer = pId }

let updateMore<'Msg> (model: Model<'Msg>) (evs: seq<Ev>) = (model, evs) ||> Seq.fold updateSingle

let intercept<'Msg> updateMore (model: Model<'Msg>) (msg: 'Msg) =
    let toCoreEv = model.ToCoreEv model

    let uIds =
        toCoreEv msg
        |> List.choose (fun (ev: Ev) -> ev.UnitId)
        |> List.distinct

    let newEvs =
        ([ msg ], uIds)
        ||> Seq.fold (fun evs uId ->
                let u = getUnit uId model
                let b = getBoard u.Loc model
                let evs = evs |> List.collect (fun ev -> b.Rules.Intercept uId ev model)
                (evs,
                 u.Attrs
                 |> Map.toSeq
                 |> Seq.map fst)
                ||> Seq.fold (fun evs aId ->
                        let a = getAttr aId model
                        let newEvs = evs |> List.collect (fun ev -> a.Rules.Intercept uId ev model)
                        newEvs))

    let postActions =
        uIds
        |> Seq.collect (fun uId ->
            let u = getUnit uId model
            u.Attrs
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.collect (fun aId ->
                let a = getAttr aId model
                let m2 = updateMore model (List.collect toCoreEv newEvs)
                a.Rules.PostAction uId m2))
        |> Seq.toList

    newEvs @ postActions |> List.collect toCoreEv

let update<'Msg> (model: Model<'Msg>) (evs: seq<'Msg>) =
    let evs = evs |> Seq.collect (intercept updateMore model)
    printfn "%A" evs
    updateMore model evs

//------------
module Board =
    module Deck =
        let create maxSize name = Board<_>.Default name (Pos(maxSize, 1))

        let moveHere unitId boardId m =
            let board = getBoard boardId m
            let (Pos(maxSize, _)) = board.Size

            let boardUnitsIndexes =
                m.Units
                |> Map.values
                |> Seq.filter (fun u -> u.Loc = boardId)
                |> Seq.map (fun u ->
                    let (Pos(x, _)) = u.Pos in x)
                |> Seq.toList

            let firstEmptyIndex =
                [ 0 ] @ boardUnitsIndexes @ [ maxSize + 1 ]
                |> Seq.sort
                |> Seq.pairwise
                |> Seq.tryFind (fun (i, j) -> i + 1 < j)
                |?> fun (i, _) -> i + 1

            //printfn "Board.Deck.moveHere: %A" <| (boardUnitsIndexes, firstEmptyIndex)
            firstEmptyIndex |?> fun x -> MoveUnit(unitId, boardId, Pos(x, 1))

        let getFirstUnitId boardId m =
            m.Units
            |> Map.toSeq
            |> Seq.filter (fun (_, u) -> u.Loc = boardId)
            |> Seq.tryHead
            |?> fst

//------------
let unitTextView model =
    model.UnitTextView |?? (fun (unit: Unit) ->
    [ unit.Name
      (unit.Attrs
       |> Map.toSeq
       |> Seq.map (fun (aId, x) ->
           let a = getAttr aId model
           a.Name + " " + string x)
       |> String.concat ", ") ])

//---------------

module Model =
    // type InitEv<'Msg> =
    //     | AddAttr of AttrId * Attr<'Msg>
    //     | AddUnit of UnitId * Unit
    //     | AddBoard of BoardId * Board<'Msg>
    //     | AddPlayer of PlayerId * Player<'Msg>

    // let applyInitEv (ev: InitEv<_>) m =
    //     match ev with
    //     | AddAttr (attrId, attr) -> { m with Attrs = Map.add attrId attr m.Attrs } : Model<_>
    //     | AddUnit (unitId, unit) -> { m with Units = Map.add unitId unit m.Units }
    //     | AddBoard (boardId, board) -> { m with Boards = Map.add boardId board m.Boards }
    //     | AddPlayer (playerId, player) -> { m with Players = Map.add playerId player m.Players }

    // let nextAttrId m = m.Attrs |> Map.keys |> Seq.max |> fun (AttrId aId) -> AttrId (aId + 1)
    // let nextUnitId m = m.Units |> Map.keys |> Seq.max |> fun (UnitId aId) -> UnitId (aId + 1)
    // let nextBoardId m = m.Boards |> Map.keys |> Seq.max |> fun (BoardId aId) -> BoardId (aId + 1)
    // let nextPlayerId m = m.Players |> Map.keys |> Seq.max |> fun (PlayerId aId) -> PlayerId (aId + 1)

    // type InitCmds<'Msg> =
    //     { AddAttr: (AttrId -> Attr<'Msg>) -> AttrId
    //       AddUnit: (UnitId -> Unit) -> UnitId
    //       AddBoard: (BoardId -> Board<'Msg>) -> BoardId
    //       AddPlayer: (PlayerId -> Player<'Msg>) -> PlayerId
    //       UpdateBoard: BoardId -> (Board<'Msg> -> Board<'Msg>) -> unit
    //       ToCoreEv: (Model<'Msg> -> 'Msg -> Ev list) -> unit
    //       UnitTextView: (Unit -> string list) -> unit
    //       ActivePlayer: PlayerId -> unit
    //       InitialMsgs: 'Msg seq -> unit }

    open Prelude

    let idGen cons =
        let mutable x = 0
        fun () ->
            x <- x + 1
            cons x

    let addEntity idGen add f m =
        let xId = idGen()
        xId, add m xId (f xId)

    let updateEntity xId get add f m =
        let x = get m xId
        add m xId (f x)

    let attrIdGen = idGen AttrId
    let unitIdGen = idGen UnitId
    let boardIdGen = idGen BoardId
    let playerIdGen = idGen PlayerId
    let addAttr f = addEntity attrIdGen (fun m k v -> { m with Attrs = Map.add k v m.Attrs }) f |> State
    let addUnit f = addEntity unitIdGen (fun m k v -> { m with Units = Map.add k v m.Units }) f |> State
    let addBoard f = addEntity boardIdGen (fun m k v -> { m with Boards = Map.add k v m.Boards }) f |> State
    let addPlayer f = addEntity playerIdGen (fun m k v -> { m with Players = Map.add k v m.Players }) f |> State
    let updateBoard bId f =
        updateEntity bId (fun m k -> Map.find k m.Boards) (fun m k v -> { m with Boards = Map.add k v m.Boards }) f |> State.unit
    let toCoreEv f = State.unit (fun m -> { m with ToCoreEv = f })
    let unitTextView f = State.unit (fun m -> { m with UnitTextView = Some f })
    let activePlayer pId = State.unit (fun m -> { m with ActivePlayer = pId})
    let initialMsgs msgs =
        State.unit (fun m -> (m, msgs) ||> Seq.fold (fun m msg -> update m [ msg ]))
        
    let init<'Msg> (s: State<Model<'Msg>, unit>) : Model<'Msg> =
        s |> State.run Model<_>.Default |> snd

    let modelTest() = init <| state {
        let! _ = addAttr (fun _ -> Attr<_>.Default "A")
        ()
    }    