module rec FTafl.Core

type Rules<'Msg> =
    {
        GetMoves : UnitId -> Model<'Msg> -> (UnitId option * 'Msg) list
        Intercept : UnitId -> 'Msg -> Model<'Msg> -> 'Msg list
        PostAction : UnitId -> Model<'Msg> -> 'Msg list
    }
    static member Default : Rules<'Msg> = {
        GetMoves = (fun _ _ -> [])
        Intercept = (fun _ ev _ -> [ ev ])
        PostAction = (fun _ _ -> [])
        }

type AttrId = AttrId of int
type Attr<'Msg> =
    {
        Name : string
        Rules : Rules<'Msg>
    }
    static member Default name = {
        Name = name
        Rules = Rules<'Msg>.Default
        }
type Pos = Pos of int * int
type AttrDelta = AttrDelta of AttrId * int

type Ev =
    | ChangeAttr of UnitId * AttrDelta
    | MoveUnit of UnitId * BoardId * Pos

    member ev.UnitId =
        match ev with
        | ChangeAttr(uId, _) -> uId
        | MoveUnit(uId, _, _) -> uId

type Action = Ev list

type PlayerId = PlayerId of int
type UnitId = UnitId of int
type Unit =
    {
        Name : string
        Attrs : Map<AttrId, int>
        Owner : PlayerId
        Loc : BoardId
        Pos : Pos
        InnerBoard : BoardId option
    }

type BoardId = BoardId of int
type Board<'Msg> =
    {
        Name : string
        Size : Pos
        Rules : Rules<'Msg>
    }
    static member Default name size = {
        Name = name
        Size = size
        Rules = Rules<_>.Default
    }

type Model<'Msg> =
    {
        Attrs : Map<AttrId, Attr<'Msg>>
        Units : Map<UnitId, Unit>
        Boards : Map<BoardId, Board<'Msg>>
        ToCoreEv : Model<'Msg> -> 'Msg -> Ev list
        UnitTextView : (Unit -> string) option
    }
    static member Default = 
        let d() : Model<'Msg> = { Attrs = Map.empty; Units = Map.empty; Boards = Map.empty; ToCoreEv = (fun _ _ -> []); UnitTextView = None }
        d()

module Model =
    let init f : Model<_> =
        let mutable m = Model<_>.Default
        let idGen cons =
            let mutable x = 0
            fun () -> x <- x + 1; cons x
        let addEntity idGen add f =
            let xId = idGen()
            m <- add m xId (f xId)
            xId
        let attrIdGen = idGen AttrId
        let unitIdGen = idGen UnitId
        let boardIdGen = idGen BoardId
        let playerIdGen = idGen PlayerId
        let addAttr = addEntity attrIdGen (fun m k v -> { m with Attrs = Map.add k v m.Attrs })
        let addUnit = addEntity unitIdGen (fun m k v -> { m with Units = Map.add k v m.Units })
        let addBoard = addEntity boardIdGen (fun m k v -> { m with Boards = Map.add k v m.Boards })
        let postUpdate f = m <- f m
        f addAttr addUnit addBoard playerIdGen postUpdate
        m

let getUnit uId model =
    model.Units |> Map.find uId //TODO: error handling



let getAttr aId model =
    model.Attrs |> Map.find aId //TODO: error handling



let getBoard bId model =
    model.Boards |> Map.find bId //TODO: error handling



let getUnitAttrValue uId aId model =
    let u = getUnit uId model
    u.Attrs |> Map.tryFind aId |> Option.defaultValue 0

let haveUnitAttr uId aId model = getUnitAttrValue uId aId model > 0

let setAttrToValue uId attrId value model =
    let currValue = getUnitAttrValue uId attrId model
    ChangeAttr(uId, AttrDelta(attrId, value - currValue))

let getUnitActions uId model =
    let u = getUnit uId model
    let b = getBoard u.Loc model
    Seq.append
        (b.Rules.GetMoves uId model)
        (u.Attrs |> Map.toSeq |> Seq.collect (fun (aId, x) ->
            let a = getAttr aId model
            a.Rules.GetMoves uId model))
    |> Seq.filter (fun (_, msg) -> not (List.isEmpty <| model.ToCoreEv model msg))

let updateUnit uId f model =
    let u = getUnit uId model |> f
    { model with Units = Map.add uId u model.Units }

let updateSingle model ev =
    //printfn "%A" ev
    match ev with
    | ChangeAttr(uId, AttrDelta(attr, x)) ->
        model |> updateUnit uId (fun u ->
            let oldValue = u.Attrs |> Map.tryFind attr |> Option.defaultValue 0
            let newValue = oldValue + x
            { u with Attrs = ((if newValue > 0 then Map.add attr newValue else Map.remove attr) u.Attrs) })
    | MoveUnit(uId, bId, pos) ->
        model |> updateUnit uId (fun u ->
            { u with Loc = bId; Pos = pos })

let updateMore model (evs : #seq<_>) = (model, evs) ||> Seq.fold updateSingle

let update model evs =
    let evs = evs |> Seq.collect (intercept model)
    printfn "%A" evs
    updateMore model evs

let intercept<'Msg> model (msg : 'Msg) =
    let toCoreEv = model.ToCoreEv model
    let uIds = toCoreEv msg |> List.map (fun (ev : Ev) -> ev.UnitId) |> List.distinct
    let newEvs =
        ([ msg ], uIds) ||> Seq.fold (fun evs uId ->
            let u = getUnit uId model
            let b = getBoard u.Loc model
            let evs = evs |> List.collect (fun ev -> b.Rules.Intercept uId ev model)
            (evs, u.Attrs |> Map.toSeq |> Seq.map fst) ||> Seq.fold (fun evs aId ->
                let a = getAttr aId model
                let newEvs = evs |> List.collect (fun ev -> a.Rules.Intercept uId ev model)
                newEvs
            ))
    let postActions =
        uIds |> Seq.collect (fun uId ->
            let u = getUnit uId model
            u.Attrs |> Map.toSeq |> Seq.map fst |> Seq.collect (fun aId ->
                let a = getAttr aId model
                let m2 = updateMore model (List.collect toCoreEv newEvs)
                a.Rules.PostAction uId m2
            )) |> Seq.toList
    newEvs @ postActions
    |> List.collect toCoreEv

//------------

module Board =
    module Deck =
        let create maxSize name = Board<_>.Default name (Pos(maxSize, 1))
        let moveHere unitId boardId m =
            let board = getBoard boardId m
            let (Pos(maxSize, _)) = board.Size
            let boardUnitsIndexes =
                m.Units |> Map.values |> Seq.filter (fun u -> u.Loc = boardId)
                |> Seq.map (fun u -> let (Pos(x, _)) = u.Pos in x) |> Seq.toList
            let firstEmptyIndex = [ 0 ] @ boardUnitsIndexes @ [ maxSize + 1 ] |> Seq.sort |> Seq.pairwise |> Seq.tryFind (fun (i, j) -> i + 1 < j) |?> fun (i, _) -> i + 1
            //printfn "Board.Deck.moveHere: %A" <| (boardUnitsIndexes, firstEmptyIndex)
            firstEmptyIndex |?> fun x -> MoveUnit(unitId, boardId, Pos(x, 1))
        let getFirstUnitId boardId m =
            m.Units |> Map.toSeq |> Seq.filter (fun (_, u) -> u.Loc = boardId) |> Seq.tryHead |?> fst

//------------

let unitTextView model =
    model.UnitTextView |?? (fun (unit: Unit) ->
        unit.Name + " : " + (unit.Attrs |> Map.toSeq |> Seq.map (fun (aId, x) ->
            let a = getAttr aId model
            a.Name + " " + string x) |> String.concat ", "))
