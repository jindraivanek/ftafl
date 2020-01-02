module FTafl.Games.Tablut

open FTafl.Core

let map = Map.ofSeq

type Msg =
    | Move of UnitId * Pos
    | Dead of UnitId

let init =
    state {
        let! boardId = Model.addBoard <| fun _ -> Board.Tiled.create 9 9 "Board"
        let! graveyardId = Model.addBoard <| fun _ -> Board.Deck.create 21 "Graveyard"
        let! player1Id = Model.addPlayer <| fun _ -> { AI = None }
        let! player2Id = Model.addPlayer <| fun player2Id -> { AI = Some(AI.multiTurnAI 2 player2Id) }

        let getOpPlayer pId =
            if pId = player1Id then player2Id else player1Id

        let move uId m =
            let u = getUnit uId m
            Board.Tiled.rookMoves u.Pos boardId m |> List.map (fun p -> Some(boardId, p), Move(uId, p))

        let moveRules: Rules<Msg> = { Rules.Default with GetMoves = move }

        let! kingId = Model.addAttr <| fun _ ->
                          { Attr<_>.Default "K" with
                                CostWeight = 100.0
                                Rules = moveRules }
        let! attackerId = Model.addAttr <| fun _ -> { Attr<_>.Default "A" with Rules = moveRules }
        let! defenderId = Model.addAttr <| fun _ -> { Attr<_>.Default "D" with Rules = moveRules }

        let toCoreEv m =
            function
            | Move(uId, p) ->
                let pl = (getUnit uId m).Owner
                Some
                    [ MoveUnit(uId, boardId, p)
                      SetActivePlayer(getOpPlayer pl) ]
            | Dead uId -> 
                let removeAttrs uId =
                    let u = getUnit uId m
                    u.Attrs |> Map.toSeq |> Seq.map (fun (aId, a) -> ChangeAttr (uId, AttrDelta(aId, -a))) |> Seq.toList
                Board.Deck.moveHere uId graveyardId m |> Option.map (fun x -> (removeAttrs uId) @ [x])

        let checkCapture uId m =
            let pl = (getUnit uId m).Owner
            let unitsPos =
                m.Units
                |> Map.toSeq
                |> Seq.map (fun (uId, u) -> u.Pos, (uId, u))
                |> map

            let isEnemy p u =
                unitsPos
                |> Map.tryFind p
                |?> (fun (_, u2) -> u.Owner <> u2.Owner)
                |?? false
            unitsPos
            |> Map.toSeq
            |> Seq.collect (fun (p, (uId, u)) ->
                let ch f = isEnemy (f p) u
                if u.Owner <> pl && ((ch Pos.up && ch Pos.down) || (ch Pos.left && ch Pos.right))
                then [ Dead uId ]
                else [])
            |> Seq.toList

        let globalIntercept _ ev m =
            match ev with
            | Move(uId, p) -> ev :: checkCapture uId m
            | msg -> [ msg ]

        do! Model.updateBoard boardId (fun b -> { b with Rules = { b.Rules with Intercept = globalIntercept } })

        let mkUnit attrId playerId (x, y) unitId =
            { Name =
                  if attrId = kingId then "K"
                  elif attrId = attackerId then "A"
                  elif attrId = defenderId then "D"
                  else "?"
                  + sprintf "(%i)" unitId
              Owner = playerId
              InnerBoard = None
              Loc = boardId
              Pos = Pos(x, y)
              Attrs = map [ attrId, 1 ] }

        let units =
            [ mkUnit kingId player1Id (5, 5)
              mkUnit defenderId player1Id (3, 5)
              mkUnit defenderId player1Id (4, 5)
              mkUnit defenderId player1Id (6, 5)
              mkUnit defenderId player1Id (7, 5)
              mkUnit defenderId player1Id (5, 3)
              mkUnit defenderId player1Id (5, 4)
              mkUnit defenderId player1Id (5, 6)
              mkUnit defenderId player1Id (5, 7)
              mkUnit attackerId player2Id (4, 1)
              mkUnit attackerId player2Id (5, 1)
              mkUnit attackerId player2Id (6, 1)
              mkUnit attackerId player2Id (5, 2)
              mkUnit attackerId player2Id (4, 9)
              mkUnit attackerId player2Id (5, 9)
              mkUnit attackerId player2Id (6, 9)
              mkUnit attackerId player2Id (5, 8)
              mkUnit attackerId player2Id (1, 4)
              mkUnit attackerId player2Id (1, 5)
              mkUnit attackerId player2Id (1, 6)
              mkUnit attackerId player2Id (2, 5)
              mkUnit attackerId player2Id (9, 4)
              mkUnit attackerId player2Id (9, 5)
              mkUnit attackerId player2Id (9, 6)
              mkUnit attackerId player2Id (8, 5) ]

        let! _ = units |> State.List.map (fun u -> Model.addUnit (fun (UnitId uId) -> u uId))

        let unitTextView (unit: Unit) =
            let a aId =
                unit.Attrs
                |> Map.tryFind aId
                |?? 0
            [ unit.Name; ""; ""; "" ]

        let toCoreEv m msg = toCoreEv m msg |> Option.defaultValue []

        do! Model.toCoreEv toCoreEv
        do! Model.unitTextView unitTextView

        do! Model.activePlayer player1Id
    }
    |> Model.init<Msg>
