module FTafl.SCCG

open FTafl.Core

let map = Map.ofSeq

type Msg =
    | Attack of UnitId * UnitId
    | Cast of UnitId
    | Dead of UnitId
    | EndTurn of PlayerId
    | Draw of PlayerId

let rng = System.Random()

let init =
    let rngAI =
        let rng = System.Random()
        fun _ moves ->
            let moves = moves |> Seq.toArray
            let i = rng.Next() % moves.Length
            moves.[i]

    let simpleAI myBoards opBoards =
        let cost m =
            let costForBoard bId =
                m.Units
                |> Map.values
                |> Seq.filter (fun u -> u.Loc = bId)
                |> Seq.sumBy (fun u ->
                    u.Attrs
                    |> Map.values
                    |> Seq.sum)
            (Seq.sumBy costForBoard myBoards)
            - (Seq.sumBy costForBoard opBoards)
        fun model moves ->
            let moves = moves |> Seq.toArray
            moves |> Seq.maxBy (fun c -> Core.update model [ c ] |> cost)

    Model.init (fun addAttr addUnit addBoard addPlayer postUpdate ->
        let graveyard1Id =
            addBoard <| fun _ -> Board.Deck.create 8 "Player1 Graveyard"
        let deck1Id = addBoard <| fun _ -> Board.Deck.create 30 "Player1 Deck"
        let hand1Id = addBoard <| fun _ -> Board.Deck.create 9 "Player1 Hand"
        let avatar1Id =
            addBoard <| fun _ -> Board.Deck.create 1 "Player1 Avatar"
        let board1Id = addBoard <| fun _ -> Board.Deck.create 8 "Player1 Board"
        let board2Id = addBoard <| fun _ -> Board.Deck.create 8 "Player2 Board"
        let avatar2Id =
            addBoard <| fun _ -> Board.Deck.create 1 "Player2 Avatar"
        let hand2Id = addBoard <| fun _ -> Board.Deck.create 9 "Player2 Hand"
        let deck2Id = addBoard <| fun _ -> Board.Deck.create 30 "Player2 Deck"
        let graveyard2Id =
            addBoard <| fun _ -> Board.Deck.create 8 "Player2 Graveyard"
        let player1Id = addPlayer <| fun _ -> { AI = None }
        let player2Id = addPlayer <| fun _ -> { AI = Some (simpleAI [board2Id; avatar2Id] [board1Id; avatar1Id]) }

        let getOpPlayer pId =
            if pId = player1Id then player2Id
            else player1Id

        let getBoard b1 b2 pId =
            if pId = player1Id then b1
            else b2

        let boardId = getBoard board1Id board2Id
        let opBoardId = getOpPlayer >> boardId

        let isMyBoard pId bId =
            if pId = player1Id then bId = board1Id
            else bId = board2Id

        let handId = getBoard hand1Id hand2Id
        let graveyardId = getBoard graveyard1Id graveyard2Id
        let avatarBoardId = getBoard avatar1Id avatar2Id
        let deckBoardId = getBoard deck1Id deck2Id

        let getAvatarId pId m =
            let bId = avatarBoardId pId
            m.Units
            |> Map.toSeq
            |> Seq.find (fun (_, u) -> u.Loc = bId)
            |> fst

        let getOpAvatarId pId m = getAvatarId (getOpPlayer pId) m
        let manaId = addAttr <| fun _ -> Attr<_>.Default "M"
        let maxManaId = addAttr <| fun _ -> Attr<_>.Default "MM"
        let exhaustId = addAttr <| fun _ -> Attr<_>.Default "E"

        let move uId m xs =
            let u = getUnit uId m
            let aId = getAvatarId u.Owner m
            if haveUnitAttr aId exhaustId m then []
            else xs

        let summonMove uId m = move uId m [ None, Cast uId ]

        let endTurnMove uId m =
            let u = getUnit uId m
            [ None, EndTurn u.Owner ] |> move uId m

        let attackMove uId m =
            let u = getUnit uId m
            if not (isMyBoard u.Owner (u.Loc)) || haveUnitAttr uId exhaustId m then
                []
            else
                m.Units
                |> Map.toSeq
                |> Seq.filter
                    (fun (_, u2) ->
                    u2.Loc = opBoardId u.Owner
                    || u2.Loc = avatarBoardId (getOpPlayer u.Owner))
                |> Seq.map (fun (u2Id, _) -> Some u2Id, Attack(uId, u2Id))
                |> Seq.toList
                |> move uId m

        let healthId =
            addAttr <| fun thisId ->
                { Attr<_>.Default "H" with
                      Rules =
                          { Rules<_>.Default with
                                PostAction =
                                    fun uId m ->
                                        if getUnitAttrValue uId thisId m <= 0 then
                                            [ Dead uId ]
                                        else [] } }

        let attackId =
            addAttr
            <| fun thisId ->
                { Attr<_>.Default "A" with
                      Rules = { Rules<_>.Default with GetMoves = attackMove } }

        // let armorId = addAttr <| fun thisId ->
        //     { Attr.Default "D" with
        //         Intercept = fun _ ev m ->
        //             match ev with
        //             | ChangeAttr(uId, AttrDelta(healthId, x)) when x < 0 ->
        //                 let armorValue = getUnitAttrValue uId thisId m
        //                 [ ChangeAttr(uId, AttrDelta(healthId, min 0 (x + armorValue))) ]
        //             | ev -> [ ev ]
        //     }
        let toCoreEv m =
            function
            | Attack(uId, u2Id) ->
                let atk = getUnitAttrValue uId attackId m
                let atk2 = getUnitAttrValue u2Id attackId m
                Some
                    [ ChangeAttr(uId, AttrDelta(exhaustId, 1))
                      ChangeAttr(u2Id, AttrDelta(healthId, -atk))
                      ChangeAttr(uId, AttrDelta(healthId, -atk2)) ]
            | Cast uId ->
                let u = getUnit uId m
                let avatarId = getAvatarId u.Owner m
                let manaCost = getUnitAttrValue uId manaId m

                let summonEv =
                    if (getUnitAttrValue avatarId manaId m) >= manaCost then
                        Some(ChangeAttr(avatarId, AttrDelta(manaId, -manaCost)))
                    else None
                [ summonEv
                  Board.Deck.moveHere uId (boardId u.Owner) m
                  Some(ChangeAttr(uId, AttrDelta(exhaustId, 1))) ]
                |> List.chooseAll id
            | Dead uId ->
                let u = getUnit uId m
                Board.Deck.moveHere uId (graveyardId u.Owner) m
                |> Option.map List.singleton
            | EndTurn pId ->
                let p2Id = getOpPlayer pId
                let aId = getAvatarId pId m
                let a2Id = getAvatarId p2Id m

                let p2UnitIds =
                    m.Units
                    |> Map.toSeq
                    |> Seq.filter
                        (fun (_, u) -> u.Loc = boardId p2Id && u.Owner = p2Id)
                    |> Seq.map fst
                    |> Seq.toList

                let newMaxMana = getUnitAttrValue a2Id maxManaId m + 1
                (p2UnitIds
                 |> List.map
                     (fun uId -> ChangeAttr(uId, AttrDelta(exhaustId, -1))))
                @ [ ChangeAttr(aId, AttrDelta(exhaustId, 1))
                    ChangeAttr(a2Id, AttrDelta(exhaustId, -1)) ]
                  @ [ ChangeAttr(a2Id, AttrDelta(maxManaId, 1))
                      setAttrToValue a2Id manaId newMaxMana m ]
                    @ [ SetActivePlayer p2Id ]
                |> Some
            | Draw pId ->
                let deck = deckBoardId pId
                Board.Deck.getFirstUnitId deck m
                |?> fun uId ->
                    Board.Deck.moveHere uId (handId pId) m |> Option.toList

        let globalIntercept _ ev _ =
            match ev with
            | EndTurn pId as msg ->
                [ msg
                  Draw(getOpPlayer pId) ]
            | msg -> [ msg ]

        let baseUnit playerId =
            { Name = "unit"
              Owner = playerId
              InnerBoard = None
              Loc = deckBoardId playerId
              Pos = Pos(0, 0)
              Attrs =
                  map
                      [ healthId, 5
                        attackId, 2
                        manaId, 3 ] }

        let genUnitAttrs() =
            let genAttr maxVal =
                rng.Next() % (maxVal * maxVal)
                |> float
                |> sqrt
                |> int
                |> fun x -> maxVal - x

            let manaCost attrs =
                attrs
                |> Map.values
                |> Seq.sum
                |> fun x -> x / 2

            let attrs =
                map
                    [ healthId, genAttr 10
                      attackId, genAttr 10 ]

            Map.add manaId (manaCost attrs) attrs

        let _ =
            let attrs = map [ healthId, 20 ]
            [ player1Id; player2Id ]
            |> List.map (fun ((PlayerId pId) as p) ->
                addUnit (fun (UnitId uId) ->
                    { //let attrs = if p = player1Id then attrs else Map.add exhaustId 1 attrs
                      baseUnit p with
                          Name = sprintf "avatar_%i(%i)" pId uId
                          Loc = avatarBoardId p
                          Pos = Pos(1, 1)
                          Attrs = attrs }))

        let _ =
            [ player1Id; player2Id ]
            |> List.collect (fun p ->
                [ 1..30 ]
                |> List.map (fun i ->
                    addUnit (fun (UnitId uId) ->
                        { baseUnit p with
                              Name = sprintf "unit(%i)" uId
                              Loc = deckBoardId p
                              Pos = Pos(i, 1)
                              Attrs = genUnitAttrs() })))

        let unitTextView (unit : Unit) =
            let a aId =
                unit.Attrs
                |> Map.tryFind aId
                |?? 0
            sprintf "%s : (%i) %i/%i" unit.Name (a manaId) (a attackId)
                (a healthId)
            |> fun x ->
                if a exhaustId > 0 then sprintf "** %s **" x
                else x

        postUpdate (fun m ->
            let boards =
                let h1 = m.Boards.[hand1Id]
                let h2 = m.Boards.[hand2Id]
                let a1 = m.Boards.[avatar1Id]
                let a2 = m.Boards.[avatar2Id]
                m.Boards
                |> Map.add hand1Id
                       { h1 with Rules = { h1.Rules with GetMoves = summonMove } }
                |> Map.add hand2Id
                       { h2 with Rules = { h2.Rules with GetMoves = summonMove } }
                |> Map.add avatar1Id
                       { a1 with
                             Rules =
                                 { a1.Rules with
                                       GetMoves = endTurnMove
                                       Intercept = globalIntercept } }
                |> Map.add avatar2Id
                       { a2 with
                             Rules =
                                 { a2.Rules with
                                       GetMoves = endTurnMove
                                       Intercept = globalIntercept } }

            let toCoreEv m msg = toCoreEv m msg |> Option.defaultValue []

            let m =
                { m with
                      Boards = boards
                      ToCoreEv = toCoreEv
                      UnitTextView = Some unitTextView }

            let repeat n msg = [ 1..n ] |> List.map (fun _ -> msg)
            (m,
             repeat 2 (Draw player1Id)
             @ repeat 4 (Draw player2Id) @ [ EndTurn player2Id ])
            ||> Seq.fold (fun m msg -> update m [ msg ])
            |> fun m -> { m with ActivePlayer = player1Id })
        ())
