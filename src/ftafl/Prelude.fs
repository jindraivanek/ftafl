[<AutoOpen>]
module Prelude

/// Option.map
let (|?>) o f = Option.map f o

/// Option.bind
let (|?>=) o f = Option.bind f o

/// Option.defaultValue
let (|??) o x = Option.defaultValue x o

[<CompilerMessage("type hole", 9999)>]
let ___<'a> = failwith<'a> "type hole"

let inline (!!) (x : ^a) : ^b =
    ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

module List =
    let chooseAll f xs =
        let l = List.length xs
        xs
        |> List.choose f
        |> fun ys ->
            if List.length ys = l then Some ys
            else None

module Map =
    let keys m =
        m
        |> Map.toSeq
        |> Seq.map fst

    let values m =
        m
        |> Map.toSeq
        |> Seq.map snd

module Seq =
    let groupInto keys f xs =
        let g = Seq.groupBy f xs
        let gKeys = Seq.map fst g |> set
        let emptyKeys = (set keys - gKeys)
        Seq.append g (emptyKeys |> Seq.map (fun x -> x, Seq.empty))