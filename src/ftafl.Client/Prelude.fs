[<AutoOpen>]
module Prelude

let (|?>) o f = Option.map f o
let (|?>=) o f = Option.bind f o

/// Option.defaultValue
let (|??) o x = Option.defaultValue x o

[<CompilerMessage("type hole", 0)>]
let ___<'a> = Unchecked.defaultof<'a>
let inline (!!) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

module List =
    let chooseAll f xs =
        let l = List.length xs
        xs |> List.choose f |> fun ys -> if List.length ys = l then Some ys else None

module Map =
    let keys m = m |> Map.toSeq |> Seq.map fst
    let values  m = m |> Map.toSeq |> Seq.map snd

