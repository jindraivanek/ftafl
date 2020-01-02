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

let inline (!!) (x: ^a): ^b = ((^a or ^b): (static member op_Implicit: ^a -> ^b) x)

let (|Eq|_|) x y =
    if x = y then Some() else None

let tee f x = f x; x
let teePrint x = tee (printfn "%A") x

module List =
    let chooseAll f xs =
        let l = List.length xs
        xs
        |> List.choose f
        |> fun ys ->
            if List.length ys = l then Some ys else None

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

type State<'s, 'a> = State of ('s -> ('a * 's))

module State =
    let unit f = State(fun x -> (), f x)

    let inline run state x =
        let (State(f)) = x in f state

    let get = State(fun s -> s, s)
    let put newState = State(fun _ -> (), newState)

    let map f s =
        State(fun (state: 's) ->
            let x, state = run state s
            f x, state)

    let combine x1 x2 =
        State(fun state ->
            let result, state = run state x1
            run state x2)

    module List =
        let map f xs =
            xs
            |> List.map f
            |> List.reduceBack (fun x1 x2 -> combine x1 x2)

        let collect f xs =
            xs
            |> List.collect f
            |> List.reduceBack (fun x1 x2 -> combine x1 x2)

/// The state monad passes around an explicit internal state that can be
/// updated along the way. It enables the appearance of mutability in a purely
/// functional context by hiding away the state when used with its proper operators
/// (in StateBuilder()). In other words, you implicitly pass around an implicit
/// state that gets transformed along its journey through pipelined code.
type StateBuilder() =
    member this.Zero() = State(fun s -> (), s)
    member this.Return x = State(fun s -> x, s)
    member inline this.ReturnFrom(x: State<'s, 'a>) = x

    member this.Bind(x, f): State<'s, 'b> =
        State(fun state ->
            let (result: 'a), state = State.run state x
            State.run state (f result))

    member this.Combine(x1: State<'s, 'a>, x2: State<'s, 'b>) = State.combine x1 x2

    member this.Delay f: State<'s, 'a> = f()

    member this.For(seq, (f: 'a -> State<'s, 'b>)) =
        seq
        |> Seq.map f
        |> Seq.reduceBack (fun x1 x2 -> this.Combine(x1, x2))

    member this.While(f, x) =
        if f() then this.Combine(x, this.While(f, x)) else this.Zero()

let state = StateBuilder()
