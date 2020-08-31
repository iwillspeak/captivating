// ~~~~~~~~` Captivating `~~~~~~~~~~
//
// This is an experiment in correctly binding the capture of local and closure
// variables in a lambda-calculus derived language.

// ================ Syntax Tree =================

/// Syntactic node, as if from source code.
type SynNode =
    | Number of int
    | Define of string * SynNode
    | Load of string
    | Store of string * SynNode
    | Lambda of string * SynNode
    | Seq of SynNode list

// =========== Semantically Bound Tree ===========

/// Locals are indices into some arbitrary array of storage. Locals begin their
/// indexing again at 0 when a new lambda is defined
type Local = Local of int

/// Bound node, with captured variables resolved
type Bound =
    | Number of int
    | Load of Local
    | Store of Local * Bound
    | Seq of Bound list

// ================== Bind Pass ================== 

/// Binder context, flowed through the bind to keep track of the current state
type BindCtx =
    { mutable Locals: string list }

    /// Lookup the given `id` in the current binder context.
    member ctx.Lookup id =
        List.tryFindIndex (fun l -> l = id) ctx.Locals
        |> Option.map (fun idx -> Local (ctx.Locals.Length - 1 - idx))

/// Bind the syntactic tree and produce a bound tree
let rec private bind ctx = function
    | SynNode.Number n -> Bound.Number n
    | SynNode.Define(id, init) ->
        let localIdx = ctx.Locals.Length
        ctx.Locals <- id::ctx.Locals
        Bound.Store(Local localIdx, bind ctx init)
    | SynNode.Load id ->
        match ctx.Lookup id with
        | Some local -> Bound.Load local
        | None -> failwithf "Reference to undefined %s" id
    | SynNode.Store(id, expr) ->
        match ctx.Lookup id with
        | Some local -> Bound.Store(local, bind ctx expr)
        | None -> failwithf "Attempt to store into undefined local %s" id
    | SynNode.Seq s -> List.map (bind ctx) s |> Bound.Seq
    | e -> failwithf "Error binding %A" e

/// Test the binder on a given syntactic tree.
let private testBind tree =
    printfn "raw: %A" tree
    let ctx = { Locals = [] }
    bind ctx tree |> printfn "Bound: %A"

[<EntryPoint>]
let main argv =

    SynNode.Number 123 |> testBind
    SynNode.Define("foo", (SynNode.Number 123)) |> testBind
    SynNode.Define("foo", (SynNode.Define("bar", SynNode.Number 456))) |> testBind
    SynNode.Seq [ SynNode.Define("foo", SynNode.Number 234)
                ; SynNode.Load("foo") ] |> testBind
    SynNode.Seq [ SynNode.Define("foo", SynNode.Number 123)
                ; SynNode.Define("bar", SynNode.Number 456)
                ; SynNode.Load("bar")
                ; SynNode.Define("baz", SynNode.Number 456)
                ; SynNode.Load("foo")
                ; SynNode.Load("baz") ] |> testBind
    SynNode.Seq [ SynNode.Define("hello", SynNode.Number 123)
                ; SynNode.Define("world", SynNode.Number 456)
                ; SynNode.Store("hello", SynNode.Load "world") ] |> testBind

    0 // return an integer exit code
