// ~~~~~~~~~` Captivating `~~~~~~~~~~
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

/// Storage locations for variables.
type Storage =
    /// Locals are indices into some arbitrary array of storage. Locals begin their
    /// indexing again at 0 when a new lambda is defined
    | Local of int
    /// Load from the function's argument
    | Arg
    /// A captured local or argument stored in the captures environment at some
    /// given index
    | Environment of Storage
    /// Value is stored in the parent environment, the storage location could be
    /// another `Capture`, or an `Environment` storage location
    | Capture of Storage

/// Bound node, with captured variables resolved
type Bound =
    | Number of int
    | Load of Storage
    | Store of Storage * Bound
    | Seq of Bound list
    | Lambda of Storage list * Bound

// ================== Bind Pass ================== 

type Variable =
    { Name: string
    ; mutable Storage: Storage }

/// Binder context, flowed through the bind to keep track of the current state
type BindCtx =
    { Parent: BindCtx option
    ; mutable NextLocal: int
    ; mutable Captures: Storage list
    ; mutable Locals: Variable list }

    /// Lookup the given `id` in the current binder context.
    member ctx.Lookup id =
        match ctx.LookupVar id with
        // If this was in our environment, then return the variable's current
        // storage location
        | Some(v) -> Some(v.Storage)
        | None -> ctx.ParentLookup id

    /// Lookup the given `id` in the current context, and move it to captured
    /// storage if it hasn't already been moved there.
    member ctx.LookupAndCapture id =
        match ctx.LookupVar id with
        | Some(v) -> 
            match v.Storage with
            | Environment(_) -> Some(v.Storage)
            | s -> 
                v.Storage <- Storage.Environment(s)
                Some(v.Storage)
        | None -> ctx.ParentLookup id

    /// Look up the id in the parent context, and return an `Capture` to it.
    member ctx.ParentLookup id =
        match ctx.Parent with
        | Some(parent) -> 
            match parent.LookupAndCapture id with
            | Some(storage) ->
                ctx.Captures <- storage::ctx.Captures
                Some(Storage.Capture(storage))
            | None -> None
        | None -> None

    /// Introduce a new local definition.
    member ctx.Define id =
        let storage = Storage.Local(ctx.NextLocal)
        ctx.Locals <- { Name = id; Storage = storage }::ctx.Locals
        ctx.NextLocal <- ctx.NextLocal + 1
        storage

    /// Define a lambda argument
    member ctx.DefineArg id =
        ctx.Locals <- { Name = id
                      ; Storage = Storage.Arg }::ctx.Locals

    /// Lookup the raw varirable for for the given ID
    member private ctx.LookupVar id =
        List.tryFind (fun l -> l.Name = id) ctx.Locals

    /// Create the empty root bind context
    static member Root =
        { Parent = None
        ; NextLocal = 0
        ; Captures = []
        ; Locals = [] }

    /// Create a derived contex for binding lambdas
    static member WithParent parent =
        { Parent = Some(parent)
        ; NextLocal = 0
        ; Captures = []
        ; Locals = [] }

/// Bind the syntactic tree and produce a bound tree
let rec private bind (ctx: BindCtx) = function
    | SynNode.Number n -> Bound.Number n
    | SynNode.Define(id, init) ->
        let local = ctx.Define id
        Bound.Store(local, bind ctx init)
    | SynNode.Load id ->
        match ctx.Lookup id with
        | Some local -> Bound.Load local
        | None -> failwithf "Reference to undefined %s" id
    | SynNode.Store(id, expr) ->
        match ctx.Lookup id with
        | Some local -> Bound.Store(local, bind ctx expr)
        | None -> failwithf "Attempt to store into undefined local %s" id
    | SynNode.Seq s -> List.map (bind ctx) s |> Bound.Seq
    | SynNode.Lambda(formal, body) ->
        let lambdaCtx = BindCtx.WithParent(ctx)
        lambdaCtx.DefineArg formal
        let body = bind lambdaCtx body 
        Bound.Lambda(lambdaCtx.Captures, body)

/// Test the binder on a given syntactic tree.
let private testBind tree =
    printfn "raw: %A" tree
    bind BindCtx.Root tree |> printfn "Bound: %A"

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
    SynNode.Lambda("x", SynNode.Load "x") |> testBind
    SynNode.Lambda("x", SynNode.Seq [
            SynNode.Define("counter", SynNode.Number 1) // << store to local
            SynNode.Lambda("y", SynNode.Store("counter", SynNode.Number 2)) // << capture
            SynNode.Load "counter" // load from captured value
        ]) |> testBind
    SynNode.Seq [ SynNode.Define("a", SynNode.Number 1337)
                ; SynNode.Lambda("x", SynNode.Store("a", SynNode.Number 102))
                ; SynNode.Lambda("y", SynNode.Seq [
                    SynNode.Define("b", SynNode.Lambda("z", SynNode.Store("a", SynNode.Load("z"))))
                    SynNode.Load("b")
                ])
        ] |> testBind

    0 // return an integer exit code
