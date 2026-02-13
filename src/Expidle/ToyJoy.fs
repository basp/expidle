module Expidle.ToyJoy

type JoyValue =
    | Bool of bool
    | Int of int
    | String of string
    | Symbol of string
    | Quotation of JoyValue list
    
    override x.ToString() =
        match x with
        | Bool b -> b.ToString()
        | Int i -> i.ToString()
        | String s -> s
        | Symbol name -> name
        | Quotation q ->
            (String.concat " " (List.map string q))
            |> sprintf "[%s]" 
       
type Runtime = {
    Queue: JoyValue list
    Stack: JoyValue list
    Env: Map<string, JoyDefinition>
    Trace: TraceEntry list }

and TraceEntry = {
    Instruction: JoyValue
    StackBefore: JoyValue list
    StackAfter: JoyValue list
    QueueBefore: JoyValue list
    QueueAfter: JoyValue list
    Resolution: JoyDefinition option }
    
and JoyDefinition =
    | Builtin of Builtin
    | Defined of JoyValue list
    
and Builtin = Runtime -> Result<Runtime, JoyError>

and JoyError =
    | StackUnderflow
    | TypeMismatch of expected: string * actual: JoyValue
    | UndefinedSymbol of string
    | InvalidQuotation of JoyValue
    
    override x.ToString() =
        match x with
        | StackUnderflow ->
            "Stack underflow"
        | TypeMismatch (expected, actual) ->
            sprintf "Type mismatch: expected %s, got %O" expected actual
        | UndefinedSymbol name ->
            sprintf "Undefined symbol: %s" name
        | InvalidQuotation q ->
            sprintf "Invalid quotation: %A" q

module RuntimeStateView =
    type RuntimeStateView = {
        Queue: JoyValue list
        Stack: JoyValue list
        TraceSummary: JoyValue list }

    let init (rt: Runtime) =
        let traceSummary =
            rt.Trace
            |> List.map (fun te -> te.Instruction)
        { Queue = rt.Queue
          Stack = rt.Stack
          TraceSummary = traceSummary }

module Builtins =
    let dup rt : Result<Runtime, JoyError> =
        match rt.Stack with
        | x :: xs ->
            Ok { rt with Stack = x :: x :: xs }
        | _ ->
            Error StackUnderflow
    
    let swap rt =
        match rt.Stack with
        | x :: y :: xs ->
            Ok { rt with Stack = y :: x :: xs }
        | _ ->
            Error StackUnderflow
        
    let pop rt =
        match rt.Stack with
        | _ :: xs ->
            Ok { rt with Stack = xs }
        | _ ->
            Error StackUnderflow
            
    let i rt =        
        match rt.Stack with
        | Quotation q :: xs ->
            Ok { rt with
                    Stack = xs
                    Queue = q @ rt.Queue }
        | x :: _ ->
            Error (InvalidQuotation x)
        | [] ->
            Error StackUnderflow

let step (rt : Runtime) : Result<Runtime, JoyError * TraceEntry> =
    match rt.Queue with
    | [] ->
        Ok rt
    | instr :: remainingQueue ->
        // Save a snapshot of the state before executing instruction.
        let stackBefore = rt.Stack
        let queueBefore = rt.Queue

        // Base runtime for the next step.        
        let baseRt = { rt with Queue = remainingQueue }

        // Execute instruction.
        let resultAfter =
            match instr with
            | Int _
            | Bool _
            | String _
            | Quotation _ ->
                // Pushing a literal never fails.
                Ok ({ baseRt with Stack = instr :: stackBefore }, None)
            | Symbol name ->
                // Symbol lookup.
                match rt.Env.TryFind name with
                | None ->
                    Error (UndefinedSymbol name)
                // Builtins are executed immediately.
                | Some (Builtin bi) ->
                    bi baseRt
                    |> Result.map (fun newRt ->
                        (newRt, Some (Builtin bi)))
                // User-defined words expand into the queue.
                | Some (Defined def) ->
                    let newRt =
                        { baseRt with Queue = def @ baseRt.Queue }
                    Ok (newRt, Some (Defined def))

        match resultAfter with
        | Error err ->
            // Even on error, we still produce a trace entry
            let traceEntry =
                {
                    Instruction = instr
                    StackBefore = stackBefore
                    StackAfter = rt.Stack      // unchanged
                    QueueBefore = queueBefore
                    QueueAfter = rt.Queue      // unchanged
                    Resolution = None
                }
            Error (err, traceEntry)
        | Ok (rtAfter, resolution) ->
            let traceEntry =
                {
                    Instruction = instr
                    StackBefore = stackBefore
                    StackAfter = rtAfter.Stack
                    QueueBefore = queueBefore
                    QueueAfter = rtAfter.Queue
                    Resolution = resolution
                }
            Ok { rtAfter with Trace = traceEntry :: rt.Trace }

let rec runUntilHalt (rt: Runtime) : Result<Runtime, JoyError * TraceEntry> = 
    match rt.Queue with
    | [] -> Ok rt
    | _ ->
        match step rt with
        | Ok rt -> runUntilHalt rt
        | err -> err        

let defaultEnv = Map.ofList [
    "dup", Builtin Builtins.dup
    "swap", Builtin Builtins.swap
    "pop", Builtin Builtins.pop
    "i", Builtin Builtins.i
]

let defaultRuntime =
    { Queue = []
      Stack = []
      Env = defaultEnv
      Trace = [] }
