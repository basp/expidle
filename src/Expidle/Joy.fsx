type JoyValue =
    | Bool of bool
    | Int of int
    | String of string
    | Symbol of string
    | Quotation of JoyValue list
       
type Runtime = {
    Queue: JoyValue list
    Stack: JoyValue list
    Env: Map<string, JoyDefinition>
    Trace: TraceEntry list
}

and TraceEntry = {
    Instruction: JoyValue
    StackBefore: JoyValue list
    StackAfter: JoyValue list
    QueueBefore: JoyValue list
    QueueAfter: JoyValue list
    Resolution: JoyDefinition option
}
    
and JoyDefinition =
    | Builtin of Builtin
    | Defined of JoyValue list
    
and Builtin = Runtime -> Runtime

module Builtins =
    let dup rt =
        let newStack =
            match rt.Stack with
            | x :: xs -> x :: x :: xs
            | _ -> failwith "dup: stack underflow"
        { rt with Stack = newStack }
    
    let swap rt =
        let newStack = 
            match rt.Stack with
            | x :: y :: xs -> y :: x :: xs
            | _ -> failwith "swap: stack underflow"
        { rt with Stack = newStack }
        
    let pop rt =
        let newStack = 
            match rt.Stack with
            | _ :: xs -> xs
            | _ -> failwith "pop: stack underflow"
        { rt with Stack = newStack }
            
    let i rt =
        match rt.Stack with
        | Quotation q :: xs ->
            { rt with
                Stack = xs;
                Queue = q @ rt.Queue }
        | _ -> failwith "i: expect quotation"

let step rt =
    match rt.Queue with
    | [] -> rt // nothing to do    
    | instr :: remainingQueue ->
        let stackBefore = rt.Stack
        let queueBefore = rt.Queue
        
        let baseRt = { rt with Queue = remainingQueue }
            
        let (rtAfter, resolution) =
            match instr with
            | Int _
            | Bool _
            | String _
            | Quotation _ ->
                { baseRt with Stack = instr :: stackBefore }, None
            | Symbol name ->
                match rt.Env.TryFind name with
                | None -> failwithf $"undefined symbol: %s{name}"
                | Some (Builtin bi) ->
                    let newRt = bi baseRt
                    (newRt, Some (Builtin bi))
                | Some (Defined def) ->
                    let newRt = { baseRt with Stack = def @ stackBefore }
                    (newRt, Some (Defined def))              
        
        let traceEntry = {
            Instruction = instr
            StackBefore = stackBefore
            StackAfter = rtAfter.Stack
            QueueBefore = queueBefore
            QueueAfter = rtAfter.Queue
            Resolution = resolution
        }

        // TODO: Decide on trace order (latest first or oldest first?)        
        { rtAfter with Trace = traceEntry :: rt.Trace }

let env = Map.ofList [
    "dup", Builtin Builtins.dup
    "swap", Builtin Builtins.swap
    "pop", Builtin Builtins.pop
    "i", Builtin Builtins.i
]

let p = [Int 1; Symbol "dup"; Int 2; Symbol "swap"]
    
let rt0 = {
    Queue = p
    Stack = []
    Env = env
    Trace = []
}


// Evaluate a few steps and print the trace. 
let rt1 = step rt0
let rt2 = step rt1
let rt3 = step rt2
let rt4 = step rt3
let rt5 = step rt4

printfn $"%A{rt5.Trace}"