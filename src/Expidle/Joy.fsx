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
    // TODO:
    // Trace: TraceEntry list
}

and JoyDefinition =
    | Builtin of Builtin
    | Defined of JoyValue list
    
and Builtin = Runtime -> Runtime

    