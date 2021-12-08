module Aoc.Utils

open FSharp.Quotations
open FSharp.Quotations.Evaluator
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Core.LanguagePrimitives.IntrinsicFunctions
open System.Reflection
open System.Diagnostics


let inline getLines (s: string) : seq<string> = s.Split("\n")


let rec exprToString (expr: Expr) : string =
    match expr with
    | ValueWithName (o, t, s) -> sprintf "%s (value=%A)" s o
    | Value (o, _) -> o.ToString()
    | SpecificCall <@@ (=) @@> (_, _, exprs) ->
        exprs
        |> List.map exprToString
        |> String.concat " = "
    | SpecificCall <@@ GetArray @@> (_, _, exprs) ->
        let arr = exprs |> List.head |> exprToString

        let rest =
            exprs |> List.tail |> List.head |> exprToString

        sprintf "(%s).[%s]" arr rest
    | SpecificCall <@@ Seq.toList @@> (_, _, exprs) ->
        exprs
        |> List.map exprToString
        |> String.concat " "
        |> sprintf "[%s]"
    | SpecificCall <@@ seq @@> (_, _, exprs) ->
        exprs
        |> List.map exprToString
        |> String.concat " "
    | SpecificCall <@@ (>=) @@> (_, _, exprs) ->
        exprs
        |> List.map exprToString
        |> String.concat " >= "
    | SpecificCall <@@ (..) @@> (_, _, exprs) ->
        exprs
        |> List.map exprToString
        |> String.concat ".."
    | Call (o, method, exprs) ->

        let mname = method.Name.ToString()
        let args = exprs |> List.map exprToString

        match method.CallingConvention with
        | CallingConventions.Standard ->
            let sargs = args |> String.concat " "
            sprintf "%s %s" mname sargs
        | _ ->
            let self = args |> List.head
            let rest = args |> List.tail
            let sargs = rest |> String.concat ", "
            sprintf "%s.%s(%s)" self mname sargs


    | PropertyGet (parent, property, _) ->
        match parent with
        | Some e -> exprToString e + "." + property.Name
        | None -> property.Name
    | Coerce (e, _) -> exprToString e
    | any ->
        printfn "%A" any
        any.ToString()


let assertion (expr: Expr<bool>) (message: string) =
    let value = expr |> QuotationEvaluator.Evaluate


    let fail_message =
        if System.String.IsNullOrWhiteSpace message then
            "Assertion Error"
        else
            message

    if not value then
        let stack = new StackTrace(true)
        let frame = stack.GetFrame(1)
        printfn "%A" <| frame
        let s = exprToString (expr)
        failwithf "Expression %s evaluated to false\n%s" s fail_message

#if DEBUG
    // printfn "%A" expr
    expr |> exprToString |> printfn "%s"
#endif



let inline unimplemented () = assertion <@ false @> "unimplemented!"
let inline unreachable () = assertion <@ false @> "unreachable!"

let inline trace (v: 'a) =
    printfn "%A" v
    v

let isNumeric (a: string) = System.Int64.TryParse(a, ref 0L)

let inline curry f a b = f (a, b)
let inline uncurry f (a, b) = f a b

let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let thd3 (_, _, c) = c
