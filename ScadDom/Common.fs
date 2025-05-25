module ScadDom.Common

open System
open System.Text

[<Struct>]
type internal DeferScope =
    { f: unit -> unit }

    interface IDisposable with
        member this.Dispose() = this.f ()

let internal defer (f: unit -> unit) = { DeferScope.f = f }

let escapeUserString (s: string) : string =
    s
        .Replace(@"\", @"\\")
        .Replace(@"""", @"\""")
        .Replace("\n", @"\\n")
        .Replace("\r", @"\\r")
        .Replace("\t", @"\\t")

let escapedUserStringLiteral (s: string) : string = $"\"{escapeUserString s}\""

let internal strWrap (wrapStr: string) (inner: string) =
    if wrapStr.Length = 0 then
        invalidArg "wrapStr" "Can't be empty"

    if wrapStr.Length % 2 <> 0 then
        invalidArg "wrapStr" "Must have an even length"

    let len = wrapStr.Length + inner.Length
    let sb = StringBuilder(len)
    let wrapMid = wrapStr.Length / 2

    for i = 0 to wrapMid - 1 do
        sb.Append wrapStr.[i] |> ignore

    for i = 0 to inner.Length - 1 do
        sb.Append inner.[i] |> ignore

    for i = wrapMid to wrapStr.Length - 1 do
        sb.Append wrapStr.[i] |> ignore

    sb.ToString()
