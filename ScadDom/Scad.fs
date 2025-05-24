module ScadDom.Scad

open System.Collections.Generic
open System.IO
open ScadDom.Common
open ScadDom.Dom

[<AutoOpen>]
module internal RenderingUtils =
    let internal indentSize = 4

    let internal scadNumberString (f: float) : string =
        f.ToString(System.Globalization.CultureInfo.InvariantCulture)

    let internal invariantCulture = System.Globalization.CultureInfo.InvariantCulture

    let internal f_str = scadNumberString
    let internal i_str (x: int) = x.ToString(invariantCulture)
    let internal b_str (b: bool) = if b then "true" else "false"
    let internal p2_str (p: Point2d) = $"[{f_str p.x}, {f_str p.y}]"

    let internal p3_str (p: Point3d) =
        $"[{f_str p.x}, {f_str p.y}, {f_str p.z}]"

type private RenderingContext(textWriter: TextWriter) =
    member val indentLevel = 0 with get, set
    member val pendingPfx = Queue<char>() with get

    member this.IncIndent(n: int) =
        this.indentLevel <- this.indentLevel + n

    member this.IndentScope() =
        this.IncIndent 1
        defer (fun () -> this.IncIndent -1)

    member this.Indent() =
        for i = 0 to this.indentLevel - 1 do
            textWriter.Write ' '

    member this.WriteAtIndent(s: string) =
        this.Indent()

        while this.pendingPfx.Count > 0 do
            textWriter.Write(this.pendingPfx.Dequeue())

        textWriter.Write s

    member this.WriteLineAtIndent(s: string) =
        this.Indent()

        while this.pendingPfx.Count > 0 do
            textWriter.Write(this.pendingPfx.Dequeue())

        textWriter.Write s

let rec private render (ctx: RenderingContext) (dom: ScadDomNode) : unit =
    match dom with
    | Difference children -> renderFnCall ctx "difference" [] children
    | Union children -> renderFnCall ctx "union" [] children
    | Intersection children -> renderFnCall ctx "intersection" [] children
    | Circle radius -> ctx.WriteLineAtIndent $"circle({f_str radius});"
    | Square(width, height, center) -> ctx.WriteLineAtIndent $"square([{f_str width}, {f_str height}], {b_str center});"
    | Polygon(points, paths, convexity) -> failwith "todo"
    // | Text(content, size, font, horizontalAlign, verticalAlign, spacing, direction, language, script) ->
    //     failwith "todo"
    | Import(filePath, convexity, layer) ->
        let args =
            seq {
                yield [ escapedUserStringLiteral filePath ]

                if convexity.IsSome then
                    yield [ "convexity"; (f_str convexity.Value) ]
            }
            |> List.ofSeq

        renderFnCall ctx "import" args []
    | Projection(child, cut) -> renderFnCall ctx "projection" [ [ "cut"; b_str cut ] ] [ child ]
    | Sphere radius -> ctx.WriteLineAtIndent $"sphere({f_str radius});"
    | Cube(width = width; depth = cdepth; height = height; center = center) when width = cdepth && cdepth = height ->
        ctx.WriteLineAtIndent $"cube({f_str width}, center={b_str center});"
    | Cube(width = width; depth = depth; height = height; center = center) ->
        ctx.WriteLineAtIndent $"cube([{f_str width}, {f_str depth}, {f_str height}], center={b_str center});"
    | Cylinder(height, radius1, radius2, center) ->
        ctx.WriteLineAtIndent $"cylinder({f_str height}, {f_str radius1}, {f_str radius2}, center={b_str center});"
    // | Polyhedron(points, faces, convexity) -> failwith "todo"
    | LinearExtrude(child, height, v, center, convexity, twist, slices, scale) ->
        let args =
            seq {
                yield [ "height"; f_str height ]

                if v.IsSome then
                    yield [ "v"; p3_str v.Value ]

                if center.IsSome then
                    yield [ "center"; b_str center.Value ]

                if convexity.IsSome then
                    yield [ "convexity"; f_str convexity.Value ]

                if twist.IsSome then
                    yield [ "twist"; i_str twist.Value ]

                if slices.IsSome then
                    yield [ "slices"; i_str slices.Value ]

                if scale.IsSome then
                    yield [ "scale"; f_str scale.Value ]
            }
            |> List.ofSeq

        renderFnCall ctx "linear_extrude" args [ child ]
    | RotateExtrude(child, convexity, angle, start) ->
        let args =
            seq {
                if convexity.IsSome then
                    yield [ "convexity"; f_str convexity.Value ]

                if angle.IsSome then
                    yield [ "angle"; f_str angle.Value ]

                if start.IsSome then
                    yield [ "start"; f_str start.Value ]
            }
            |> List.ofSeq

        renderFnCall ctx "linear_extrude" args [ child ]
    //| Surface(filePath, center, convexity, invert) -> failwith "todo"
    | Translate(child, vector) -> renderFnCall ctx "translate" [ [ "v"; p3_str vector ] ] [ child ]
    //| Rotate(child, rotationAnglesXyz, angle, axisVector) -> failwith "todo"
    | Scale(child, scaleVector) -> renderFnCall ctx "scale" [ [ "v"; (p3_str scaleVector) ] ] [ child ]
    //| Resize(child, newDimensions, autoX, autoY, autoZ, convexity) -> failwith "todo"
    | Mirror(child, mirrorVector) -> renderFnCall ctx "mirror" [ [ "v"; p3_str mirrorVector ] ] [ child ]
    | MultMatrix(child, matrix) ->
        let matrixStr =
            [ for (f1, f2, f3, f4) in matrix -> $"[{f_str f1}, {f_str f2}, {f_str f3}, {f_str f4}]" ]
            |> String.concat ","

        let matrixStr = $"[{matrixStr}]"
        renderFnCall ctx "multmatrix" [ [ "m"; matrixStr ] ] [ child ]
    | Color(scadColor, child) ->
        let colorArg =
            match scadColor with
            | NamedScadColor(name = name) -> [ escapedUserStringLiteral name ]
            | ScadColor(red = red; green = green; blue = blue; alpha = alpha) ->
                [ "c"; $"[{f_str red}, {f_str green}, {f_str blue}], alpha = {f_str alpha}" ]

        renderFnCall ctx "color" [ colorArg ] [ child ]
    | OffsetRadius(child, r) ->
        let args = [ [ "r"; f_str r ] ]
        renderFnCall ctx "offset" args [ child ]
    | OffsetDelta(child, delta, chamfer) ->
        let args = [ [ "delta"; f_str delta ]; [ "chamfer"; b_str chamfer ] ]
        renderFnCall ctx "offset" args [ child ]
    | Hull children -> renderFnCall ctx "hull" [] children
    | Minkowski(convexity, children) ->
        let arg = convexity |> Option.toList |> List.map i_str
        renderFnCall ctx "minkowski" [ arg ] children
    | AsteriskModifier child ->
        ctx.pendingPfx.Enqueue('*')
        render ctx child
    | ExclamationModifier child ->
        ctx.pendingPfx.Enqueue('!')
        render ctx child
    | HashModifier child ->
        ctx.pendingPfx.Enqueue '#'
        render ctx child
    | PercentModifier child ->
        ctx.pendingPfx.Enqueue '%'
        render ctx child

and private renderFnCall
    (ctx: RenderingContext)
    (fnName: string)
    (kvps: (string list) list)
    (children: ScadDomNode list)
    =
    let argStr = [ for kvp in kvps -> kvp |> String.concat " = " ] |> String.concat ", "

    match children with
    | [] -> ctx.WriteLineAtIndent $"{fnName}({argStr});"
    | _ ->
        ctx.WriteLineAtIndent $"{fnName}({argStr}) {{ "
        use _ = ctx.IndentScope()

        for c in children do
            render ctx c

        ctx.WriteLineAtIndent " }"

let renderToString (dom: ScadDomNode) : string =
    use sw = new StringWriter()
    let ctx = RenderingContext(sw)
    render ctx dom
    sw.ToString()

let renderToStreamWriter (dom: ScadDomNode) (streamWriter: StreamWriter) (leaveOpen: bool) : unit =
    let ctx = RenderingContext(streamWriter)

    try
        render ctx dom
    finally
        if not leaveOpen then
            streamWriter.Dispose()

let renderToPath (dom: ScadDomNode) (path: string) : unit =
    use fs = File.OpenWrite(path)
    use sw = new StreamWriter(fs)
    let ctx = RenderingContext(sw)
    render ctx dom
