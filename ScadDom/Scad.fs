module ScadDom.Scad

open System.Collections.Generic
open System.IO
open ScadDom.Common
open ScadDom.Dom

[<AutoOpen>]
module internal RenderingUtils =
    let internal indentSize = 4
    
    let internal invariantCulture = System.Globalization.CultureInfo.InvariantCulture
    
    let internal f_str (f: float) : string =
        f.ToString(invariantCulture)
        
    let internal i_str (x: int) = x.ToString(invariantCulture)
    
    let internal b_str (b: bool) = if b then "true" else "false"
    
    let internal p2_str (p: Point2d) = $"[{f_str p.x}, {f_str p.y}]"

    let internal p3_str (p: Point3d) =
        $"[{f_str p.x}, {f_str p.y}, {f_str p.z}]"

[<AutoOpen>]
module internal OutputHelpers =
    type internal FnArg =
        | RawFnArg of string
        | NamedFnArg of name: string * valueStr: string

    type internal OutputTree =
        | FnCall of name: string * args: FnArg list * children: OutputTree list
        | Modified of modifier: string * child: OutputTree

let private (|MatchName|) (name: string) x = name

let rec internal domToOutputTree (dom: ScadDomNode) : OutputTree =
    match dom with
    | Difference children & MatchName "difference" name
    | Union children & MatchName "union" name
    | Intersection children & MatchName "intersection" name ->
        FnCall(name, [], children |> List.map domToOutputTree)
    | Circle radius ->
        FnCall("circle", [ RawFnArg (f_str radius) ], [])
    | Square(width, height, center) ->
        let args =
            [ RawFnArg <| strWrap "[]" $"{f_str width}, {f_str height}"
              NamedFnArg("center", b_str center) ]

        FnCall("square", args, [])
    | Polygon(points, paths, convexity) ->
        let pointsArg = RawFnArg <| strWrap "[]" (points |> List.map p2_str |> String.concat ", ")
        let pathArgsOpt =
            paths
            |> Option.map (fun paths ->
                let pathsArg =
                    paths
                    |> List.map (fun route -> strWrap "[]" (route |> List.map i_str |> String.concat ", "))
                    |> String.concat ", "

                NamedFnArg("paths", pathsArg))

        let convexityArgOpt =
            convexity |> Option.map (fun c -> NamedFnArg("convexity", i_str c))

        let allArgs =
            [ pointsArg ]
            @ (pathArgsOpt |> Option.toList)
            @ (convexityArgOpt |> Option.toList)

        FnCall("polygon", allArgs, [])
    // Case: Text
    // | Text(content, size, font, halign, valign, spacing, direction, language, script) ->
    //     let textArg = RawFnArg (escapedUserStringLiteral content)
    //     let allArgs =
    //         [ textArg ] @
    //         (size      |> Option.map (fun s -> NamedFnArg("size", f_str s))           |> Option.toList) @
    //         (font      |> Option.map (fun f -> NamedFnArg("font", escapedUserStringLiteral f)) |> Option.toList) @
    //         (halign    |> Option.map (fun h -> NamedFnArg("halign", escapedUserStringLiteral h))  |> Option.toList) @
    //         (valign    |> Option.map (fun v -> NamedFnArg("valign", escapedUserStringLiteral v))  |> Option.toList) @
    //         (spacing   |> Option.map (fun s -> NamedFnArg("spacing", f_str s))         |> Option.toList) @
    //         (direction |> Option.map (fun d -> NamedFnArg("direction", escapedUserStringLiteral d))|> Option.toList) @
    //         (language  |> Option.map (fun l -> NamedFnArg("language", escapedUserStringLiteral l)) |> Option.toList) @
    //         (script    |> Option.map (fun s -> NamedFnArg("script", escapedUserStringLiteral s))  |> Option.toList)
    //     renderFnCall "text" allArgs []

    // Case: Import
    | Import(filePath, convexity, layer) ->
        let allArgs =
            [ RawFnArg(escapedUserStringLiteral filePath) ]
            @ (convexity
               |> Option.map (fun c -> NamedFnArg("convexity", f_str c))
               |> Option.toList)
            @ (layer
               |> Option.map (fun l -> NamedFnArg("layer", escapedUserStringLiteral l))
               |> Option.toList) // Assuming layer is Option<string>

        FnCall("import", allArgs, [])
    | Projection(child, cut) -> FnCall("projection", [ NamedFnArg("cut", b_str cut) ], [ domToOutputTree child ])
    | Sphere radius -> FnCall("sphere", [ RawFnArg(f_str radius) ], [])
    | Cube(width = width; depth = cdepth; height = height; center = center) when width = cdepth && cdepth = height ->
        FnCall("cube", [ RawFnArg(f_str width); NamedFnArg("center", b_str center) ], [])
    | Cube(width = width; depth = depth; height = height; center = center) ->
        FnCall(
            "cube",
            [ RawFnArg $"[{f_str width}, {f_str depth}, {f_str height}]"
              NamedFnArg("center", b_str center) ],
            []
        )
    | Cylinder(height, radius1, radius2, center) ->
        let args =
            [ RawFnArg(f_str height)
              RawFnArg(f_str radius1)
              RawFnArg(f_str radius2)
              NamedFnArg("center", b_str center) ]

        FnCall("cylinder", args, [])
    // | Polyhedron(points, faces, convexity) ->
    //     let pointsArg = RawFnArg $"[{points |> List.map p3_str |> String.concat ", "}]"
    //     let facesArg = RawFnArg $"[{faces |> List.map (fun facePath -> $"[{facePath |> List.map i_str |> String.concat ", "}]") |> String.concat ", "}]"
    //     let convexityArgOpt = convexity |> Option.map (fun c -> NamedFnArg("convexity", i_str c))
    //     let allArgs = [ pointsArg; facesArg ] @ (convexityArgOpt |> Option.toList)
    //     renderFnCall "polyhedron" allArgs []
    | LinearExtrude(child, height, v, center, convexity, twist, slices, scale) ->
        let allArgs =
            [ NamedFnArg("height", f_str height) ]
            @ (v |> Option.map (fun vec -> NamedFnArg("v", p3_str vec)) |> Option.toList)
            @ (center |> Option.map (fun c -> NamedFnArg("center", b_str c)) |> Option.toList)
            @ (convexity
               |> Option.map (fun c -> NamedFnArg("convexity", f_str c))
               |> Option.toList)
            @ (twist |> Option.map (fun t -> NamedFnArg("twist", i_str t)) |> Option.toList)
            @ (slices |> Option.map (fun s -> NamedFnArg("slices", i_str s)) |> Option.toList)
            @ (scale |> Option.map (fun sc -> NamedFnArg("scale", f_str sc)) |> Option.toList)

        FnCall("linear_extrude", allArgs, [ domToOutputTree child ])
    | RotateExtrude(child, convexity, angle, start) ->
        let allArgs =
            (convexity
             |> Option.map (fun c -> NamedFnArg("convexity", f_str c))
             |> Option.toList)
            @ (angle |> Option.map (fun a -> NamedFnArg("angle", f_str a)) |> Option.toList)
            @ (start |> Option.map (fun s -> NamedFnArg("start", f_str s)) |> Option.toList)

        FnCall("rotate_extrude", allArgs, [ domToOutputTree child ])
    // | Surface(filePath, center, convexity, invert) ->
    //     let fileArg = RawFnArg (escapedUserStringLiteral filePath)
    //     let centerArgOpt = center |> Option.map (fun c -> NamedFnArg("center", b_str c))
    //     let convexityArgOpt = convexity |> Option.map (fun c -> NamedFnArg("convexity", i_str c))
    //     let invertArgOpt = invert |> Option.map (fun i -> NamedFnArg("invert", b_str i))
    //     let allArgs = [ fileArg ] @ (centerArgOpt |> Option.toList) @ (convexityArgOpt |> Option.toList) @ (invertArgOpt |> Option.toList)
    //     renderFnCall "surface" allArgs []
    | Translate(child, vector) ->
        FnCall("translate", [ NamedFnArg("v", p3_str vector) ], [ domToOutputTree child ])
    // | Rotate(child, rotationAnglesXyz, angle, axisVector) ->
    //     let args =
    //         match rotationAnglesXyz, angle, axisVector with
    //         | Some angles, _, _ -> [ RawFnArg (p3_str angles) ] // Euler angles vector
    //         | None, Some ang, Some axis -> [ RawFnArg (f_str ang); RawFnArg (p3_str axis) ] // Angle and axis vector
    //         | None, Some ang, None -> [ RawFnArg (p3_str { X = 0.0; Y = 0.0; Z = ang }) ] // Angle only (implicit Z axis)
    //         | _ -> [] // Or handle as an error/comment, e.g., [ NamedFnArg("error", RawFnArg """ "Invalid Rotate args" """)]
    //     renderFnCall "rotate" args [ child ]
    | Scale(child, scaleVector) ->
        FnCall("scale", [ NamedFnArg("v", p3_str scaleVector) ], [ domToOutputTree child ])
    // Case: Resize
    // | Resize(child, newDimensions, autoX, autoY, autoZ, convexity) ->
    //     let dimsArg = RawFnArg (p3_str newDimensions)
    //     let autoArgOpt =
    //         match autoX, autoY, autoZ with
    //         | Some ax, Some ay, Some az when ax = ay && ay = az -> Some(NamedFnArg("auto", b_str ax)) // All same, single bool
    //         | None, None, None -> None // No auto specified
    //         | _ -> // Mixed or some Nones, use vector form, defaulting Nones to false
    //             let x = autoX.GetValueOrDefault(false)
    //             let y = autoY.GetValueOrDefault(false)
    //             let z = autoZ.GetValueOrDefault(false)
    //             Some(NamedFnArg("auto", $"[{b_str x},{b_str y},{b_str z}]"))
    //     let convexityArgOpt = convexity |> Option.map (fun c -> NamedFnArg("convexity", i_str c))
    //     let allArgs = [ dimsArg ] @ (autoArgOpt |> Option.toList) @ (convexityArgOpt |> Option.toList)
    //     renderFnCall "resize" allArgs [ child ]
    | Mirror(child, mirrorVector) ->
        FnCall("mirror", [ NamedFnArg("v", p3_str mirrorVector) ], [ domToOutputTree child ])
    | MultMatrix(child, matrix) ->
        let matrixStr =
            matrix
            |> List.map (fun (f1, f2, f3, f4) -> $"[{f_str f1}, {f_str f2}, {f_str f3}, {f_str f4}]")
            |> String.concat ","

        let matrixArgValue = $"[{matrixStr}]"
        FnCall("multmatrix", [ NamedFnArg("m", matrixArgValue) ], [ domToOutputTree child ])
    | Color(scadColor, child) ->
        let colorArgs =
            match scadColor with
            | NamedScadColor name -> [ RawFnArg(escapedUserStringLiteral name) ]
            | ScadColor(r, g, b, alpha) ->
                [ NamedFnArg("c", $"[{f_str r}, {f_str g}, {f_str b}]")
                  NamedFnArg("alpha", f_str alpha) ]

        FnCall("color", colorArgs, [ domToOutputTree child ])
    | OffsetRadius(child, r) -> FnCall("offset", [ NamedFnArg("r", f_str r) ], [ domToOutputTree child ])
    | OffsetDelta(child, delta, chamfer) ->
        FnCall(
            "offset",
            [ NamedFnArg("delta", f_str delta); NamedFnArg("chamfer", b_str chamfer) ],
            [ domToOutputTree child ]
        )
    | Hull children -> FnCall("hull", [], children |> List.map domToOutputTree)
    | Minkowski(convexity, children) ->
        let args = convexity |> Option.map (fun c -> RawFnArg(i_str c)) |> Option.toList
        FnCall("minkowski", args, children |> List.map domToOutputTree)
    | DisableModifier child & MatchName "*" modifier
    | RootModifier child & MatchName "!" modifier
    | DebugModifier child & MatchName "#" modifier
    | BackgroundModifier child & MatchName "%" modifier ->
        Modified(modifier, domToOutputTree child)
    
let private writeIndent (tw: TextWriter) (depth: int) =
    for i = 0 to depth * indentSize do
        tw.Write ' '

let private renderArgs (tw: TextWriter) (args: FnArg list) =
    let mutable first = true

    for arg in args do
        if not first then
            tw.Write ", "

        match arg with
        | RawFnArg s ->
            tw.Write s
        | NamedFnArg(name, valueStr) ->
            tw.Write name; tw.Write " = "; tw.Write valueStr
        first <- false

let rec private renderOutput'
    (tw: TextWriter)
    (output: OutputTree)
    (depth: int)
    (pendingModifiers : Queue<string>) =
    match output with
    | FnCall(name, [], []) ->
        writeIndent tw depth

        while pendingModifiers.Count > 0 do
            pendingModifiers.Dequeue() |> tw.Write

        tw.Write name
        tw.Write "();\n"
    | FnCall(name, args, children) ->
        writeIndent tw depth

        while pendingModifiers.Count > 0 do
            pendingModifiers.Dequeue() |> tw.Write

        tw.Write name
        tw.Write "("
        renderArgs tw args
        tw.Write ")"

        match children with
        | [] -> tw.Write ";\n"
        | _ ->
            tw.Write " {\n"

            for c in children do
                renderOutput' tw c (depth + 1) pendingModifiers

            writeIndent tw depth
            tw.Write "}\n"
    | Modified(modifier, child) ->
        pendingModifiers.Enqueue modifier
        renderOutput' tw child depth pendingModifiers

let private renderOutput (tw: TextWriter) (output: OutputTree) =
    let pendingModifiers = Queue<string>()
    renderOutput' tw output 0 pendingModifiers

let renderToString (dom: ScadDomNode) : string =
    use sw = new StringWriter()
    let outputTree = domToOutputTree dom
    renderOutput sw outputTree
    sw.ToString()

let renderToStreamWriter (dom: ScadDomNode) (sw: StreamWriter) (leaveOpen: bool) : unit =
    try
        let outputTree = domToOutputTree dom
        renderOutput sw outputTree
    finally
        if not leaveOpen then
            sw.Dispose()

let renderToPath (dom: ScadDomNode) (path: string) : unit =
    use fs = File.Open(path, FileMode.Create)
    use sw = new StreamWriter(fs)
    let outputTree = domToOutputTree dom
    renderOutput sw outputTree
