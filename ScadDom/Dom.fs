module ScadDom.Dom

type ScadColor =
    | NamedScadColor of name: string
    | ScadColor of red: float * green: float * blue: float * alpha: float

    static member Lavender: ScadColor = NamedScadColor "Lavender"
    static member Thistle: ScadColor = NamedScadColor "Thistle"
    static member Plum: ScadColor = NamedScadColor "Plum"
    static member Violet: ScadColor = NamedScadColor "Violet"
    static member Orchid: ScadColor = NamedScadColor "Orchid"
    static member Fuchsia: ScadColor = NamedScadColor "Fuchsia"
    static member Magenta: ScadColor = NamedScadColor "Magenta"
    static member MediumOrchid: ScadColor = NamedScadColor "MediumOrchid"
    static member MediumPurple: ScadColor = NamedScadColor "MediumPurple"
    static member BlueViolet: ScadColor = NamedScadColor "BlueViolet"
    static member DarkViolet: ScadColor = NamedScadColor "DarkViolet"
    static member DarkOrchid: ScadColor = NamedScadColor "DarkOrchid"
    static member DarkMagenta: ScadColor = NamedScadColor "DarkMagenta"
    static member Purple: ScadColor = NamedScadColor "Purple"
    static member Indigo: ScadColor = NamedScadColor "Indigo"
    static member DarkSlateBlue: ScadColor = NamedScadColor "DarkSlateBlue"
    static member SlateBlue: ScadColor = NamedScadColor "SlateBlue"
    static member MediumSlateBlue: ScadColor = NamedScadColor "MediumSlateBlue"
    static member Reds: ScadColor = NamedScadColor "Reds"
    static member IndianRed: ScadColor = NamedScadColor "IndianRed"
    static member LightCoral: ScadColor = NamedScadColor "LightCoral"
    static member Salmon: ScadColor = NamedScadColor "Salmon"
    static member DarkSalmon: ScadColor = NamedScadColor "DarkSalmon"
    static member LightSalmon: ScadColor = NamedScadColor "LightSalmon"
    static member Red: ScadColor = NamedScadColor "Red"
    static member Crimson: ScadColor = NamedScadColor "Crimson"
    static member FireBrick: ScadColor = NamedScadColor "FireBrick"
    static member DarkRed: ScadColor = NamedScadColor "DarkRed"
    static member Blues: ScadColor = NamedScadColor "Blues"
    static member Aqua: ScadColor = NamedScadColor "Aqua"
    static member Cyan: ScadColor = NamedScadColor "Cyan"
    static member LightCyan: ScadColor = NamedScadColor "LightCyan"
    static member PaleTurquoise: ScadColor = NamedScadColor "PaleTurquoise"
    static member Aquamarine: ScadColor = NamedScadColor "Aquamarine"
    static member Turquoise: ScadColor = NamedScadColor "Turquoise"
    static member MediumTurquoise: ScadColor = NamedScadColor "MediumTurquoise"
    static member DarkTurquoise: ScadColor = NamedScadColor "DarkTurquoise"
    static member CadetBlue: ScadColor = NamedScadColor "CadetBlue"
    static member SteelBlue: ScadColor = NamedScadColor "SteelBlue"
    static member LightSteelBlue: ScadColor = NamedScadColor "LightSteelBlue"
    static member PowderBlue: ScadColor = NamedScadColor "PowderBlue"
    static member LightBlue: ScadColor = NamedScadColor "LightBlue"
    static member SkyBlue: ScadColor = NamedScadColor "SkyBlue"
    static member LightSkyBlue: ScadColor = NamedScadColor "LightSkyBlue"
    static member DeepSkyBlue: ScadColor = NamedScadColor "DeepSkyBlue"
    static member DodgerBlue: ScadColor = NamedScadColor "DodgerBlue"
    static member CornflowerBlue: ScadColor = NamedScadColor "CornflowerBlue"
    static member RoyalBlue: ScadColor = NamedScadColor "RoyalBlue"
    static member Blue: ScadColor = NamedScadColor "Blue"
    static member MediumBlue: ScadColor = NamedScadColor "MediumBlue"
    static member DarkBlue: ScadColor = NamedScadColor "DarkBlue"
    static member Navy: ScadColor = NamedScadColor "Navy"
    static member MidnightBlue: ScadColor = NamedScadColor "MidnightBlue"
    static member Pinks: ScadColor = NamedScadColor "Pinks"
    static member Pink: ScadColor = NamedScadColor "Pink"
    static member LightPink: ScadColor = NamedScadColor "LightPink"
    static member HotPink: ScadColor = NamedScadColor "HotPink"
    static member DeepPink: ScadColor = NamedScadColor "DeepPink"
    static member MediumVioletRed: ScadColor = NamedScadColor "MediumVioletRed"
    static member PaleVioletRed: ScadColor = NamedScadColor "PaleVioletRed"
    static member Greens: ScadColor = NamedScadColor "Greens"
    static member GreenYellow: ScadColor = NamedScadColor "GreenYellow"
    static member Chartreuse: ScadColor = NamedScadColor "Chartreuse"
    static member LawnGreen: ScadColor = NamedScadColor "LawnGreen"
    static member Lime: ScadColor = NamedScadColor "Lime"
    static member LimeGreen: ScadColor = NamedScadColor "LimeGreen"
    static member PaleGreen: ScadColor = NamedScadColor "PaleGreen"
    static member LightGreen: ScadColor = NamedScadColor "LightGreen"
    static member MediumSpringGreen: ScadColor = NamedScadColor "MediumSpringGreen"
    static member SpringGreen: ScadColor = NamedScadColor "SpringGreen"
    static member MediumSeaGreen: ScadColor = NamedScadColor "MediumSeaGreen"
    static member SeaGreen: ScadColor = NamedScadColor "SeaGreen"
    static member ForestGreen: ScadColor = NamedScadColor "ForestGreen"
    static member Green: ScadColor = NamedScadColor "Green"
    static member DarkGreen: ScadColor = NamedScadColor "DarkGreen"
    static member YellowGreen: ScadColor = NamedScadColor "YellowGreen"
    static member OliveDrab: ScadColor = NamedScadColor "OliveDrab"
    static member Olive: ScadColor = NamedScadColor "Olive"
    static member DarkOliveGreen: ScadColor = NamedScadColor "DarkOliveGreen"
    static member MediumAquamarine: ScadColor = NamedScadColor "MediumAquamarine"
    static member DarkSeaGreen: ScadColor = NamedScadColor "DarkSeaGreen"
    static member LightSeaGreen: ScadColor = NamedScadColor "LightSeaGreen"
    static member DarkCyan: ScadColor = NamedScadColor "DarkCyan"
    static member Teal: ScadColor = NamedScadColor "Teal"
    static member Oranges: ScadColor = NamedScadColor "Oranges"
    static member Coral: ScadColor = NamedScadColor "Coral"
    static member Tomato: ScadColor = NamedScadColor "Tomato"
    static member OrangeRed: ScadColor = NamedScadColor "OrangeRed"
    static member DarkOrange: ScadColor = NamedScadColor "DarkOrange"
    static member Orange: ScadColor = NamedScadColor "Orange"
    static member Yellows: ScadColor = NamedScadColor "Yellows"
    static member Gold: ScadColor = NamedScadColor "Gold"
    static member Yellow: ScadColor = NamedScadColor "Yellow"
    static member LightYellow: ScadColor = NamedScadColor "LightYellow"
    static member LemonChiffon: ScadColor = NamedScadColor "LemonChiffon"
    static member LightGoldenrodYellow: ScadColor = NamedScadColor "LightGoldenrodYellow"
    static member PapayaWhip: ScadColor = NamedScadColor "PapayaWhip"
    static member Moccasin: ScadColor = NamedScadColor "Moccasin"
    static member PeachPuff: ScadColor = NamedScadColor "PeachPuff"
    static member PaleGoldenrod: ScadColor = NamedScadColor "PaleGoldenrod"
    static member Khaki: ScadColor = NamedScadColor "Khaki"
    static member DarkKhaki: ScadColor = NamedScadColor "DarkKhaki"
    static member Browns: ScadColor = NamedScadColor "Browns"
    static member Cornsilk: ScadColor = NamedScadColor "Cornsilk"
    static member BlanchedAlmond: ScadColor = NamedScadColor "BlanchedAlmond"
    static member Bisque: ScadColor = NamedScadColor "Bisque"
    static member NavajoWhite: ScadColor = NamedScadColor "NavajoWhite"
    static member Wheat: ScadColor = NamedScadColor "Wheat"
    static member BurlyWood: ScadColor = NamedScadColor "BurlyWood"
    static member Tan: ScadColor = NamedScadColor "Tan"
    static member RosyBrown: ScadColor = NamedScadColor "RosyBrown"
    static member SandyBrown: ScadColor = NamedScadColor "SandyBrown"
    static member Goldenrod: ScadColor = NamedScadColor "Goldenrod"
    static member DarkGoldenrod: ScadColor = NamedScadColor "DarkGoldenrod"
    static member Peru: ScadColor = NamedScadColor "Peru"
    static member Chocolate: ScadColor = NamedScadColor "Chocolate"
    static member SaddleBrown: ScadColor = NamedScadColor "SaddleBrown"
    static member Sienna: ScadColor = NamedScadColor "Sienna"
    static member Brown: ScadColor = NamedScadColor "Brown"
    static member Maroon: ScadColor = NamedScadColor "Maroon"
    static member Whites: ScadColor = NamedScadColor "Whites"
    static member White: ScadColor = NamedScadColor "White"
    static member Snow: ScadColor = NamedScadColor "Snow"
    static member Honeydew: ScadColor = NamedScadColor "Honeydew"
    static member MintCream: ScadColor = NamedScadColor "MintCream"
    static member Azure: ScadColor = NamedScadColor "Azure"
    static member AliceBlue: ScadColor = NamedScadColor "AliceBlue"
    static member GhostWhite: ScadColor = NamedScadColor "GhostWhite"
    static member WhiteSmoke: ScadColor = NamedScadColor "WhiteSmoke"
    static member Seashell: ScadColor = NamedScadColor "Seashell"
    static member Beige: ScadColor = NamedScadColor "Beige"
    static member OldLace: ScadColor = NamedScadColor "OldLace"
    static member FloralWhite: ScadColor = NamedScadColor "FloralWhite"
    static member Ivory: ScadColor = NamedScadColor "Ivory"
    static member AntiqueWhite: ScadColor = NamedScadColor "AntiqueWhite"
    static member Linen: ScadColor = NamedScadColor "Linen"
    static member LavenderBlush: ScadColor = NamedScadColor "LavenderBlush"
    static member MistyRose: ScadColor = NamedScadColor "MistyRose"
    static member Grays: ScadColor = NamedScadColor "Grays"
    static member Gainsboro: ScadColor = NamedScadColor "Gainsboro"
    static member LightGrey: ScadColor = NamedScadColor "LightGrey"
    static member Silver: ScadColor = NamedScadColor "Silver"
    static member DarkGray: ScadColor = NamedScadColor "DarkGray"
    static member Gray: ScadColor = NamedScadColor "Gray"
    static member DimGray: ScadColor = NamedScadColor "DimGray"
    static member LightSlateGray: ScadColor = NamedScadColor "LightSlateGray"
    static member SlateGray: ScadColor = NamedScadColor "SlateGray"
    static member DarkSlateGray: ScadColor = NamedScadColor "DarkSlateGray"
    static member Black: ScadColor = NamedScadColor "Black"

[<CLIMutable>]
[<Struct>]
type Point2d = { x: float; y: float }

[<CLIMutable>]
[<Struct>]
type Point3d = { x: float; y: float; z: float }

type ScadDomNode =
    // Boolean Operations
    | Difference of children: ScadDomNode list
    | Union of children: ScadDomNode list
    | Intersection of children: ScadDomNode list

    // 2D Operations
    | Circle of radius: float
    | Square of width: float * height: float * center: bool
    | Polygon of points: Point2d list * paths: (int list) list option * convexity: int option
    // | Text of
    //     content: string
    //     * size: float option
    //     * font: string option
    //     * horizontalAlign: string option
    //     * verticalAlign: string option
    //     * spacing: float option
    //     * direction: string option
    //     * language: string option
    //     * script: string option
    | Import of filePath: string * convexity: int option * layer: string option
    | Projection of child: ScadDomNode * cut: bool

    // 3D Operations
    | Sphere of radius: float
    | Cube of width: float * depth: float * height: float * center: bool
    | Cylinder of height: float * radius1: float * radius2: float * center: bool
    // | Polyhedron of
    //     points: Point2d list
    //     * faces: (int list) list
    //     * convexity: int option
    | LinearExtrude of
        child: ScadDomNode *
        height: float *
        v: Point3d option *
        center: bool option *
        convexity: int option *
        twist: int option *
        slices: int option *
        scale: int option
    | RotateExtrude of child: ScadDomNode * convexity: int option * angle: float option * start: float option
    // | Surface of
    //     filePath: string
    //     * center: bool option
    //     * convexity: int option
    //     * invert: bool option

    // Transformations
    | Translate of child: ScadDomNode * vector: Point3d
    // | Rotate of
    //     child: ScadDomNode
    //     * rotationAnglesXYZ: Point2d option
    //     * angle: float option
    //     * axisVector: Point2d option
    | Scale of child: ScadDomNode * scaleVector: Point3d
    // | Resize of
    //     child: ScadDomNode
    //     * newDimensions: Point2d
    //     * autoX: bool
    //     * autoY: bool
    //     * autoZ: bool
    //     * convexity: int option
    | Mirror of child: ScadDomNode * mirrorVector: Point3d
    | MultMatrix of child: ScadDomNode * matrix: (float * float * float * float) list // Represents a 4x4 matrix
    | Color of ScadColor * child: ScadDomNode
    | OffsetRadius of child: ScadDomNode * r: float
    | OffsetDelta of child: ScadDomNode * delta: float * chamfer: bool
    | Hull of children: ScadDomNode list
    | Minkowski of convexity: int option * children: ScadDomNode list

    // Modifiers
    | DisableModifier of child: ScadDomNode
    | RootModifier of child: ScadDomNode
    | DebugModifier of child: ScadDomNode
    | BackgroundModifier of child: ScadDomNode
